
/* ************************************************************************** *
 * Frame  (©2023 Lelanthran Manickum)                                         *
 *                                                                            *
 * This program comes with ABSOLUTELY NO WARRANTY. This is free software      *
 * and you are welcome to redistribute it under certain conditions;  see      *
 * the LICENSE file for details.                                              *
 * ****************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>
#include <time.h>
#include <inttypes.h>
#include <string.h>
#include <errno.h>

#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <dirent.h>

#include "frm.h"
#include "ds_str.h"
#include "ds_array.h"

struct frm_t {
   char *dbpath;
   char *olddir;
   char *lastmsg;
};

#define ERR(x,...)     do {\
   char *prefix = NULL;\
   char *msg = NULL;\
   char *full = NULL;\
   ds_str_printf (&prefix, "[%s:%i] ", __FILE__, __LINE__);\
   ds_str_printf (&msg, __VA_ARGS__);\
   full = ds_str_cat (prefix, msg, NULL);\
   if (!prefix || !msg || !full) {\
      fprintf (stderr, "[%s:%i] FATAL OOM, aborting\n", __FILE__, __LINE__);\
      free (prefix); free (msg); free (full);\
   } else {\
      fprintf (stderr, "%s\n", full);\
      free (x->lastmsg); x->lastmsg = full;\
      free (prefix); free (msg);\
   }\
} while (0)

#define REMOVEME FRM_ERROR

static const char *lockfile = "framedb.lock";



/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

static bool wrapper_isdir (const struct dirent *de)
{
#ifdef PLATFORM_Windows

   struct stat sb;
   if ((stat (de->d_name, &sb)) != 0) {
      FRM_ERROR ("Error: Failed to stat [%s]: %m\n", de->d_name);
      return false;
   }
   return S_ISDIR(sb.st_mode);

#else

   return de->d_type == DT_DIR;

#endif
}

static int wrapper_mkdir (const char *name)
{
   int result = -1;
#ifdef PLATFORM_Windows

   result = mkdir (name);

#else

   result = mkdir (name, 0777);

#endif

   if (result!=0) {
      FRM_ERROR ("Error: failed to create directory [%s]: %m\n", name);
      return -1;
   }
   return 0;
}


/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

static const char *uint64_string (char dst[47], uint64_t value)
{
   snprintf (dst, 46, "%" PRIu64, value);
   return dst;
}

static char *pushdir (const char *newdir)
{
   char *ret = getcwd (NULL, 0);
   if (!ret) {
      FRM_ERROR ("Failed to get the current working directory\n");
      return NULL;
   }
   if ((chdir (newdir))!=0) {
      FRM_ERROR ("Failed to switch to new dir [%s]\n", newdir);
      free (ret);
      return NULL;
   }

   return ret;
}

static void popdir (char **olddir)
{
   if (!olddir || !*olddir)
      return;

   if ((chdir (*olddir))!=0) {
      FRM_ERROR ("Failed to switch back to [%s]\n", *olddir);
      return;
   }

   free (*olddir);
   *olddir = NULL;
}

static char *get_path (frm_t *frm) {
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

   char *pwd = getcwd (NULL, 0);
   if (!pwd) {
      ERR (frm, "Error: failed to retrieve path: %m\n");
      return NULL;
   }

   // Fixup the path, if it is an absolute path.
   size_t nbytes = strlen (frm->dbpath);
   if (nbytes > strlen (pwd)) {
      ERR (frm, "Error: internal corruption detected\n");
      free (pwd);
      return NULL;
   }

   char *path = ds_str_dup (&pwd[nbytes + 1]);
   if (!path) {
      ERR (frm, "OOM error: allocating path\n");
   }

   free (pwd);
   return path;
}


static char *history_read (const char *dbpath, size_t count)
{
   char *pwd = pushdir (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to switch dir [%s]: %m\n", dbpath);
      return NULL;
   }

   char *history = frm_readfile ("history");
   if (!history) {
      // Ignoring empty history. History is allowed to be empty.
      history = ds_str_dup ("");
      if (!history) {
         FRM_ERROR ("OOM error allocating empty history\n");
         popdir (&pwd);
         return NULL;
      }
   }

   if (count == (size_t)-1) {
      popdir (&pwd);
      return history;
   }

   char *tmp = history;
   for (size_t i=0; i<count; i++) {
      tmp = strchr (tmp, '\n');
      if (!tmp)
         break;
      tmp++;
   }
   if (tmp)
      *tmp = 0;

   popdir (&pwd);
   return history;
}

static bool history_append (const char *dbpath, const char *path)
{
   if (!dbpath || !path) {
      FRM_ERROR ("Error: cannot append history with null paths\n");
      return false;
   }

   char *pwd = pushdir (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to push current working directory\n");
      return false;
   }

   char *history = history_read (dbpath, (size_t)-1);
   if (!history) {
      popdir (&pwd);
      return false;
   }

   if (!(frm_writefile ("history",
               path, "\n",
               history,
               NULL))) {
      FRM_ERROR ("Failed to write file [%s/history]: %m\n", dbpath);
      free (history);
      popdir (&pwd);
      return false;
   }

   free (history);
   popdir (&pwd);
   return true;
}

static char *history_find (const char *dbpath, const char *prefix)
{
   if (!dbpath || !prefix) {
      FRM_ERROR ("Error: cannot search history with null paths\n");
      return NULL;
   }

   char *pwd = pushdir (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to push current working directory\n");
      return NULL;
   }

   char *history = history_read (dbpath, (size_t)-1);
   if (!history) {
      popdir (&pwd);
      return NULL;
   }

   char *sptr = NULL, *tok = NULL;
   tok = strtok_r (history, "\n", &sptr);
   do {
      char *found = strstr (tok, prefix);
      if (found == tok) {
         // If we can switch to it, it exists and we return it,
         // otherwise we just keep on trying.
         char *olddir = pushdir (tok);
         if (olddir) {
            popdir (&olddir);
            popdir (&pwd);
            char *ret = ds_str_dup (tok);
            free (history);
            return ret;
         }
      }
   } while ((tok = strtok_r (NULL, "\n", &sptr)));

   free (history);
   popdir (&pwd);
   return NULL;
}

static bool index_add (const char *dbpath, const char *entry)
{
   if (!dbpath || !entry || !entry[0]) {
      FRM_ERROR ("Error: null parameters passed to index_add: [%s:%s]\n",
            dbpath, entry);
      return false;
   }

   char *olddir = pushdir (dbpath);
   if (!olddir) {
      FRM_ERROR ("Error: failed to switch directory [%s]: %m\n", dbpath);
      return false;
   }

   char *index = frm_readfile ("index");
   if (!index) {
      FRM_ERROR ("Error: failed to read index: %m\n");
      popdir (&olddir);
      return false;
   }

   if (!(frm_writefile("index", entry, "\n", index, NULL))) {
      FRM_ERROR ("Error: failed to write index: %m\n");
      popdir (&olddir);
      free (index);
      return false;
   }

   free (index);
   popdir (&olddir);
   return true;
}

static bool index_remove (const char *dbpath, const char *entry)
{
   bool error = true;
   char *olddir = NULL;
   FILE *infile = NULL, *outfile = NULL;
   char *line = NULL;
   static const size_t line_len = 1024 * 1024;
   char fname[] = "frame-tmpfile-XXXXXX";
   int fd = -1;
   size_t nitems = 0;

   if (!dbpath || !entry || !entry[0]) {
      FRM_ERROR ("Error: null parameters passed to index_add: [%s:%s]\n",
            dbpath, entry);
      goto cleanup;
   }

   olddir = pushdir (dbpath);
   if (!olddir) {
      FRM_ERROR ("Error: failed to switch directory [%s]: %m\n", dbpath);
      goto cleanup;
   }

   fd = mkstemp (fname);
   if (fd < 0) {
      FRM_ERROR ("Failed to create temporary file: %m\n");
      return NULL;
   }
   close (fd);

   if (!(infile = fopen ("index", "r"))) {
      FRM_ERROR ("Error: failed to open index for reading: %m\n");
      goto cleanup;
   }

   if (!(outfile = fopen (fname, "w"))) {
      FRM_ERROR ("Error: failed to open [%s] for writing: %m\n", fname);
      goto cleanup;
   }

   if (!(line = malloc (line_len))) {
      FRM_ERROR ("OOM error allocating line for index\n");
      goto cleanup;
   }

   while ((fgets (line, line_len - 1, infile))) {
      char *tmp = strchr (line, '\n');
      if (tmp)
         *tmp = 0;
      if ((strcmp (line, entry))==0) {
         nitems++;
         continue;
      }
      fprintf (outfile, "%s\n", line);
   }

   if (!nitems) {
      FRM_ERROR ("Warning: [%s] not found in index\n", entry);
   }

   fclose (infile); infile = NULL;
   fclose (outfile); outfile = NULL;

   if ((rename (fname, "index"))!=0) {
      FRM_ERROR ("Error: failed to update index from [%s]: %m\n", fname);
      goto cleanup;
   }

   error = false;

cleanup:
   if (fd >= 0) {
      close (fd);
   }

   remove (fname); // Don't care about the return value for this.

   if (infile)
      fclose (infile);

   if (outfile)
      fclose (outfile);

   popdir (&olddir);
   free (line);

   return !error;
}

static bool isslash (int c)
{
   return c=='/' || c=='\\';
}

static char *strrslash (const char *s)
{
   char *ret = strrchr (s, '/');
   if (ret)
      return ret;

   return strrchr (s, '\\');
}


static bool removedir (const char *target)
{
   if (!target || !target[0] || isslash(target[0]) || target[0] == '.') {
      FRM_ERROR ("Error: invalid directory removal name [%s]\n", target);
      errno = EINVAL;
      return false;
   }

   char *olddir = pushdir (target);
   if (!olddir) {
      FRM_ERROR ("Error: failed to switch to [%s]: %m\n", target);
      return false;
   }

   DIR *dirp = opendir (".");
   if (!dirp) {
      FRM_ERROR ("Error: failed to read directory [%s]: %m\n", target);
      return false;
   }
   struct dirent *de;
   while ((de = readdir (dirp))) {
      if (de->d_name[0] == '.')
         continue;
      if (wrapper_isdir (de)) {
         removedir (de->d_name);
      } else {
         if ((unlink (de->d_name)) != 0) {
            FRM_ERROR ("Error: unlink [%s]: %m\n", de->d_name);
            closedir (dirp);
            popdir (&olddir);
            return false;
         }
      }
   }
   closedir (dirp);
   popdir (&olddir);

   if ((rmdir (target)) != 0) {
      FRM_ERROR ("Error: Failed to rmdir() [%s]: %m\n", target);
      return false;
   }

   return true;
}

static int sort_entries (const void *lhs, const void *rhs)
{
   const char * const *lstr = lhs;
   const char * const *rstr = rhs;

   return strcmp (*lstr, *rstr);
}

static char **index_read (const char *dbpath)
{
   bool error = true;

   char **lines = NULL;
   size_t nlines = 0;
   size_t nbytes = 0;

   char *line = NULL;
   static const size_t line_len = 1024 * 1024;
   FILE *inf = NULL;

   char *olddir = NULL;

   if (!dbpath) {
      FRM_ERROR ("Error: dbpath is null\n");
      goto cleanup;
   }

   if (!(olddir = pushdir (dbpath))) {
      FRM_ERROR ("Error: failed to switch directory: %m\n");
      goto cleanup;
   }

   if (!(inf = fopen ("index", "r"))) {
      FRM_ERROR ("Error: failed to open index for reading: %m\n");
      goto cleanup;
   }

   if (!(line = malloc (line_len))) {
      FRM_ERROR ("OOM error allocating line for reading index\n");
      goto cleanup;
   }

   while ((fgets (line, line_len - 1, inf))) {
      char *eol = strchr (line, '\n');
      if (eol)
         *eol = 0;

      size_t newsize = nlines + 1;
      char **tmp = realloc (lines, (sizeof *tmp) * (newsize + 1));
      if (!tmp) {
         FRM_ERROR ("OOM error allocating storage for index\n");
         goto cleanup;
      }
      tmp[nlines+1] = NULL;
      if (!(tmp[nlines] = ds_str_dup (line))) {
         FRM_ERROR ("OOM error allocating index entry\n");
         free (tmp);
         goto cleanup;
      }
      nbytes += strlen (line) + 2;
      nlines = newsize;
      lines = tmp;
   }

   qsort (lines, nlines, sizeof *lines, sort_entries);

   error = false;

cleanup:
   popdir (&olddir);

   if (error) {
      for (size_t i=0; i<nlines; i++) {
         free (lines[i]);
      }
      free (lines);
      lines = NULL;
   }

   if (inf) {
      fclose (inf);
   }

   free (line);
   return lines;
}

static bool internal_frame_create (const char *path,
                                   const char *name, const char *msg)
{
   char *pwd = pushdir (path);
   if (!pwd) {
      FRM_ERROR ("Failed to store current working directory\n");
      return false;
   }

   if ((wrapper_mkdir (name))!=0) {
      FRM_ERROR ("Failed to create directory [%s/%s]: %m\n", path, name);
      popdir (&pwd);
      return false;
   }

   char *newdir = pushdir (name);
   if (!newdir) {
      FRM_ERROR ("Failed to switch directory [%s]\n", name);
      popdir (&pwd);
      return false;
   }


   char tstring[47];
   if (!(frm_writefile ("info",
               "mtime: ", uint64_string(tstring, (uint64_t)time(NULL)), "\n",
               NULL))) {
      FRM_ERROR ("Failed to create info file [%s/%s/info]: %m\n", path, name);
      return false;
   }

   if (!(frm_writefile ("payload", msg, "\n", NULL))) {
      FRM_ERROR ("Failed to create payload file [%s/%s/payload]: %m\n", path, name);
      popdir (&newdir);
      popdir (&pwd);
      return false;
   }

   if (!(frm_writefile ("index", "", NULL))) {
      FRM_ERROR ("Warning: failed to update index\n");
   }

   popdir (&newdir);
   popdir (&pwd);
   return true;
}

/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

static void frm_free (frm_t *frm)
{
   if (!frm) {
      errno = ENOENT;
      return;
   }

   free (frm->dbpath);
   free (frm->olddir);
   free (frm->lastmsg);
   free (frm);
}

static frm_t *frm_alloc (const char *dbpath, const char *olddir)
{
   frm_t *ret = calloc (1, sizeof *ret);
   if (!ret) {
      FRM_ERROR ("OOM error allocating frm_t");
      return NULL;
   }

   ret->dbpath = ds_str_dup (dbpath);
   ret->olddir = ds_str_dup (olddir);
   ret->lastmsg = ds_str_dup ("Success");

   if (!ret->dbpath || !ret->olddir || !ret->lastmsg) {
      FRM_ERROR ("Failed to allocate fields [dbpath:%p], [olddir:%p]\n",
               ret->dbpath, ret->olddir);
      frm_free (ret);
      ret = NULL;
   }

   size_t dbpath_len = strlen (dbpath);
   if (isslash (ret->dbpath[dbpath_len-1]))
      ret->dbpath[dbpath_len-1] = 0;

   return ret;
}

void frm_mem_free (void *ptr)
{
   free (ptr);
}

char *frm_readfile (const char *name)
{
   FILE *inf = fopen (name, "r");
   if (!inf) {
      // Commenting this out - caller must print out the fname of the failing
      // frm_readfile(fname) call.
      // FRM_ERROR ("Failed to open [%s] for reading: %m\n", name);
      return NULL;
   }

   if ((fseek (inf, 0, SEEK_END))!=0) {
      FRM_ERROR ("Failed to seek EOF [%s]: %m\n", name);
      fclose (inf);
      return NULL;
   }

   long len = ftell (inf);
   if (len < 0) {
      FRM_ERROR ("Failed to determine file length [%s]: %m\n", name);
      fclose (inf);
      return NULL;
   }

   if ((fseek (inf, 0, SEEK_SET))!=0) {
      FRM_ERROR ("Failed to reset file position [%s]: %m\n", name);
      fclose (inf);
      return NULL;
   }

   char *ret = malloc (len + 2);
   if (!ret) {
      FRM_ERROR ("OOM error allocating file contents [%s]\n", name);
      fclose (inf);
      return NULL;
   }

   size_t nbytes = fread (ret, 1, len+1, inf);
   if (!feof (inf) || ferror (inf)) {
      FRM_ERROR ("Read [%zu of %li] bytes in [%s]: %m\n", nbytes, len, name);
      fclose (inf);
      free (ret);
      return NULL;
   }

   fclose (inf);
   ret[len] = 0;
   return ret;
}

bool frm_vwritefile (const char *name, const char *data, va_list ap)
{
   FILE *outf = fopen (name, "wt");
   if (!outf) {
      FRM_ERROR ("Failed to open [%s] for writing: %m\n", name);
      return false;
   }

   while (data) {
      fprintf (outf, "%s", data);
      data = va_arg (ap, const char *);
   }
   fclose (outf);
   return true;
}

bool frm_writefile (const char *fname, const char *data, ...)
{
   va_list ap;

   va_start (ap, data);
   bool ret = frm_vwritefile (fname, data, ap);
   va_end (ap);

   return ret;
}

const char *frm_homepath (void)
{
   static bool set = false;

   if (set) {
      return getenv ("HOME");
   }


#ifdef PLATFORM_Windows
   const char *homedrive = getenv ("HOMEDRIVE");
   const char *homepath = getenv ("HOMEPATH");
   char *winhome = NULL;
   if ((ds_str_printf (&winhome, "HOME=%s%s", homedrive, homepath)) == 0) {
      FRM_ERROR ("OOM error allocating environment variable [%s=%s]\n",
            homedrive, homepath);
      return NULL;
   }

   if (!winhome) {
      FRM_ERROR ("Failed to set $HOME variable: [%s][%s][%s]\n",
            homedrive, homepath, winhome);
      free (winhome);
      return NULL;
   }

   if ((putenv (winhome)) != 0) {
      FRM_ERROR ("Warning: failed to set environment [%s]: %m\n", winhome);
   }

   free (winhome);
   homedrive = NULL;
   homepath = NULL;
   winhome = NULL;
#endif

   return getenv ("HOME");
}

frm_t *frm_create (const char *dbpath)
{
   if ((wrapper_mkdir (dbpath))!=0) {
      FRM_ERROR ("Failed to create directory [%s]: %m\n", dbpath);
      return NULL;
   }

   if (!(internal_frame_create (dbpath, "root", "ENTER YOUR NOTES HERE"))) {
      FRM_ERROR ("Failed to create frame [%s/root]: %m\n", dbpath);
      return NULL;
   }

   char *pwd = pushdir (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to store current working directory\n");
      return NULL;
   }

   if (!(history_append (dbpath, "root"))) {
      FRM_ERROR ("Failed to set the working frame to [root]\n");
      popdir (&pwd);
      return NULL;
   }

   if (!(frm_writefile("index", "", NULL))) {
      FRM_ERROR ("Error: failed to write index: %m\n");
      popdir (&pwd);
      return NULL;
   }
   popdir (&pwd);
   return frm_init (dbpath);
}

frm_t *frm_init (const char *dbpath)
{
   bool error = true;
   frm_t *ret = NULL;
   char *history = NULL, *frame = NULL, *newpath = NULL;

   char *pwd = pushdir (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to switch directory to [%s]: %m\n", dbpath);
      goto cleanup;
   }

   int fd = -1;
   if ((fd = open (lockfile, O_CREAT|O_EXCL, S_IRWXU))<0) {
      FRM_ERROR ("Error: Failed to create lockfile [%s][%s]: %m\n",
               dbpath, lockfile);
      goto cleanup;
   }

   // TODO: replace this with frm_history
   history = frm_readfile ("history");
   frame = NULL;
   if (!history) {
      FRM_ERROR ("Warning: no history found, defaulting to root frame\n");
      frame = ds_str_dup ("root");
   } else {
      char *eol = strchr (history, '\n');
      if (!eol) {
         eol = &history[strlen (history)];
      }
      *eol = 0;
      frame = ds_str_dup (history);
      if (!frame) {
         FRM_ERROR ("OOM error copying last working frame path\n");
         goto cleanup;
      }
   }

   newpath = pushdir (frame);
   if (!newpath) {
      FRM_ERROR ("Failed to switch to frame [%s]: %m\n", frame);
      goto cleanup;
   }

   if (!(ret = frm_alloc (dbpath, pwd))) {
      FRM_ERROR ("OOM error allocating frm_t\n");
      goto cleanup;
   }

   error = false;

cleanup:
   if ((close (fd)) != 0) {
      FRM_ERROR ("Error: unable to close lockfile descriptor %i: %m\n", fd);
   }

   free (pwd);
   free (history);
   free (frame);
   free (newpath);
   if (error) {
      frm_close (ret);
   }

   return ret;
}

void frm_close (frm_t *frm)
{
   if (!frm) {
      errno = ENOENT;
      return;
   }

   popdir (&frm->olddir);
   if ((chdir (frm->dbpath))!=0) {
      ERR (frm, "Error: Failed to switch to dbpath [%s]: %m\n", frm->dbpath);
      ERR (frm, "Warning: lockfile must be maually deleted [%s][%s]\n",
               frm->dbpath, lockfile);
   } else {
      errno = 0;
      if ((remove (lockfile))!=0) {
         ERR (frm, "Error: Failed to remove [%s][%s]: %m\n", frm->dbpath, lockfile);
         ERR (frm, "Warning: lockfile must be maually deleted [%s/%s]\n",
               frm->dbpath, lockfile);
      }
   }

   free (frm->dbpath);
   free (frm->lastmsg);
   free (frm);
}

char *frm_history (frm_t *frm, size_t count)
{
   if (!frm) {
      FRM_ERROR ("Found null object for frm_t\n");
      errno = EINVAL;
      return ds_str_dup ("");
   }

   return history_read(frm->dbpath, count);
}

char *frm_current (frm_t *frm)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
      errno = EINVAL;
      return ds_str_dup ("");
   }
   char *tmp = getcwd (NULL, 0);
   if (!tmp) {
      ERR (frm, "Failed to get the current working directory\n");
      tmp = ds_str_dup ("");
   }

   char *pattern = ds_str_cat (frm->dbpath, FRM_DIR_SEPARATOR, NULL);
   char *ret = ds_str_strsubst(tmp, pattern, "", NULL);
   free (pattern);
   if (!ret) {
      ERR (frm, "OOM error allocating current path\n");
      ret = ds_str_dup ("");
   }

   free (tmp);
   return ret;
}

char *frm_payload (void)
{
   char *ret = frm_readfile ("payload");
   if (!ret) {
      FRM_ERROR ("Failed to read [payload]: %m\n");
      return ds_str_dup ("");
   }

   return ret;
}

struct info_t {
   uint64_t mtime;
};

static bool read_info (struct info_t *dst, const char *fname)
{
   char *data = frm_readfile(fname);
   if (!data) {
      FRM_ERROR ("Failed to read [%s]: %m\n", fname);
      return false;
   }

   memset (dst, 0, sizeof *dst);

   char *name = NULL;
   char *sptr = NULL;
   char *tok = strtok_r (data, "\n", &sptr);
   do {
      free (name);
      if (!(name = ds_str_dup (tok))) {
         FRM_ERROR ("OOM error allocating info fields\n");
         free (data);
         return false;
      }
      char *value = strchr (name, ':');
      if (value) {
         *value++ = 0;
      }

      if ((strcmp (name, "mtime"))==0) {
         if ((sscanf (value, "%" PRIu64, &dst->mtime))!=1) {
            FRM_ERROR ("Could not parse date value [%s]\n", value);
         }
      }
   } while ((tok = strtok_r (NULL, "\n", &sptr)));
   free (name);
   free (data);
   return true;
}

static bool write_info (const struct info_t *info, const char *fname)
{
   char tstring[47];
   if (!(frm_writefile (fname,
               "mtime:", uint64_string (tstring, info->mtime), "\n",
               NULL))) {
      FRM_ERROR ("Failed to write info: %m\n");
      return false;
   }

   return true;
}

static bool update_info_mtime (const char *fname)
{
   struct info_t info;
   if (!(read_info (&info, fname))) {
      FRM_ERROR ("Failed to read info file: %m\n");
      return false;
   }

   info.mtime = time (NULL);
   if (!(write_info (&info, fname))) {
      FRM_ERROR ("Failed to update info file: %m\n");
      return false;
   }

   return true;
}


uint64_t frm_date_epoch (void)
{
   struct info_t info;
   if (!(read_info (&info, "info"))) {
      FRM_ERROR ("Failed to read [info]: %m\n");
      return (uint64_t)-1;
   }

   return info.mtime;
}

char *frm_date_str (void)
{
   uint64_t epoch = frm_date_epoch ();
   if (epoch == (uint64_t)-1) {
      FRM_ERROR ("Failed to retrieve mtime\n");
      return ds_str_dup ("");
   }
   char *tmp = ctime ((time_t *)&epoch);
   char *eol = strchr (tmp, '\n');
   if (eol)
      *eol = 0;
   return ds_str_dup (tmp);
}

const char *frm_lastmsg (frm_t *frm)
{
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = EINVAL;
      return "";
   }

   return frm->lastmsg;
}


static bool internal_frm_push (frm_t *frm, const char *name, const char *message,
                               bool dir_change)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

   if ((wrapper_mkdir (name))!=0) {
      ERR (frm, "Failed to create directory [%s]: %m\n", name);
      return false;
   }

   char *olddir = pushdir (name);
   if (!olddir) {
      ERR (frm, "Failed to switch to [%s]: %m\n", name);
      return false;
   }

   if (!(frm_writefile ("payload", message, "\n", NULL))) {
      ERR (frm, "Failed to write message to [%s/payload]: %m\n", name);
      popdir (&olddir);
      return false;
   }

   char tstring[47];
   if (!(frm_writefile ("info",
               "mtime: ", uint64_string(tstring, (uint64_t)time(NULL)), "\n",
               NULL))) {
      ERR (frm, "Failed to create info file [%s/info]: %m\n", name);
      popdir (&olddir);
      return false;
   }

   char *path = get_path (frm);
   if (dir_change) {
      if (!(history_append(frm->dbpath, path))) {
         ERR (frm, "Failed to update history\n");
         free (path);
         popdir (&olddir);
         return false;
      }
   }

   if (!(index_add (frm->dbpath, path))) {
      ERR (frm, "Warning: failed to update index\n");
   }

   if (dir_change) {
      free (olddir);
   } else {
      popdir (&olddir);
   }

   free (path);
   return true;
}

bool frm_push (frm_t *frm, const char *name, const char *message)
{
   return internal_frm_push (frm, name, message, true);
}

bool frm_new (frm_t *frm, const char *name, const char *message)
{
   return internal_frm_push (frm, name, message, false);
}

bool frm_payload_replace (const char *message)
{
   if (!(frm_writefile ("payload", message, NULL))) {
      FRM_ERROR ("Failed to write [payload]: %m\n");
      return false;
   }

   if (!(update_info_mtime ("info"))) {
      FRM_ERROR ("Failed to update info file with mtime: %m\n");
      return false;
   }

   return true;
}

bool frm_payload_append (const char *message)
{
   char *current = frm_readfile ("payload");
   if (!current) {
      FRM_ERROR ("Warning: failed to read [payload]: %m\n");
   }

   bool ret = true;
   if (!(frm_writefile ("payload", current, "\n", message, NULL))) {
      FRM_ERROR ("Error writing [payload]: %m\n");
      ret = false;
   }
   free (current);

   if (ret && !(update_info_mtime ("info"))) {
      FRM_ERROR ("Failed to update info file with mtime: %m\n");
      ret = false;
   }

   return ret;
}

char *frm_payload_fname (void)
{
   char *pwd = getcwd (NULL, 0);
   if (!pwd) {
      FRM_ERROR ("Error: failed to get current working directory: %m\n");
      return NULL;
   }

   char *fname = ds_str_cat (pwd, "/payload", NULL);
   if (!fname) {
      FRM_ERROR ("OOM error allocating filename of payload file\n");
   }

   free (pwd);
   return fname;
}

bool frm_top (frm_t *frm)
{
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = ENOENT;
      return false;
   }

   char *target = ds_str_cat (frm->dbpath, "/root", NULL);
   if (!target) {
      ERR (frm, "OOM error allocating path for root frame\n");
      return false;
   }

   if ((chdir (target))!=0) {
      ERR (frm, "Error: failed to switchdir to [%s/root]: %m\n", target);
      free (target);
      return false;
   }

   free (target);
   if (!(history_append (frm->dbpath, "root"))) {
      ERR (frm, "Error: failed to record history\n");
      return false;
   }

   return true;
}

bool frm_up (frm_t *frm)
{
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

   char *pwd = getcwd (NULL, 0);
   if (!pwd) {
      ERR (frm, "Error: could not retrieve the current working directory: %m\n");
      return false;
   }

   char *tmp = ds_str_cat (frm->dbpath, "/root", NULL);
   if (!tmp) {
      ERR (frm, "OOM error allocating temporary pattern\n");
      return false;
   }

   if ((strcmp (tmp, pwd))==0) {
      ERR (frm, "Error: cannot go up a level beyond dbpath [%s]\n", frm->dbpath);
      free (tmp);
      free (pwd);
      return false;
   }
   free (tmp);

   char *olddir = pushdir ("..");
   if (!olddir) {
      ERR (frm, "Failed to switch directory [..]: %m\n");
      free (pwd);
      return false;
   }

   char *path = get_path (frm);
   if (!(history_append (frm->dbpath, path))) {
      ERR (frm, "Failed to set the working frame to [root]\n");
      free (olddir);
      free (pwd);
      return false;
   }

   free (pwd);
   free (path);
   free (olddir);
   return true;
}

bool frm_down (frm_t *frm, const char *target)
{
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

   if ((chdir (target))!=0) {
      ERR (frm, "Failed to switch to target [%s]\n", target);
      return false;
   }

   char *pwd = get_path (frm);
   if (!(history_append (frm->dbpath, pwd))) {
      ERR (frm, "Failed to set the working frame to [root]\n");
      free (pwd);
      return false;
   }

   free (pwd);
   return true;
}

bool frm_switch (frm_t *frm, const char *target)
{
   if (!frm || !target || !target[0]) {
      ERR (frm, "Error: null objects passed for frm_switching\n");
      return false;
   }

   char *suffixed = isslash (target[strlen(target)-1])
      ? ds_str_dup (target)
      : ds_str_cat (target, "/", NULL);
   if (!suffixed) {
      ERR (frm, "OOM error allocating suffixed string\n");
      errno = ENOMEM;
      return false;
   }

   char *actual = history_find (frm->dbpath, suffixed);
   free (suffixed);
   if (!actual) {
      if (!(actual = ds_str_dup (target))) {
         ERR (frm, "OOM error allocating target to switch to [%s]\n", target);
         return false;
      }
   }

   if ((chdir (frm->dbpath))!=0) {
      ERR (frm, "Failed to switch to dbpath [%s]\n", frm->dbpath);
      free (actual);
      return false;
   }

   if ((chdir (actual))!=0) {
      ERR (frm, "Failed to switch to target [%s]\n", actual);
      free (actual);
      return false;
   }

   char *pwd = get_path (frm);
   if (!(history_append (frm->dbpath, pwd))) {
      ERR (frm, "Failed to set the working frame to [root]\n");
      free (actual);
      free (pwd);
      return false;
   }

   free (actual);
   free (pwd);
   return true;
}

bool frm_switch_direct (frm_t *frm, const char *target)
{
   if (!frm || !target || !target[0]) {
      ERR (frm, "Error: null objects passed for frm_switching\n");
      return false;
   }

   char *suffixed = isslash (target[strlen(target)-1])
      ? ds_str_dup (target)
      : ds_str_cat (target, "/", NULL);
   if (!suffixed) {
      ERR (frm, "OOM error allocating suffixed string\n");
      errno = ENOMEM;
      return false;
   }

   if ((chdir (frm->dbpath))!=0) {
      ERR (frm, "Failed to switch to dbpath [%s]\n", frm->dbpath);
      free (suffixed);
      return false;
   }

   if ((chdir (suffixed))!=0) {
      ERR (frm, "Failed to switch to target [%s]\n", suffixed);
      free (suffixed);
      return false;
   }

   char *pwd = get_path (frm);
   if (!(history_append (frm->dbpath, pwd))) {
      ERR (frm, "Failed to append history: [%s]\n", pwd);
      free (suffixed);
      free (pwd);
      return false;
   }

   free (suffixed);
   free (pwd);
   return true;
}

bool frm_back (frm_t *frm, size_t index)
{
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = ENOENT;
      return false;
   }

   bool error = true;
   FILE *infile = NULL;
   char *line = NULL;
   char *olddir = NULL;
   static const size_t line_len = 1024 * 1024 * 10;

   if (!(olddir = pushdir (frm->dbpath))) {
      ERR (frm, "Failed to switch to dbpath directory [%s]: %m\n", frm->dbpath);
      goto cleanup;
   }

   if (!(infile = fopen ("history", "r"))) {
      ERR (frm, "Failed to open [history] for reading: %m\n");
      goto cleanup;
   }

   if (!(line = malloc (line_len))) {
      ERR (frm, "OOM error allocating line for reading history\n");
   }

   size_t nline = 0;
   line[0] = 0;
   while ((fgets (line, line_len - 1, infile))) {
      char *tmp = strchr (line, '\n');
      if (tmp)
         *tmp = 0;
      if (nline++ >= index)
         break;
   }

   if (!line[0]) {
      ERR (frm, "History file appears to be empty, aborting switch.\n");
      goto cleanup;
   }

   frm_switch (frm, line);

   error = false;
cleanup:
   free (line);
   free (olddir);
   if (infile)
      fclose (infile);

   return !error;
}

void frm_strarray_free (char **array)
{
   for (size_t i=0; array && array[i]; i++) {
      free (array[i]);
   }
   free (array);
}

bool frm_pop (frm_t *frm, bool force)
{
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

   if (!force) {
      char **subframes = frm_list (frm, NULL);
      size_t nsubframes = 0;
      for (size_t i=0; subframes && subframes[i]; i++) {
         nsubframes++;
         free (subframes[i]);
      }
      free (subframes);

      if (nsubframes!=0) {
         ERR (frm, "Error: cannot pop a frame that has children\n");
         errno = ENOTEMPTY;
         return false;
      }
   }

   char *oldpath = get_path (frm);
   if (!oldpath) {
      ERR (frm, "Error: failed to retrieve current working directory: %m\n");
      return false;
   }

   if (!(frm_up (frm))) {
      ERR (frm, "Error: failed to switch to parent frame: %m\n");
      free (oldpath);
      return false;
   }

   if (!(frm_delete (frm, oldpath))) {
      ERR (frm, "Error: failed to remove path [%s]: %m\n", oldpath);
      free (oldpath);
      return false;
   }

   free (oldpath);
   return true;
}

bool frm_rename (frm_t *frm, const char *newname)
{
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

   char *current_name = frm_current(frm);
   if (!current_name) {
      ERR (frm, "OOM error retrieving current frame path\n");
      return false;
   }

   const char *oldname = strrslash (current_name);
   if (!oldname) {
      ERR(frm, "Error: Internal corruption (current node has no slash): [%s]\n",
               current_name);
      free (current_name);
      return false;
   }
   oldname++;

   if (!(frm_up (frm))) {
      ERR (frm, "Error: Failed to switch to parent directory [%s/..]: %m\n",
            oldname);
      free (current_name);
      return false;
   }

   if ((rename (oldname, newname))!=0) {
      ERR (frm, "Error: failed to rename [%s] to [%s]: %m\n", oldname, newname);
      free (current_name);
      return false;
   }

   if (!(index_remove (frm->dbpath, current_name))) {
      ERR (frm, "Warning: failed to remove [%s] from index\n", current_name);
   }
   free (current_name);

   if (!(frm_down(frm, newname))) {
      ERR (frm, "Warning: cannot switch to renamed frame [%s]: %m\n", newname);
      return false;
   }

   current_name = frm_current(frm);
   if (!current_name) {
      ERR (frm, "OOM error retrieving current frame path\n");
      return false;
   }

   if (!(index_add(frm->dbpath, current_name))) {
      ERR (frm, "Warning: failed to add [%s] to index\n", current_name);
   }
   free (current_name);
   return true;
}

bool frm_delete (frm_t *frm, const char *target)
{
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

   char **subframes = frm_list (frm, target);
   char *olddir = pushdir (frm->dbpath);
   if (!olddir) {
      ERR (frm, "Error: failed to switch directory: %m\n");
      frm_strarray_free(subframes);
      return false;
   }

   if (!(removedir (target))) {
      ERR (frm, "Error: failed to remove directory[%s]: %m\n", target);
      popdir (&olddir);
      frm_strarray_free (subframes);
      return false;
   }

   for (size_t i=0; subframes && subframes[i]; i++) {
      if (!(index_remove (frm->dbpath, subframes[i]))) {
         ERR (frm, "Warning: failed to remove [%s] from index\n", subframes[i]);
      }
   }
   frm_strarray_free (subframes);
   if (!(index_remove (frm->dbpath, target))) {
      ERR (frm, "Warning: failed to remove [%s] from index\n", target);
   }

   popdir (&olddir);
   return true;
}

static char **match (frm_t *frm, const char *sterm,
      uint32_t flags, const char *from)
{
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = EINVAL;
      return NULL;
   }

   char **index = index_read(frm->dbpath);
   if (!index) {
      ERR (frm, "Error: failed to read index\n");
      return NULL;
   }

   bool error = true;
   char **results = NULL;
   size_t results_len = 0;

   for (size_t i=0; index[i]; i++) {
      bool found = strstr (index[i], from)!=NULL;
      if (!found) {
         continue;
      }

      found = strstr (index[i], sterm)!=NULL;
      if (flags & FRM_MATCH_INVERT) {
         found = !found;
      }

      if (found) {
         size_t newsize = results_len + 1;
         char **tmp = realloc (results, (sizeof *tmp) * (newsize + 1));
         if (!tmp) {
            ERR (frm, "OOM error allocating match results\n");
            goto cleanup;
         }
         tmp[newsize] = NULL;
         if (!(tmp[results_len] = ds_str_dup (index[i]))) {
            ERR (frm, "OOM error allocating match item %zu\n", i);
            goto cleanup;
         }

         results = tmp;
         results_len++;
      }
   }

   // If we reached this point with NULL results then no errors occurred but
   // no matches were found either. Must return an empty list.
   if (!results && !(results = calloc (1, sizeof *results))) {
      ERR (frm, "OOM error allocating empty list\n");
      goto cleanup;
   }

   error = false;
cleanup:
   for (size_t i=0; index && index[i]; i++) {
      free (index[i]);
   }
   free (index);

   if (error) {
      for (size_t i=0; results && results[i]; i++) {
         free (results[i]);
      }
      free (results);
      results = 0;
   }

   return results;
}

char **frm_list (frm_t *frm, const char *from)
{
   if (!frm) {
      FRM_ERROR ("Error: null object passed for frm_t\n");
      errno = ENOENT;
      return NULL;
   }

   char *olddir = frm_switch_path (frm, from);
   if (!olddir) {
      ERR (frm, "Error: failed to switch path to [%s]\n", from);
      return NULL;
   }

   char *current = frm_current (frm);
   if (!current) {
      ERR (frm, "Error: unable to determine current frame: %m\n");
      popdir (&olddir);
      return NULL;
   }

   char *prefixed_current = ds_str_cat (current, "/", NULL);
   if (!prefixed_current) {
      ERR (frm, "OOM error allocating temporary string for current frame\n");
      free (current);
      popdir (&olddir);
      return NULL;
   }
   free (current);

   char **ret = match (frm, "", 0, prefixed_current);
   free (prefixed_current);
   popdir (&olddir);
   return ret;
}

char **frm_match (frm_t *frm, const char *sterm, uint32_t flags)
{
   char *current = frm_current (frm);
   if (!current) {
      ERR (frm, "Failed to retrieve the current frame\n");
      return NULL;
   }

   char **results = match (frm, sterm, flags, current);
   free (current);
   return results;
}

char **frm_match_from_root (frm_t *frm, const char *sterm, uint32_t flags)
{
   return match (frm, sterm, flags, "root");
}


/* ************************************************************ */


struct frm_node_t {
   const frm_node_t *parent;
   ds_array_t *children;

   char *name;
   uint64_t date;
};

static void node_del (frm_node_t *node)
{
   if (!node)
      return;

   size_t nchildren = ds_array_length(node->children);
   for (size_t i=0; i<nchildren; i++) {
      frm_node_t *child = ds_array_get(node->children, i);
      if (!child) {
         FRM_ERROR ("Possible corruption, attempt to overflow array: %zu\n", i);
      }
      node_del (child);
   }
   ds_array_del (node->children);
   free (node->name);
   free (node);
}

static frm_node_t *node_new (const frm_node_t *parent,
                             const char *name, uint64_t date)
{
   frm_node_t *ret = calloc (1, sizeof *ret);
   if (!ret) {
      FRM_ERROR ("OOM error allocating node\n");
   }

   if (!(ret->children = ds_array_new ())) {
      FRM_ERROR ("OOM error allocating array object for children\n");
      free (ret);
      return NULL;
   }

   ret->parent = parent;
   ret->date = date;

   if (!(ret->name = ds_str_dup (name))) {
      FRM_ERROR ("OOM error allocating name field for node\n");
      node_del (ret);
      ret = NULL;
   }

   return ret;
}

static frm_node_t *node_open (const frm_node_t *parent, const char *dirname)
{
   bool error = true;
   DIR *dirp = NULL;
   char *pwd = pushdir (dirname);
   frm_node_t *ret = NULL;
   struct info_t info;

   if (!pwd) {
      FRM_ERROR ("Error: failed to switch to directory [%s]: %m\n", dirname);
      goto cleanup;
   }

   if (!(dirp = opendir ("."))) {
      FRM_ERROR ("Error: failed to read directory [%s]: %m\n", dirname);
      goto cleanup;
   }

   if (!(read_info (&info, "info"))) {
      FRM_ERROR ("Failed to read info file: %m\n");
      return false;
   }

   if (!(ret = node_new (parent, dirname, info.mtime))) {
      FRM_ERROR ("Error: failed to create new node [%s]\n", dirname);
      goto cleanup;
   }

   struct dirent *de;
   while ((de = readdir (dirp))) {
      if (de->d_name[0] == '.')
         continue;
      if (wrapper_isdir (de)) {
         frm_node_t *child = node_open (ret, de->d_name);
         if (!child) {
            FRM_ERROR ("Error: failed to read child [%s] of [%s]: %m\n",
                     de->d_name, dirname);
         }
         if (!(ds_array_ins_tail(ret->children, child))) {
            FRM_ERROR ("OOM error adding child [%s] to [%s]\n",
                     de->d_name, dirname);
            node_del (child);
            goto cleanup;
         }
      }
   }

   error = false;

cleanup:
   if (error) {
      node_del (ret);
      ret = NULL;
   }

   closedir (dirp);
   popdir (&pwd);
   return ret;
}

frm_node_t *frm_node_create (frm_t *frm)
{
   char *pwd = pushdir (frm->dbpath);
   if (!pwd) {
      ERR (frm, "Error: failed to switch to [%s]: %m\n", frm->dbpath);
      return NULL;
   }

   // TODO: Must switch to basedir first.
   frm_node_t *ret = node_open (NULL, "root");
   popdir (&pwd);
   return ret;
}

void frm_node_free (frm_node_t *rootnode)
{
   node_del (rootnode);
}


const char *frm_node_name (const frm_node_t *node)
{
   return node ? node->name : "???";
}

uint64_t frm_node_date (const frm_node_t *node)
{
   return node ? node->date : (uint64_t)-1;
}

char *frm_node_fpath (const frm_node_t *node)
{
   if (!node) {
      return "";
   }

   char *parent_fpath = frm_node_fpath (node->parent);
   if (!parent_fpath) {
      FRM_ERROR ("OOM error getting parent name [%s]\n", node->name);
      return NULL;
   }

   char *ret = ds_str_cat (parent_fpath, "/", node->name, NULL);

   if (!ret) {
      FRM_ERROR ("OOM error joining name [%s] to parent fpath [%s]\n",
               node->name, parent_fpath);
   }
   free (parent_fpath);

   return ret;
}


size_t frm_node_nchildren (const frm_node_t *node)
{
   return node ? ds_array_length (node->children) :  0;
}

const frm_node_t *frm_node_child (const frm_node_t *node, size_t index)
{
   if (!node || index > (ds_array_length(node->children) + 1)) {
      FRM_ERROR ("Error: possible overflow detected\n");
      return NULL;
   }

   return ds_array_get (node->children, index);
}

const frm_node_t *frm_node_parent (const frm_node_t *node)
{
   return node ? node->parent : NULL;
}

const frm_node_t *frm_node_root (const frm_node_t *node)
{
   if (!node)
      return NULL;

   if (!node->parent)
      return node;

   return frm_node_root (node->parent);
}

const frm_node_t *frm_node_find (const frm_node_t *node, const char *fpath)
{
   size_t slen = strlen (node->name);
   if ((memcmp (node->name, fpath, slen))!=0)
      return NULL;

   size_t nchildren = ds_array_length (node->children);
   for (size_t i=0; i<nchildren; i++) {
      frm_node_t *child = ds_array_get (node->children, i);
      if (!child) {
         FRM_ERROR ("Possible corruption, indexing [%zu] of [%s]\n",
                  i, node->name);
         return NULL;
      }

      const frm_node_t *target = frm_node_find (child, &fpath[slen+1]);
      if (target)
         return target;
   }
   return NULL;
}

char *frm_switch_path (frm_t *frm, const char *from)
{
   // Attempt to switch to 'from' as a relative path. If that fails
   // attempt to switch to 'from' as an absolute framename (absolute
   // relative to frm->dbpath).
   if (!from || !from[0]) {
      from = "./";
   }
   char *olddir = pushdir (from);
   if (!olddir) {
      ERR (frm, "Warning: using relative path [%s] failed, trying absolute path\n",
            from);
      char *tmp = ds_str_cat (frm->dbpath, "/", from, NULL);
      if (!tmp) {
         ERR (frm, "OOM error allocating absolute path for framename [%s/%s]\n",
               frm->dbpath, from);
         return NULL;
      }
      char *first_errmsg = ds_str_dup (strerror (errno));
      if (!first_errmsg) {
         ERR (frm, "OOM error allocating error message string: %i\n", errno);
         return NULL;
      }

      olddir = pushdir (tmp);
      free (tmp);
      if (!olddir) {
         ERR (frm, "Neither [%s] nor [%s/%s] could be used: [%s][%m]\n",
               from, frm->dbpath, from, first_errmsg);
         free (first_errmsg);
         return NULL;
      }
      free (first_errmsg);
   }

   return olddir;
}
