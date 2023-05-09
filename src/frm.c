
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

struct frm_t {
   char *dbpath;
   char *olddir;
};

#define REMOVEME FRM_ERROR

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
      FRM_ERROR ("Error: failed to retrieve path: %m\n");
      return NULL;
   }

   // Fixup the path, if it is an absolute path.
   size_t nbytes = strlen (frm->dbpath);
   if (nbytes > strlen (pwd)) {
      FRM_ERROR ("Error: internal corruption detected\n");
      free (pwd);
      return NULL;
   }

   char *path = ds_str_dup (&pwd[nbytes + 1]);
   if (!path) {
      FRM_ERROR ("OOM error: allocating path\n");
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

static bool removedir (const char *target)
{
   if (!target || !target[0] || target[0]=='/' || target[0] == '.') {
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
      if (de->d_type == DT_DIR) {
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

static bool node_create (const char *path, const char *name, const char *msg)
{
   char *pwd = pushdir (path);
   if (!pwd) {
      FRM_ERROR ("Failed to store current working directory\n");
      return false;
   }

   static const mode_t mode = 0777;
   if ((mkdir (name, mode))!=0) {
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

   if (!ret->dbpath || !ret->olddir) {
      FRM_ERROR ("Failed to allocate fields [dbpath:%p], [olddir:%p]\n",
               ret->dbpath, ret->olddir);
      frm_free (ret);
      ret = NULL;
   }

   size_t dbpath_len = strlen (dbpath);
   if (ret->dbpath[dbpath_len-1] == '/')
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
      FRM_ERROR ("Failed to open [%s] for reading: %m\n", name);
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

   char *ret = malloc (len + 1);
   if (!ret) {
      FRM_ERROR ("OOM error allocating file contents [%s]\n", name);
      fclose (inf);
      return NULL;
   }

   size_t nbytes = fread (ret, 1, len, inf);
   if (nbytes != (size_t)len) {
      FRM_ERROR ("Read {%zu of %li] bytes in [%s]: %m\n", nbytes, len, name);
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
   FILE *outf = fopen (name, "w");
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

frm_t *frm_create (const char *dbpath)
{
   static const mode_t mode = 0777;
   if ((mkdir (dbpath, mode))!=0) {
      FRM_ERROR ("Failed to create directory [%s]: %m\n", dbpath);
      return NULL;
   }

   if (!(node_create (dbpath, "root", "ENTER YOUR NOTES HERE"))) {
      FRM_ERROR ("Failed to create node [%s/root]: %m\n", dbpath);
      return NULL;
   }

   char *pwd = pushdir (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to store current working directory\n");
      return NULL;
   }

   if (!(history_append (dbpath, "root"))) {
      FRM_ERROR ("Failed to set the working node to [root]\n");
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
   char *history = NULL, *node = NULL, *newpath = NULL;

   char *pwd = pushdir (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to switch directory to [%s]: %m\n", dbpath);
      goto cleanup;
   }

   // TODO: replace this with frm_history
   history = frm_readfile ("history");
   node = NULL;
   if (!history) {
      FRM_ERROR ("Warning: no history found, defaulting to root node\n");
      node = "root";
   } else {
      char *eol = strchr (history, '\n');
      if (!eol) {
         eol = &history[strlen (history)];
      }
      *eol = 0;
      node = ds_str_dup (history);
      if (!node) {
         FRM_ERROR ("OOM error copying last working node path\n");
         goto cleanup;
      }
   }

   newpath = pushdir (node);
   if (!newpath) {
      FRM_ERROR ("Failed to switch to node [%s]: %m\n", node);
      goto cleanup;
   }

   if (!(ret = frm_alloc (dbpath, pwd))) {
      FRM_ERROR ("OOM error allocating frm_t\n");
      goto cleanup;
   }

   error = false;

cleanup:
   free (pwd);
   free (history);
   free (node);
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
   free (frm->dbpath);
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
      FRM_ERROR ("Failed to get the current working directory\n");
      tmp = ds_str_dup ("");
   }

   char *pattern = ds_str_cat (frm->dbpath, "/", NULL);
   char *ret = ds_str_strsubst(tmp, pattern, "", NULL);
   free (pattern);
   if (!ret) {
      FRM_ERROR ("OOM error allocating current path\n");
      ret = ds_str_dup ("");
   }

   free (tmp);
   return ret;
}

char *frm_payload (frm_t *frm)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
      errno = EINVAL;
      return ds_str_dup ("");
   }
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

static bool read_info (frm_t *frm, struct info_t *dst, const char *fname)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

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

static bool write_info (frm_t *frm, const struct info_t *info, const char *fname)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

   char tstring[47];
   if (!(frm_writefile (fname,
               "mtime:", uint64_string (tstring, info->mtime), "\n",
               NULL))) {
      FRM_ERROR ("Failed to write info: %m\n");
      return false;
   }

   return true;
}

static bool update_info_mtime (frm_t *frm, const char *fname)
{
   struct info_t info;
   if (!(read_info (frm, &info, fname))) {
      FRM_ERROR ("Failed to read info file: %m\n");
      return false;
   }

   info.mtime = time (NULL);
   if (!(write_info (frm, &info, fname))) {
      FRM_ERROR ("Failed to update info file: %m\n");
      return false;
   }

   return true;
}


uint64_t frm_date_epoch (frm_t *frm)
{
   struct info_t info;
   if (!(read_info (frm, &info, "info"))) {
      FRM_ERROR ("Failed to read [info]: %m\n");
      return (uint64_t)-1;
   }

   return info.mtime;
}

char *frm_date_str (frm_t *frm)
{
   uint64_t epoch = frm_date_epoch (frm);
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

static bool internal_frm_push (frm_t *frm, const char *name, const char *message,
                               bool dir_change)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

   if ((mkdir (name, 0777))!=0) {
      FRM_ERROR ("Failed to create directory [%s]: %m\n", name);
      return false;
   }

   char *olddir = pushdir (name);
   if (!olddir) {
      FRM_ERROR ("Failed to switch to [%s]: %m\n", name);
      return false;
   }

   if (!(frm_writefile ("payload", message, "\n", NULL))) {
      FRM_ERROR ("Failed to write message to [%s/payload]: %m\n", name);
      popdir (&olddir);
      return false;
   }

   char tstring[47];
   if (!(frm_writefile ("info",
               "mtime: ", uint64_string(tstring, (uint64_t)time(NULL)), "\n",
               NULL))) {
      FRM_ERROR ("Failed to create info file [%s/info]: %m\n", name);
      popdir (&olddir);
      return false;
   }

   char *path = get_path (frm);
   if (dir_change) {
      if (!(history_append(frm->dbpath, path))) {
         FRM_ERROR ("Failed to update history\n");
         free (path);
         popdir (&olddir);
         return false;
      }
   }

   if (!(index_add (frm->dbpath, path))) {
      FRM_ERROR ("Warning: failed to update index\n");
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

bool frm_payload_replace (frm_t *frm, const char *message)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

   if (!(frm_writefile ("payload", message, NULL))) {
      FRM_ERROR ("Failed to write [payload]: %m\n");
      return false;
   }

   if (!(update_info_mtime (frm, "info"))) {
      FRM_ERROR ("Failed to update info file with mtime: %m\n");
      return false;
   }

   return true;
}

bool frm_payload_append (frm_t *frm, const char *message)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
      errno = EINVAL;
      return false;
   }

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

   if (ret && !(update_info_mtime (frm, "info"))) {
      FRM_ERROR ("Failed to update info file with mtime: %m\n");
      ret = false;
   }

   return ret;
}

char *frm_payload_fname (frm_t *frm)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
      errno = ENOENT;
      return false;
   }

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
      FRM_ERROR ("OOM error allocating path for root node\n");
      return false;
   }

   if ((chdir (target))!=0) {
      FRM_ERROR ("Error: failed to switchdir to [%s/root]: %m\n", target);
      free (target);
      return false;
   }

   free (target);
   if (!(history_append (frm->dbpath, "root"))) {
      FRM_ERROR ("Error: failed to record history\n");
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
      FRM_ERROR ("Error: could not retrieve the current working directory: %m\n");
      return false;
   }

   char *tmp = ds_str_cat (frm->dbpath, "/root", NULL);
   if (!tmp) {
      FRM_ERROR ("OOM error allocating temporary pattern\n");
      return false;
   }

   if ((strcmp (tmp, pwd))==0) {
      FRM_ERROR ("Error: cannot go up a level beyond dbpath [%s]\n", frm->dbpath);
      free (tmp);
      free (pwd);
      return false;
   }
   free (tmp);

   char *olddir = pushdir ("..");
   if (!olddir) {
      FRM_ERROR ("Failed to switch directory [..]: %m\n");
      free (pwd);
      return false;
   }

   char *path = get_path (frm);
   if (!(history_append (frm->dbpath, path))) {
      FRM_ERROR ("Failed to set the working node to [root]\n");
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
      FRM_ERROR ("Failed to switch to target [%s]\n", target);
      return false;
   }

   char *pwd = get_path (frm);
   if (!(history_append (frm->dbpath, pwd))) {
      FRM_ERROR ("Failed to set the working node to [root]\n");
      free (pwd);
      return false;
   }

   free (pwd);
   return true;
}

bool frm_switch (frm_t *frm, const char *target)
{
   if (!frm || !target || !target[0]) {
      FRM_ERROR ("Error: null objects passed for frm_switching\n");
      return false;
   }

   char *suffixed = target[strlen(target)-1]=='/'
      ? ds_str_dup (target)
      : ds_str_cat (target, "/", NULL);
   if (!suffixed) {
      FRM_ERROR ("OOM error allocating suffixed string\n");
      errno = ENOMEM;
      return false;
   }

   char *actual = history_find (frm->dbpath, suffixed);
   free (suffixed);
   if (!actual) {
      if (!(actual = ds_str_dup (target))) {
         FRM_ERROR ("OOM error allocating target to switch to [%s]\n", target);
         return false;
      }
   }

   if ((chdir (frm->dbpath))!=0) {
      FRM_ERROR ("Failed to switch to dbpath [%s]\n", frm->dbpath);
      free (actual);
      return false;
   }

   if ((chdir (actual))!=0) {
      FRM_ERROR ("Failed to switch to target [%s]\n", actual);
      free (actual);
      return false;
   }

   char *pwd = get_path (frm);
   if (!(history_append (frm->dbpath, pwd))) {
      FRM_ERROR ("Failed to set the working node to [root]\n");
      free (actual);
      free (pwd);
      return false;
   }

   free (actual);
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
      FRM_ERROR ("Failed to switch to dbpath directory [%s]: %m\n", frm->dbpath);
      goto cleanup;
   }

   if (!(infile = fopen ("history", "r"))) {
      FRM_ERROR ("Failed to open [history] for reading: %m\n");
      goto cleanup;
   }

   if (!(line = malloc (line_len))) {
      FRM_ERROR ("OOM error allocating line for reading history\n");
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
      FRM_ERROR ("History file appears to be empty, aborting switch.\n");
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

static void free_str_array (char **array)
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
         FRM_ERROR ("Error: cannot pop a frame that has children\n");
         errno = ENOTEMPTY;
         return false;
      }
   }

   char *oldpath = get_path (frm);
   if (!oldpath) {
      FRM_ERROR ("Error: failed to retrieve current working directory: %m\n");
      return false;
   }

   if (!(frm_up (frm))) {
      FRM_ERROR ("Error: failed to switch to parent node: %m\n");
      free (oldpath);
      return false;
   }

   if (!(frm_delete (frm, oldpath))) {
      FRM_ERROR ("Error: failed to remove path [%s]: %m\n", oldpath);
      free (oldpath);
      return false;
   }

   free (oldpath);
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
      FRM_ERROR ("Error: failed to switch directory: %m\n");
      free_str_array(subframes);
      return false;
   }

   if (!(removedir (target))) {
      FRM_ERROR ("Error: failed to remove directory[%s]: %m\n", target);
      popdir (&olddir);
      free_str_array (subframes);
      return false;
   }

   for (size_t i=0; subframes && subframes[i]; i++) {
      if (!(index_remove (frm->dbpath, subframes[i]))) {
         FRM_ERROR ("Warning: failed to remove [%s] from index\n", subframes[i]);
      }
   }
   free_str_array (subframes);
   if (!(index_remove (frm->dbpath, target))) {
      FRM_ERROR ("Warning: failed to remove [%s] from index\n", target);
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
      FRM_ERROR ("Error: failed to read index\n");
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
            FRM_ERROR ("OOM error allocating match results\n");
            goto cleanup;
         }
         tmp[newsize] = NULL;
         if (!(tmp[results_len] = ds_str_dup (index[i]))) {
            FRM_ERROR ("OOM error allocating match item %zu\n", i);
            goto cleanup;
         }

         results = tmp;
         results_len++;
      }
   }

   // If we reached this point with NULL results then no errors occurred but
   // no matches were found either. Must return an empty list.
   if (!results && !(results = calloc (1, sizeof *results))) {
      FRM_ERROR ("OOM error allocating empty list\n");
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

   // Attempt to switch to 'from' as a relative path. If that fails
   // attempt to switch to 'from' as an absolute nodename (absolute
   // relative to frm->dbpath).
   if (!from || !from[0]) {
      from = "./";
   }
   char *olddir = pushdir (from);
   if (!olddir) {
      FRM_ERROR ("Warning: using relative path [%s] failed, trying absolute path\n",
            from);
      char *tmp = ds_str_cat (frm->dbpath, "/", from, NULL);
      if (!tmp) {
         FRM_ERROR ("OOM error allocating absolute path for nodename [%s/%s]\n",
               frm->dbpath, from);
         return NULL;
      }
      char *first_errmsg = ds_str_dup (strerror (errno));
      if (!first_errmsg) {
         FRM_ERROR ("OOM error allocating error message string: %i\n", errno);
         return NULL;
      }

      olddir = pushdir (tmp);
      free (tmp);
      if (!olddir) {
         FRM_ERROR ("Neither [%s] nor [%s/%s] could be used: [%s][%m]\n",
               from, frm->dbpath, from, first_errmsg);
         free (first_errmsg);
         return NULL;
      }
      free (first_errmsg);
   }

   char *current = frm_current (frm);
   if (!current) {
      FRM_ERROR ("Error: unable to determine current node: %m\n");
      return NULL;
   }

   char *prefixed_current = ds_str_cat (current, "/", NULL);
   if (!prefixed_current) {
      FRM_ERROR ("OOM error allocating temporary string for current node\n");
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
      FRM_ERROR ("Failed to retrieve the current node\n");
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

frm_node_t *frm_node_create (frm_t *frm)
{
   return NULL;
}

void frm_node_free (frm_node_t *rootnode)
{

}


const char *frm_node_name (frm_node_t *node)
{
   return "TODO";
}

uint64_t frm_node_date (frm_node_t *node)
{
   return (uint64_t)-1;
}

const char *frm_node_fpath (frm_node_t *node)
{
   return "TODO:";
}


size_t frm_node_nchildren (frm_node_t *node)
{
   return 0;
}

frm_node_t *frm_node_child (frm_node_t *node, size_t index)
{
   return NULL;
}

frm_node_t *frm_node_parent (void)
{
   return NULL;
}

frm_node_t *frm_node_root (void)
{
   return NULL;
}

frm_node_t *frm_node_find (frm_node_t *node, const char *fpath)
{
   return NULL;
}


