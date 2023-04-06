#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>
#include <time.h>
#include <inttypes.h>
#include <string.h>

#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "frm.h"
#include "ds_str.h"

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
   char *eol = NULL;
   for (size_t i=0; i<count; i++) {
      eol = strchr (tmp, '\n');
      if (!eol)
         break;
      tmp = eol;
   }
   if (eol)
      *eol = 0;

   popdir (&pwd);
   return history;
}

static bool history_append (const char *dbpath, const char *path)
{
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
               history, "\n",
               NULL))) {
      FRM_ERROR ("Failed to write file [%s/working_node]: %m\n", dbpath);
      free (history);
      popdir (&pwd);
      return false;
   }

   free (history);
   popdir (&pwd);
   return true;
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

   popdir (&newdir);
   popdir (&pwd);
   return true;
}

/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

struct frm_t {
   char *dbpath;
   char *olddir;
};

static void frm_free (frm_t *frm)
{
   if (!frm)
      return;

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
   return ret;
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

   popdir (&pwd);
   return frm_init (dbpath);
}

frm_t *frm_init (const char *dbpath)
{
   bool error = true;
   frm_t *ret = NULL;

   char *pwd = pushdir (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to switch directory to [%s]: %m\n", dbpath);
      goto cleanup;
   }

   // TODO: replace this with frm_history
   char *history = frm_readfile ("history");
   char *node = NULL;
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

   char *newpath = pushdir (node);
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
   if (!frm)
      return;

   popdir (&frm->olddir);
   free (frm->dbpath);
   free (frm);
}

char *frm_history (frm_t *frm, size_t count)
{
   if (!frm) {
      FRM_ERROR ("Found null object for frm_t\n");
      return ds_str_dup ("");
   }

   return history_read(frm->dbpath, count);
}

char *frm_current (frm_t *frm)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
      return ds_str_dup ("");
   }
   char *tmp = getcwd (NULL, 0);
   if (!tmp) {
      FRM_ERROR ("Failed to get the current working directory\n");
      tmp = ds_str_dup ("");
   }

   char *ret = ds_str_strsubst(tmp, frm->dbpath, "", NULL);
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

static bool read_info (frm_t *frm, struct info_t *dst, char *fname)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
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
      return false;
   }

   char tstring[47];
   if (!(frm_writefile (fname,
               "mtime:", uint64_string (tstring, (uint64_t)time(NULL)), "\n",
               NULL))) {
      FRM_ERROR ("Failed to write info: %m\n");
      return false;
   }

   return true;
}

static bool update_info_mtime (frm_t *frm, const char *fname)
{
   struct info_t info;
   if (!(read_info (frm, &info, "info"))) {
      FRM_ERROR ("Failed to read info file: %m\n");
      return false;
   }

   info.mtime = time (NULL);
   if (!(write_info (frm, &info, "info"))) {
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

bool frm_push (frm_t *frm, const char *name, const char *message)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
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
      return false;
   }

   // TODO: Update the history
   char *pwd = getcwd (NULL, 0);
   if (!(history_append(frm->dbpath, pwd))) {
      FRM_ERROR ("Failed to update history\n");
      free (pwd);
      return false;
   }

   free (olddir);
   free (pwd);
   return true;
}

bool frm_payload_replace (frm_t *frm, const char *message)
{
   if (!frm) {
      FRM_ERROR ("Error, null object passed for frm_t\n");
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
