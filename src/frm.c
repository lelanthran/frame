#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdarg.h>
#include <time.h>
#include <inttypes.h>

#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "frm.h"
#include "ds_str.h"

/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

static const char *get_unix_time (char dst[47])
{
   uint64_t now = time (NULL);
   snprintf (dst, 46, "%" PRIu64, now);
   return dst;
}

static char *push_cd (const char *newdir)
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

static bool writefile (const char *name, const char *data, ...)
{
   va_list ap;

   FILE *outf = fopen (name, "w");
   if (!outf) {
      FRM_ERROR ("Failed to open [%s] for writing: %m\n", name);
      return false;
   }

   va_start (ap, data);
   while (data) {
      fprintf (outf, "%s", data);
      data = va_arg (ap, const char *);
   }
   fclose (outf);
   va_end (ap);
   return true;
}

static char *readfile (const char *name)
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
   if (nbytes != len) {
      FRM_ERROR ("Read {%zu of %li] bytes in [%s]: %m\n", nbytes, len, name);
      fclose (inf);
      free (ret);
      return NULL;
   }

   ret[len] = 0;
   return ret;
}

static bool history_append (const char *dbpath, const char *path)
{
   char *pwd = push_cd (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to push current working directory\n");
      return false;
   }

   char *history = readfile ("history");
   if (!history) {
      // Ignoring empty history. History is allowed to be empty.
      history = ds_str_dup ("");
      if (!history) {
         FRM_ERROR ("OOM error allocating empty history\n");
         popdir (&pwd);
         return false;
      }
   }

   // TODO: Truncate history

   if (!(writefile ("history",
               path, "\n",
               history, "\n",
               NULL))) {
      FRM_ERROR ("Failed to write file [%s/working_node]\n", dbpath);
      free (history);
      popdir (&pwd);
      return false;
   }

   free (history);
   popdir (&pwd);
   return true;
}

struct frm_nodeinfo_t {
   char *name;
   char *payload;
   char *info;
};

frm_nodeinfo_t *node_read (const char *path, va_list ap)
{
   char *pwd = push_cd (path);
   if (!pwd) {
      FRM_ERROR ("Failed to switch current working directory to [%s]\n", path);
      return NULL;
   }

   const char *next = va_arg (ap, const char *);
   if (next == NULL) {
      frm_nodeinfo_t *ret = calloc (1, sizeof *ret);
      if (!ret) {
         FRM_ERROR ("OOM error: allocating node_t\n");
         popdir (&pwd);
         return NULL;
      }
      ret->name = ds_str_dup (path);
      ret->info = readfile ("info");
      ret->payload = readfile ("payload");

      if (!ret->name || !ret->info || !ret->payload) {
         FRM_ERROR ("Some files for the node were not read\n");
         popdir (&pwd);
         free (ret);
         return NULL;
      }

      return ret;
   }

   popdir (&pwd);
   return node_read (next, ap);
}


static bool node_create (const char *path, const char *name, const char *msg)
{
   char *pwd = push_cd (path);
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

   char *newdir = push_cd (name);
   if (!newdir) {
      FRM_ERROR ("Failed to switch directory [%s]\n", name);
      popdir (&pwd);
      return false;
   }


   char tstring[47];
   if (!(writefile ("info", "mtime: ", get_unix_time(tstring), "\n", NULL))) {
      FRM_ERROR ("Failed to create info file [%s/%s/info]\n", path, name);
      return false;
   }

   if (!(writefile ("payload", msg, "\n", NULL))) {
      FRM_ERROR ("Failed to create payload file [%s/%s/payload]\n", path, name);
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

bool frm_create (const char *dbpath)
{
   static const mode_t mode = 0777;
   if ((mkdir (dbpath, mode))!=0) {
      FRM_ERROR ("Failed to create directory [%s]: %m\n", dbpath);
      return false;
   }

   if (!(node_create (dbpath, "root", "ENTER YOUR NOTES HERE"))) {
      FRM_ERROR ("Failed to create node [%s/root]: %m\n", dbpath);
      return false;
   }

   char *pwd = push_cd (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to store current working directory\n");
      return false;
   }

   bool error = true;

   if (!(history_append (dbpath, "root"))) {
      FRM_ERROR ("Failed to set the working node to [root]\n");
      goto cleanup;
   }

   error = false;

cleanup:
   popdir (&pwd);
   return error;
}

bool frm_init (const char *dbpath)
{
   bool error = true;

   char *pwd = push_cd (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to switch directory to [%s]: %m\n", dbpath);
      goto cleanup;
   }

   char *history = readfile ("history");
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

   char *newpath = push_cd (node);
   if (!node) {
      FRM_ERROR ("Failed to switch to node [%s]\n", node);
      goto cleanup;
   }

   error = false;
cleanup:
   if (error) {
      frm_close (pwd);
   }

   return !error;
}

frm_nodeinfo_t *frm_node_read (const char *dbpath, const char *path, ...)
{
   char *pwd = push_cd (dbpath);
   if (!pwd) {
      FRM_ERROR ("Failed to store existing working directory\n");
      return NULL;
   }

   frm_nodeinfo_t *ret = NULL;

   va_list ap;
   va_start (ap, path);
   ret = node_read (path, ap);
   va_end (ap);
   popdir (&pwd);
   return ret;
}

void frm_node_free (frm_nodeinfo_t *ni)
{
   if (!ni)
      return;

   free (ni->payload);
   free (ni->info);
   free (ni->name);
   free (ni);
}

