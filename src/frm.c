#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <stdint.h>

#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>

#include "frm.h"
#include "ds_str.h"

/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

struct header_node_record_t {
   // Node title, will be truncated to 31 bytes + nul terminator
   char title[32];

   // Network byte order, converted into native endianess by the reader
   // and back to network endianess by the writer.
   uint32_t id;
};

struct frm_header_t {
   // Network byte order, converted into native endianess by the reader
   // and back to network endianess by the writer.
   uint32_t format_version;

   // The history is limited to the last 100 nodes visited.
   struct header_node_record_t history[100];
};

static uint32_t byte_swap (uint32_t src)
{
   static const uint32_t bom = 0xf1f2f3f4;
   static const uint8_t *ptr = (uint8_t *)&bom;

   if (ptr[0] == 0xf1)
      return src;

   uint32_t tmp = 0;
   tmp = tmp | ((src & 0xff000000) >> 24);
   tmp = tmp | ((src & 0x00ff0000) >> 8);
   tmp = tmp | ((src & 0x0000ff00) << 8);
   tmp = tmp | ((src & 0x000000ff) << 24);
   return tmp;
}

static frm_header_t *header_read (FILE *inf)
{
   struct frm_header_t *ret = malloc (sizeof *ret);
   if (!ret)
      return NULL;

   if ((fread (ret, sizeof *ret, 1, inf)) != 1) {
      free (ret);
      return NULL;
   }

   ret->format_version = byte_swap (ret->format_version);

   for (size_t i=0; i<sizeof ret->history / sizeof ret->history[0]; i++) {
      ret->history[i].id = byte_swap(ret->history[i].id);
   }

   return ret;
}

static bool header_write (frm_header_t *h, FILE *outf)
{
   if (!h || !outf)
      return false;

   h->format_version = byte_swap (h->format_version);

   for (size_t i=0; i<sizeof h->history / sizeof h->history[0]; i++) {
      h->history[i].id = byte_swap(h->history[i].id);
   }

   if ((fwrite (h, sizeof *h, 1, outf)) != 1)
      return false;

   return true;
}


static void header_del (frm_header_t *header)
{
   if (!header)
      return;

   free (header);
}


frm_header_t *frm_read_header (const char *dbpath)
{
   char *header_path = ds_str_cat (dbpath, "/header", NULL);
   if (!header_path)
      return NULL;

   FILE *inf = fopen (header_path, "rb");
   free (header_path);

   if (!inf)
      return NULL;

   frm_header_t *header = header_read (inf);

   fclose (inf);
   return header;
}

void frm_header_del (frm_header_t *header)
{
   header_del (header);
}

size_t frm_header_id (frm_header_t *header, size_t index)
{
   if (index >= sizeof header->history / sizeof header->history[0])
      return 0;

   return header ? header->history[index].id : 0;
}

const char *frm_header_title (frm_header_t *header, size_t index)
{
   if (index >= sizeof header->history / sizeof header->history[0])
      return "";

   return header ? header->history[index].title : "";
}

/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

struct node_t {
   uint32_t id;
   char *title;
   char *content;
   char *fpath;
   struct node_t *parent;
   // ds_array_t *children;
};


/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

struct frm_t {
   frm_header_t *header;
   struct node_t *root_node;
};

frm_t *frm_create (const char *dbpath)
{
   static const mode_t mode = 0777;

   if ((mkdir (dbpath, mode))!=0)
      return NULL;

   /* ********************* */
   bool error = true;
   frm_t *ret = NULL;
   char *header_path = ds_str_cat (dbpath, "/header", NULL);
   if (!header_path)
      goto cleanup;

   FILE *outf = fopen (header_path, "wb");
   free (header_path);

   if (!outf)
      goto cleanup;

   if (!(ret = calloc (1, sizeof *ret)))
      goto cleanup;

   if (!(ret->header = calloc (1, sizeof *ret->header)))
      goto cleanup;

   ret->header->format_version = 1;
   error = header_write(ret->header, outf) ? false : true;

cleanup:
   if (error) {
      frm_close (ret);
      ret = NULL;
   }

   if (outf) {
      fclose (outf);
   }

   return ret;
}

frm_t *frm_open (const char *dbpath)
{
   bool error = true;
   frm_t *ret = NULL;
   char *header_path = NULL;

   if (!(ret = calloc (1, sizeof *ret)))
      goto cleanup;

   header_path = ds_str_cat (dbpath, "/header", NULL);
   if (!header_path)
      goto cleanup;

   FILE *inf = fopen (header_path, "rb");
   if (!inf)
      goto cleanup;

   if (!(ret->header = header_read (inf)))
      goto cleanup;

   // TODO: Read the root node
   error = false;

cleanup:
   free (header_path);

   if (inf) {
      fclose (inf);
   }

   if (error) {
      frm_close (ret);
      ret = NULL;
   }

   return ret;
}

void frm_close (frm_t *frm)
{
   if (!frm)
      return;

   header_del (frm->header);
   free (frm);
}
