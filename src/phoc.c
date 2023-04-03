#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>

#include <sys/stat.h>
#include <fcntl.h>

#include "phoc.h"
#include "ds_array.h"

/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

struct phocus_header_t {
   size_t fileformat;
   char *current_node_title;
   size_t current_node_id;

   char **history;
   size_t history_length;
};

static phocus_header_t *header_read (FILE *inf)
{
   return NULL;
}

static void header_del (phocus_header_t *header)
{
   if (!header)
      return;

   free (header->current_node_title);
   for (size_t i=0; i<header->history_length; i++) {
      free (header->history[i]);
   }
   free (header->history);
   free (header);
}


phocus_header_t *phocus_read_header (const char *fname)
{
   FILE *inf = fopen (fname, "rb");
   if (!inf) {
      return NULL;
   }

   phocus_header_t *header = header_read (inf);

   fclose (inf);
   return header;
}

void phocus_header_del (phocus_header_t *header)
{
   header_del (header);
}

size_t phocus_header_id (phocus_header_t *header)
{
   return header ? header->current_node_id : 0;
}

const char *phocus_header_title (phocus_header_t *header)
{
   return header ? header->current_node_id : "";
}


/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

struct node_t {
   char *title;
   char *content;
   struct node_t *parent;
   ds_array_t *children;
};


/* ********************************************************** */
/* ********************************************************** */
/* ********************************************************** */

struct phocus_t {
   phocus_header_t *header;
   struct node_t *root_node;
};

phocus_t *phocus_create (const char *fname)
{
   int flags = O_WRONLY | O_CREAT | O_EXCL;
   mode_t mode = S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH;
   int fd = open (fname, flags, mode);

   if (fd < 0) {
      return NULL;
   }

   /* ********************* */
   bool error = true;
   phocus_t *ret = NULL;

   FILE *inf = fdopen (fd, "wb");
   if (!inf) {
      goto cleanup;
   }

   error = false;

cleanup:
   if (error) {
      phocus_close (ret);
      ret = NULL;
   }

   if (inf) {
      fclose (inf);
   }
   if (fd > 0) {
      close (fd);
   }

   return ret;
}

phocus_t *phocus_open (const char *fname)
{
   return NULL;
}

bool phocus_close (phocus_t *phocus)
{
}
