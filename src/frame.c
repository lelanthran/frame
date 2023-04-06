#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#include "ds_str.h"
#include "frm.h"

/* **********************************************************
 * Command line handling:
 *    prog [options] <command> [options] <subcommand> ...
 *
 * Essentially, options can come before commands, after commands
 * or both before and after commands, and multiple subcommands
 * may be present, with options once again appearing anywhere.
 */

// All options, separated by 0xfe
static char *g_options = NULL;
static void cline_parse_options (int argc, char **argv)
{
   for (int i=1; i<argc; i++) {
      if (argv[i][0] == '-' && argv[i][1] == '-') {
         char *option = &argv[i][2];
         ds_str_append (&g_options, "\xfe", option, NULL);
      }
   }
   ds_str_append (&g_options, "\xfe", NULL);
}

static char *cline_option_get (const char *name)
{
   char *sterm = ds_str_cat ("\xfe", name, NULL);
   if (!sterm) {
      fprintf (stderr, "OOM error creating option search term\n");
      return NULL;
   }
   char *position = strstr (g_options, sterm);
   // Option not found
   if (!position) {
      free (sterm);
      return NULL;
   }

   position += strlen (sterm);

   // Option found, no value found
   if (*position != '=') {
      free (sterm);
      return ds_str_dup ("");
   }

   // Option found, value found
   position++;
   char *end = strchr (position, '\xfe');
   if (!end) {
      fprintf (stderr, "Internal error, no option delimiter xfe found\n");
      free (sterm);
      return NULL;
   }

   size_t val_len = (end - position) + 1;
   char *value = calloc (val_len, 1);
   if (!value) {
      fprintf (stderr, "OOM error creating option value\n");
      free (sterm);
      return NULL;
   }

   memcpy (value, position, val_len - 1);
   free (sterm);
   return value;
}


// All commands, separated by 0xfe
static char *g_commands = NULL;
static void cline_parse_commands (int argc, char **argv)
{
   for (int i=1; i<argc; i++) {
      if (argv[i][0] != '-' && argv[i][1] != '-') {
         ds_str_append (&g_commands, argv[i], "\xfe", NULL);
      }
   }
   ds_str_append (&g_commands, "\xfe", NULL);
}

static char *cline_command_get (size_t index)
{
   char *cmd = g_commands;
   for (size_t i=0; i<index; i++) {
      cmd = strchr (cmd, '\xfe');
      if (!cmd)
         break;
   }
   if (!cmd) {
      fprintf (stderr, "Request for command [%zu] failed\n", index);
      return NULL;
   }

   char *end = strchr (cmd, '\xfe');
   if (!end) {
      fprintf (stderr, "Internal error, no command delimiter xfe found\n");
      return NULL;
   }

   size_t cmd_len = (end - cmd) + 1;
   char *ret = calloc (cmd_len, 1);
   if (!ret) {
      fprintf (stderr, "OOM error returning command\n");
      return NULL;
   }

   memcpy (ret, cmd, cmd_len - 1);
   return ret;
}


int main (int argc, char **argv)
{
   int ret = EXIT_FAILURE;
   cline_parse_options (argc, argv);
   cline_parse_commands (argc, argv);

   // TODO: At some point maybe verify that the options specified are applicable
   // to the command.
   char *command = cline_command_get (0);
   char *help = cline_option_get ("help");
   char *dbpath = cline_option_get ("dbpath");
   char *message = cline_option_get ("message");
   char *frm = NULL;

   if (!dbpath) {
      // TODO: Windows compatibility
      const char *home = getenv("HOME");
      if (!home || !home[0]) {
         fprintf (stderr, "No --dbpath specified and $HOME is not set\n");
         goto cleanup;
      }
      dbpath = ds_str_cat (home, "/.framedb", NULL);
      if (!dbpath) {
         fprintf (stderr, "OOM error copying $HOME\n");
         goto cleanup;
      }
   }

   // Check for each command in turn. Could be done in an array, but I don't care
   // enough to do it.
   if ((strcmp (command, "init"))==0) {
      if (frm_create (dbpath)!=true) {
         printf ("Initialised framedb at [%s]\n", dbpath);
         ret = EXIT_SUCCESS;
      } else {
         fprintf (stderr, "Failed to initialise framedb at [%s]: %m\n", dbpath);
         ret = EXIT_FAILURE;
      }
      goto cleanup;
   }

   if (!(frm = frm_init (dbpath))) {
      fprintf (stderr, "Failed to load db from [%s]\n", dbpath);
      goto cleanup;
   }

   if ((strcmp (command, "history"))==0) {
      char *history = frm_history (frm, 10);
      char *sptr = NULL;
      char *tok = strtok_r (history, "\n", &sptr);
      size_t i=0;
      printf ("Frame history\n");
      char indicator = '*';
      do {
         printf ("%c  %5zu: %s\n", indicator, i, tok);
         indicator = ' ';
      } while ((tok = strtok_r (NULL, "\n", &sptr)));
      printf ("\n");
      free (history);
   }

   if ((strcmp (command, "status"))==0) {
      char *current = frm_current (frm);
      char *payload = frm_payload (frm);
      char *mtime = frm_date_str (frm);

      printf ("Current frame\n   %s\n", current);
      printf ("\nNotes (%s)\n", mtime);
      char *sptr = NULL;
      char *tok = strtok_r (payload, "\n", &sptr);
      do {
         printf ("   %s\n", tok);
      } while ((tok = strtok_r (NULL, "\n", &sptr)));
      printf ("\n");
      free (current);
      free (mtime);
      free (payload);
   }

   ret = EXIT_SUCCESS;

cleanup:
   frm_close (frm);
   free (command);
   free (help);
   free (dbpath);
   free (message);
   free (g_options);
   free (g_commands);
   return ret;
}

