
/* ************************************************************************** *
 * Frame  (©2023 Lelanthran Manickum)                                         *
 *                                                                            *
 * This program comes with ABSOLUTELY NO WARRANTY. This is free software      *
 * and you are welcome to redistribute it under certain conditions;  see      *
 * the LICENSE file for details.                                              *
 * ****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>

#include <unistd.h>

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

// All options, separated by 0x1e
static char *g_options = NULL;
static void cline_parse_options (int argc, char **argv)
{
   for (int i=1; i<argc; i++) {
      if (argv[i][0] == '-' && argv[i][1] == '-') {
         char *option = &argv[i][2];
         ds_str_append (&g_options, "\x1e", option, NULL);
      }
   }
   ds_str_append (&g_options, "\x1e", NULL);
}

static char *cline_option_get (const char *name)
{
   char *sterm = ds_str_cat ("\x1e", name, NULL);
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
   char *end = strchr (position, '\x1e');
   if (!end) {
      fprintf (stderr, "Internal error, no option delimiter \\x1e found\n");
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


// All commands, separated by 0x1e
static char *g_commands = NULL;
static void cline_parse_commands (int argc, char **argv)
{
   for (int i=1; i<argc; i++) {
      if (argv[i][0] != '-' && argv[i][1] != '-') {
         ds_str_append (&g_commands, argv[i], "\x1e", NULL);
      }
   }
   ds_str_append (&g_commands, "\x1e", NULL);
}

static char *cline_command_get (size_t index)
{
   char *cmd = g_commands;
   for (size_t i=0; i<index; i++) {
      cmd = strchr (cmd, '\x1e');
      if (!cmd)
         break;
   }
   if (!cmd) {
      fprintf (stderr, "Request for command [%zu] failed\n", index);
      return NULL;
   }

   if (*cmd == '\x1e')
      cmd++;

   char *end = strchr (cmd, '\x1e');
   if (!end) {
      fprintf (stderr, "Internal error, no command delimiter \\x1e found\n");
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

static char edlin[1024 * 1024];
static char *run_editor (const char *default_file_contents)
{
   char *message = NULL;
   char *editor = getenv ("EDITOR");
   if (!editor || !editor[0]) {
      FRM_ERROR ("Warning: no $EDITOR specified.\n");
      printf ("Enter the message, ending with a single period "
               "on a line by itself\n");
      while ((fgets (edlin, sizeof edlin -1, stdin))!=NULL) {
         if (edlin[0] == '.')
            break;
         if (!(ds_str_append (&message, edlin, NULL))) {
            FRM_ERROR ("OOM reading edlin input\n");
            free (message);
            return NULL;
         }
      }
   } else {
      char fname[] = "frame-tmpfile-XXXXXX";
      int fd = mkstemp (fname);
      if (fd < 0) {
         FRM_ERROR ("Failed to create temporary file: %m\n");
         return NULL;
      }
      close (fd);
      if (!default_file_contents) {
         default_file_contents = "Enter a description of this node here";
      }

      if (!(frm_writefile (fname,
                  "PATH: ", default_file_contents,
                  "\n",
                  "\n",
                  "Replace this content with your message.",
                  "\n",
                  "There is no limit on the length of messages\n",
                  NULL))) {
         FRM_ERROR ("Failed to edit temporary file [%s]: %m\n", fname);
         return NULL;
      }
      char *shcmd = ds_str_cat (editor, " ", fname, NULL);
      printf ("Waiting for [%s] to return\n", shcmd);
      int exitcode = system (shcmd);
      free (shcmd);
      if (exitcode != 0) {
         FRM_ERROR ("Editor aborted, aborting: %m\n");
         if ((unlink (fname))!=0) {
            FRM_ERROR ("Error: Failed to remove tmpfile [%s]: %m\n", fname);
         }
         return NULL;
      }
      message = frm_readfile (fname);
      if ((unlink (fname))!=0) {
         FRM_ERROR ("Error: Failed to remove tmpfile [%s]\n", fname);
      }
      if (!message) {
         FRM_ERROR ("Failed to read editor output, aborting\n");
         return NULL;
      }
   }
   return message;
}

static void print_helpmsg (void)
{
   static const char *msg[] = {
"     Frame  (© 2023 Lelanthran Manickum)",
"",
"     This program comes with ABSOLUTELY NO WARRANTY. This is free software",
"     and you are welcome to redistribute it under certain conditions;  see",
"     the LICENSE file for details.",
"",
"frame [options] <command> [options] <subcommand>",
"",
"  Options are of the form '--name', '--name=' and '--name=value'. The first",
"two forms are  for boolean options which are either set or unset and do not",
"require any value. The third form is for options that require a value.",
"",
"  Commands and subcommands are of the form 'command'. Some commands require",
"specific options and/or mandatory subcommands. Commands that require either",
"options or subcommands will be decribed below.",
"",
"  Commands must be one of help, create, history, status, push, replace,",
"append, up, down, switch, pop, delete, list or match.",
"",
"Options:",
"",
"  --help               Print this page and exit.",
"",
"  --dbpath=<path>      Specify the location of the database path. Defaults to",
"                       '$HOME/.framedb' unless overridden by this option.",
"",
"  --message=<string>   Provides the message for any command that requires a",
"                       message (such as 'push', 'replace', etc). If this option",
"                       is not present and the command requires a message, then",
"                       the editor specified with $EDITOR is used. If $EDITOR",
"                       is empty, then a prompt for a message is issued on the",
"                       standard input.",
"",
"  --from-root          Specify that the match command must search for matches",
"                       from the root node. If this option is not present then",
"                       matches are, by default, made only from the current node",
"                       onwards (down the tree).",
"",
"  --invert             Perform an inverted search when matching using a search",
"                       term. By default the match command finds all nodes that",
"                       match the search term provided. Using this flag causes",
"                       the match command to find all nodes that *DON'T* match",
"                       the search term.",
"",
"  --quiet              Suppress all non-functional stdout messages, such as",
"                       the copyright notice.",
"",
"Commands:",
"",
"help",
"  Print this message and exit.",
"",
"create",
"  Create a new frame database. If --dbpath is specified then it is used as the",
"  location of the new database. If it is not then $HOME/.framdb is used instead.",
"",
"history [count]",
"  Display the history of all nodes visited, with a number that can be used",
"  in the 'back' command (see 'back' below). The [count] value specifies how",
"  many items to display. If [count] is omitted it defaults to 10. To list",
"  all items in the history (which may be very large) use '0' as the count.",
"",
"back [number]",
"  Jump to the nth item in the history, as specified by [number]. If [number]",
"  is omitted then '1' is used. The 'history' command helpfully lists a number",
"  next to each element that can be used to determine what number in the",
"  history to jump to. Specifying '0' is pointless.",
"",
"status",
"  Display the status of the current node.",
"",
"push",
"  Create a new node as the child of the current node. If a message is specified",
"  with '--message=<string>' then it will be used as the contents of the new",
"  node. If no message is specified with '--message=<string>' then $EDITOR will",
"  be started to allow the user to enter a message. If $EDITOR is not set, the",
"  user will be prompted for a message.",
"",
"replace",
"  Overwrite the content of the current node with the provided message. See ",
"  option '--message=<string>'.",
"",
"append",
"  Appends the provided message (see option '--message' and command 'push') to",
"  the current node.",
"",
"up",
"  Changes the current node to the parent of the current node.",
"",
"down <name>",
"  Changes the current node to the child node named by 'name'.",
"",
"switch <path>",
"  Changes the current node to the non-child node named by <path>.",
"",
"pop",
"  Deletes the current node and set the current node to the parent of the",
"  deleted node.",
"",
"delete <path>",
"  Deletes the node named by <path>. The current node is not changed.",
"",
"list",
"  Lists all descendents of the current node.",
"",
"match <sterm> [--from-root] [--invert]",
"  Lists the nodes that match the search term <sterm>, starting at the current",
"  node. If '--from-root' is specified then the search is performed from the",
"  root node and not the current node. If --invert is specified, then the search",
"  is performed for all those nodes *NOT MATCHING* the search term <sterm>.",
"",
NULL,
   };
   for (size_t i=0; msg[i]; i++) {
      printf ("%s\n", msg[i]);
   }
   printf ("\n");
}

static void status (frm_t *frm)
{
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

static void current (frm_t *frm)
{
   char *current = frm_current (frm);
   char *mtime = frm_date_str (frm);

   printf ("%s: %s\n", current, mtime);
   free (current);
   free (mtime);
}

int main (int argc, char **argv)
{
   int ret = EXIT_SUCCESS;
   cline_parse_options (argc, argv);
   cline_parse_commands (argc, argv);

   // TODO: At some point maybe verify that the options specified are applicable
   // to the command.
   char *command = cline_command_get (0);
   char *help = cline_option_get ("help");
   char *dbpath = cline_option_get ("dbpath");
   char *message = cline_option_get ("message");
   char *from_root = cline_option_get ("from-root");
   char *invert = cline_option_get ("invert");
   char *quiet = cline_option_get ("quiet");
   frm_t *frm = NULL;

   if (!command || !command[0] || (strcmp (command, "help")==0) || help) {
      print_helpmsg ();
      ret = EXIT_FAILURE;
      goto cleanup;
   }

   // TODO: have a more nuanced determination of when the copyright
   // notice should be printed.
   if (quiet==NULL) {
      printf ("Frame %s, (© 2023 Lelanthran Manickum)\n", frame_version);
   }

   if (!dbpath) {
      // TODO: Windows compatibility
      const char *home = getenv("HOME");
      if (!home || !home[0]) {
         fprintf (stderr, "No --dbpath specified and $HOME is not set\n");
         ret = EXIT_FAILURE;
         goto cleanup;
      }
      dbpath = ds_str_cat (home, "/.framedb", NULL);
      if (!dbpath) {
         fprintf (stderr, "OOM error copying $HOME\n");
         ret = EXIT_FAILURE;
         goto cleanup;
      }
   }

   // Check for each command in turn. Could be done in an array, but I don't care
   // enough to do it.
   if ((strcmp (command, "create"))==0) {
      if ((frm = frm_create (dbpath))) {
         printf ("Created framedb at [%s]\n", dbpath);
         ret = EXIT_SUCCESS;
      } else {
         fprintf (stderr, "Failed to create framedb at [%s]: %m\n", dbpath);
         ret = EXIT_FAILURE;
      }
      goto cleanup;
   }

   if (!(frm = frm_init (dbpath))) {
      fprintf (stderr, "Failed to load db from [%s]\n", dbpath);
      ret = EXIT_FAILURE;
      goto cleanup;
   }

   if ((strcmp (command, "history"))==0) {
      char *subcmd = cline_command_get (1);
      size_t count = 10;
      if (subcmd && subcmd[0]) {
         if (((sscanf (subcmd, "%zu", &count)))!=1) {
            if (!quiet) {
               fprintf (stderr, "Specified count of [%s] is invalid\n", subcmd);
               fprintf (stderr, "Using default of 10 for history count\n");
            }
            count = 10;
         }
      } else {
         if (!quiet) {
            fprintf (stderr, "No count specified, listing last 10 items\n");
         }
      }
      free (subcmd);

      if (count == 0)
         count = (size_t)-1;

      char *history = frm_history (frm, count);
      char *sptr = NULL;
      char *tok = strtok_r (history, "\n", &sptr);
      size_t i=0;
      printf ("Frame history\n");
      char indicator = '*';
      do {
         printf ("%c  %5zu: %s\n", indicator, i++, tok);
         indicator = ' ';
      } while ((tok = strtok_r (NULL, "\n", &sptr)));
      printf ("\n");
      free (history);
      goto cleanup;
   }

   if ((strcmp (command, "status"))==0) {
      status (frm);
      goto cleanup;
   }

   if ((strcmp (command, "current"))==0) {
      current (frm);
      goto cleanup;
   }

   if ((strcmp (command, "push"))==0) {
      char *name = cline_command_get(1);
      if (!name || !name[0]) {
         fprintf (stderr, "Must specify a name for the new frame being pushed\n");
         free (name);
         ret = EXIT_FAILURE;
         goto cleanup;
      }
      char *message = cline_option_get ("message");
      if (!message) {
         char *current = frm_current (frm);
         if (!current) {
            fprintf (stderr, "Warning: Failed to get the current path\n");
         }
         char *fpath = ds_str_cat (current, "/", name, NULL);
         message = run_editor (fpath);
         free (fpath);
         free (current);
      }
      if (!message) {
         fprintf (stderr, "No edit message, aborting\n");
         free (name);
         ret = EXIT_FAILURE;
         goto cleanup;
      }

      if (!(frm_push (frm, name, message))) {
         fprintf (stderr, "Failed to create new frame\n");
         free (name);
         free (message);
         ret = EXIT_FAILURE;
         goto cleanup;
      }
      free (name);
      free (message);
      name = frm_current (frm);
      printf ("Created new frame [%s]\n", name);
      free (name);
      goto cleanup;
   }

   if ((strcmp (command, "replace"))==0) {
      char *message = cline_option_get ("message");
      if (!message) {
         message = run_editor (NULL);
      }
      if (!message) {
         fprintf (stderr, "No edit message, aborting\n");
         ret = EXIT_FAILURE;
         goto cleanup;
      }

      if (!(frm_payload_replace (frm, message))) {
         fprintf (stderr, "Failed to replace message of current node: %m\n");
         ret = EXIT_FAILURE;
      }
      free (message);
      goto cleanup;
   }

   if ((strcmp (command, "edit"))==0) {
      const char *editor = getenv ("EDITOR");
      if (!editor || !editor[0]) {
         fprintf (stderr, "No editor specified in $EDITOR\n");
         ret = EXIT_FAILURE;
         goto cleanup;
      }
      char *fname = frm_payload_fname (frm);
      if (!fname) {
         fprintf (stderr, "Failed to retrieve filename of current node: %m\n");
         ret = EXIT_FAILURE;
         goto cleanup;
      }

      char *shcmd = ds_str_cat (editor, " '", fname, "'", NULL);
      if (!shcmd) {
         fprintf (stderr, "OOM error allocating shell command for editor [%s]\n",
               editor);
         free (fname);
         ret = EXIT_FAILURE;
         goto cleanup;
      }

      ret = EXIT_SUCCESS;
      if ((system (shcmd))!=0) {
         fprintf (stderr, "Failed to execute shell command [%s]: %m\n", shcmd);
         ret = EXIT_FAILURE;
      }

      free (fname);
      free (shcmd);
      current (frm);
      goto cleanup;
   }

   if ((strcmp (command, "append"))==0) {
      char *message = cline_option_get ("message");
      if (!message) {
         message = run_editor (NULL);
      }
      if (!message) {
         fprintf (stderr, "No edit message, aborting\n");
         ret = EXIT_FAILURE;
         goto cleanup;
      }

      if (!(frm_payload_append (frm, message))) {
         fprintf (stderr, "Failed to append message to current node: %m\n");
         ret = EXIT_FAILURE;
      }
      free (message);
      current (frm);
      goto cleanup;
   }

   if ((strcmp (command, "up"))==0) {
      if (!(frm_up (frm))) {
         fprintf (stderr, "Failed to move a node up the tree\n");
         ret = EXIT_FAILURE;
      }
      status (frm);
      goto cleanup;
   }

   if ((strcmp (command, "down"))==0) {
      char *target = cline_command_get(1);
      if (!target || !target[0]) {
         fprintf (stderr, "Must specify name of child node to switch to\n");
         free (target);
         ret = EXIT_FAILURE;
         goto cleanup;
      }
      if (!(frm_down (frm, target))) {
         fprintf (stderr, "Failed to switch to node [%s]\n", target);
         ret = EXIT_FAILURE;
      }
      free (target);
      status (frm);
      goto cleanup;
   }

   if ((strcmp (command, "switch"))==0) {
      char *target = cline_command_get(1);
      if (!target || !target[0]) {
         fprintf (stderr, "Must specify an absolute path to switch to\n");
         free (target);
         ret = EXIT_FAILURE;
         goto cleanup;
      }
      if (!(frm_switch (frm, target))) {
         fprintf (stderr, "Failed to switch to node [%s]\n", target);
         free (target);
         ret = EXIT_FAILURE;
         goto cleanup;
      }
      free (target);
      status (frm);
      goto cleanup;
   }

   if ((strcmp (command, "back"))==0) {
      char *subcommand = cline_command_get (1);
      if (!subcommand || !subcommand[0]) {
         subcommand = ds_str_dup ("1");
         if (!subcommand) {
            fprintf (stderr, "OOM error allocating default parameter for 'back'\n");
            ret = EXIT_FAILURE;
            goto cleanup;
         }
      }

      size_t index = 0;
      if ((sscanf (subcommand, "%zu", &index))!=1) {
         fprintf (stderr, "Invalid number: [%s]\n", subcommand);
         free (subcommand);
         ret = EXIT_FAILURE;
         goto cleanup;
      }

      if (!(frm_back (frm, index))) {
         fprintf (stderr, "Failed to switch to history item %zu\n", index);
         ret = EXIT_FAILURE;
      }
      free (subcommand);
      status (frm);
      goto cleanup;
   }


   if ((strcmp (command, "pop"))==0) {
      if (!(frm_pop (frm))) {
         fprintf (stderr, "Failed to pop current node: %m\n");
         ret = EXIT_FAILURE;
      }
      status (frm);
      goto cleanup;
   }

   if ((strcmp (command, "delete"))==0) {
      char *target = cline_command_get (1);
      if (!target || !target[0]) {
         fprintf (stderr, "Must specify a target node to delete\n");
         ret = EXIT_FAILURE;
      }

      if (!(frm_delete (frm, target))) {
         fprintf (stderr, "Failed to delete current node: %m\n");
         ret = EXIT_FAILURE;
      }
      free (target);
      status (frm);
      goto cleanup;
   }

   if ((strcmp (command, "list"))==0) {
      char *from = cline_command_get(1);

      char **results = frm_list (frm, from[0] ? from : NULL);
      free (from);

      if (!results) {
         fprintf (stderr, "Internal error during listing\n");
         ret = EXIT_FAILURE;
         goto cleanup;
      }
      for (size_t i=0; results[i]; i++) {
         printf ("   %s\n", results[i]);
         free (results[i]);
      }
      free (results);
      goto cleanup;
   }

   if ((strcmp (command, "match"))==0) {
      char *sterm = cline_command_get (1);
      if (!sterm || !sterm[0]) {
         if (!quiet) {
            fprintf (stderr, "No search term specified, returning everything\n");
         }
         free (sterm);
         if (!(sterm = ds_str_dup (""))) {
            fprintf (stderr, "OOM error allocating search term\n");
            ret = EXIT_FAILURE;
            goto cleanup;
         }
      }

      char **results = NULL;
      uint32_t flags = 0;
      if (invert) {
         flags |= FRM_MATCH_INVERT;
      }

      if (!from_root) {
         results = frm_match (frm, sterm, flags);
      } else {
         results = frm_match_from_root (frm, sterm, flags);
      }

      if (!results) {
         fprintf (stderr, "Internal error searching framedb\n");
         ret = EXIT_FAILURE;
      } else {
         for (size_t i=0; results[i]; i++) {
            printf ("   %s\n", results[i]);
            free (results[i]);
         }
         free (results);
      }

      free (sterm);
      goto cleanup;
   }

   // The default, with no arguments, is to print out the help message.
   // If we got to this point we have a command but it is unrecognised.
   fprintf (stderr, "Unrecognised command [%s]\n", command);
   ret = EXIT_FAILURE;

cleanup:
   frm_close (frm);
   free (command);
   free (help);
   free (dbpath);
   free (message);
   free (from_root);
   free (invert);
   free (quiet);

   free (g_options);
   free (g_commands);
   return ret;
}

