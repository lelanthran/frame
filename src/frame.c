#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "ds_str.h"
#include "frm.h"

// From: https://gist.github.com/lelanthran/4d50105c2d0594b5c15aeaeed72f84c3
/* *****************************************************************
 * A more robust command-line handling function than I normally use.
 * There are only two functions, so this is suitable for copying and
 * pasting into the file containing main().
 *
 * =======================================================================
 * Skip all options on the command line, and set argc/argv to point to the
 * program's non-option arguments:
 *    void cline_skipopt (int *argc, char ***argv);
 *
 * NOTE: the argc/argv parameters are modified by this function.
 *
 *
 *
 * =======================================================================
 * Get an option from the command-line:
 *    const char *cline_getopt (int          argc,
 *                              char       **argv,
 *                              const char  *longopt,
 *                              char         shortopt);
 *
 * argc:       number of arguments* argv:       array of arguments
 * longopt:    string containing name of option. If NULL, short-opt is
 *             used to find the option.
 * shortopt:   character of the option to search for. If zero, longopt is
 *             used to find the option.
 *
 * RETURNS: If option is not found, then NULL is returned. If option is
 *          found then the string that is returned:
 *          1. Will be empty if option did not have an argument
 *             such as "--option=" or "--option".
 *          2. Will contain the value of the argument if the option had an
 *             argument, such as "--option=value", or "-o value", or
 *             "-ovalue".
 *
 *          For short options, the caller must determine whether to use
 *          the returned string's value or not. If, for example, the
 *          option "-c" does not have arguments and the user entered
 *          "-cab" then the caller must only check for nullness in the
 *          return value.
 *
 *          If the caller expects "-c" to have arguments, then the
 *          returned string for "-cab" will contain "ab".
 *
 *          See EXAMPLES below for clarification.
 *
 * EXAMPLES:
 *
 * 1. Get a long option using cline_getopt()
 *    --long-option           Returns empty string ""
 *    --long-option=          Returns empty string ""
 *    --long-option=value     Returns const string "value"
 *
 * 2. Get a short option using cline_getopt()
 *    -a -b -c       Returns non-NULL for a, b and c
 *    -abc           Same as above
 *    -ofoo          Returns "foo" for o
 *    -o foo         Same as above
 *    -abco foo      Returns non-NULL for a, b and c, AND returns foo for o
 *    -abcofoo       Same as above
 *
 * 3. When the same long-option and short-option is specified the
 *    long-option takes precedence.
 *
 * 4. Options processing ends with "--". Any arguments after a "--" is
 *    encountered must be processed by the caller.
 */
static const char *cline_getopt (int argc, char **argv,
                                 const char *longopt,
                                 char shortopt)
{
   for (int i=1; i<argc; i++) {
      if (argv[i][0]!='-')
         continue;

      if ((memcmp (argv[i], "--", 3))==0)
         return NULL;

      char *value = NULL;

      if (argv[i][1]=='-' && longopt) {
         char *name = &argv[i][2];
         if ((memcmp (name, longopt, strlen (longopt)))==0) {
            argv[i][0] = 0;
            value = strchr (name, '=');
            if (!value)
               return "";
            *value++ = 0;
            return value;
         }
      }

      if (!shortopt || argv[i][1]=='-')
         continue;

      for (size_t j=1; argv[i][j]; j++) {
         if (argv[i][j] == shortopt) {
            memmove (&argv[i][j], &argv[i][j+1], strlen (&argv[i][j+1])+1);
            if (argv[i][j] == 0) {
               return argv[i+1] ? argv[i+1] : "";
            } else {
               return &argv[i][j];
            }
         }
      }
   }
   return NULL;
}

static void cline_skipopt (int *argc, char ***argv)
{
   for (int i=1; i<(*argc); i++) {
      if ((memcmp ((*argv)[i], "--", 3))==0) {
         (*argv) = &(*argv)[i+1];
         (*argc) = (*argc) - i - 1;
         return;
      }
   }
}

static const char *cline_find_command (int argc, char **argv)
{
   for (int i=1; i<argc; i++) {
      if (argv[i] && strlen(argv[i]) >= 2 && (memcmp (argv[i], "--", 2))!=0) {
         return argv[i];
      }
   }
   return NULL;
}

int main (int argc, char **argv)
{
   int ret = EXIT_FAILURE;
   const char *dbpath = cline_getopt (argc, argv, "dbpath", 0);
   const char *nhistory = cline_getopt (argc, argv, "history", 0);

   frm_t *frm = NULL;

   if (!dbpath) {
      // TODO: Windows compatibility
      const char *homedir = getenv ("HOME");
      if (!homedir) {
         fprintf (stderr,
               "Missing $HOME in environment and no --dbpath specified");
         goto cleanup;
      }

      dbpath = ds_str_cat (homedir, "/.framedb", NULL);
      if (!dbpath) {
         fprintf (stderr, "OOM error: strcat homedir construction\n");
         goto cleanup;
      }
   } else {
      // So we can free it later
      dbpath = ds_str_dup (dbpath);
   }

   // Done with options, skip to command
   const char *command = cline_find_command (argc, argv);
   printf ("Frame v%s\nCommand [%s]\n", frame_version, command);

   // If user wants only the title, print the title and exit immediately.
   if ((strcmp ("title", argv[1]))==0) {
      size_t history = 1;
      if (nhistory) {
         if ((sscanf (nhistory, "%zu", &history))!=1) {
            fprintf (stderr, "Specified history length of [%s] is invalid\n",
                  nhistory);
            goto cleanup;
         }
      }
      if (history > 100) {
         fprintf (stderr, "History limited to %zu\n", history);
      }

      frm_header_t *header = frm_read_header (dbpath);
      if (!header) {
         fprintf (stderr, "Failed to read [%s]: %m\n", dbpath);
         goto cleanup;
      }
      for (size_t i=0; i<history; i++) {
         printf ("%zu:%s\n", frm_header_id (header, i),
                             frm_header_title (header, i));
      }
      frm_header_del (header);
      ret = EXIT_SUCCESS;
      goto cleanup;
   }

   // If user wants to create a new file, create it and exit
   if ((strcmp ("create", command))==0) {
      if (!(frm = frm_create (dbpath))) {
         fprintf (stderr, "Failed to create [%s]: %m\n", dbpath);
         goto cleanup;
      }
      ret = EXIT_SUCCESS;
      goto cleanup;
   } else {
      if (!(frm = frm_open (dbpath))) {
         fprintf (stderr, "Failed to open [%s]: %m\n", dbpath);
         goto cleanup;
      }
   }

   fprintf (stderr, "Unknown command [%s]\n", command);

cleanup:
   free (dbpath);
   frm_close(frm);
   return ret;
}

