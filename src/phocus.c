#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

#include "ds_str.h"
#include "phoc.h"

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

int main (int argc, char **argv)
{
   int ret = EXIT_FAILURE;
   const char *dbfile = cline_getopt (argc, argv, "dbfile", 0);
   const char *create = cline_getopt (argc, argv, "create", 0);
   const char *title = cline_getopt (argc, argv, "title", 0);

   if (!dbfile[0]) {
      fprintf (stderr, "No file specified with --dbfile\n");
      return EXIT_FAILURE;
   }

   if (!dbfile) {
      // TODO: Windows compatibility
      const char *homedir = getenv ("HOME");
      if (!homedir) {
         fprintf (stderr,
               "Missing $HOME in environment and no --dbfile specified");
         return EXIT_FAILURE;
      }

      dbfile = ds_str_cat (homedir, "/.phocusdb", NULL);
      if (!dbfile) {
         fprintf (stderr, "OOM error: strcat homedir construction\n");
         return EXIT_FAILURE;
      }
   }

   // If user wants only the title, print the title and exit immediately.
   if (title) {
      phocus_header_t *header = phocus_read_header (dbfile);
      if (!header) {
         fprintf (stderr, "Failed to read [%s]: %m\n", dbfile);
         return EXIT_FAILURE;
      }
      printf ("%zu:%s\n",
            phocus_header_id (header), phocus_header_title (header));
      phocus_header_del (header);
      return EXIT_SUCCESS;
   }

   phocus_t *phocus = NULL;

   if (create) {
      if (!(phocus = phocus_create (dbfile))) {
         fprintf (stderr, "Failed to create [%s]: %m\n", dbfile);
         return EXIT_FAILURE;
      }
   } else {
      if (!(phocus = phocus_open (dbfile))) {
         fprintf (stderr, "Failed to open [%s]: %m\n", dbfile);
         return EXIT_FAILURE;
      }
   }

   // Done with options, skip to command
   cline_skipopt (&argc, &argv);

   printf ("Phocus v%s\nCommand [%s]\n", phocus_version, argv[1]);
   return ret;
}

