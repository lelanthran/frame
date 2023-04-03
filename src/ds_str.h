
#ifndef H_DS_STR
#define H_DS_STR

#include <stdarg.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

   // Make a copy of the src string, caller must free the result, NULL
   // returned on error.
   char *ds_str_dup (const char *src);

   // Concatenate all the strings given (ending the parameter list with a
   // NULL) into a single string that is returned which the caller must
   // free. NULL is returned on error
   char *ds_str_cat (const char *src, ...);
   char *ds_str_vcat (const char *src, va_list ap);

   // Append all the strings given in '...' (ending with a NULL) to
   // parameter '(*dst)'. Parameter '(*dst)' is reallocated as necessary
   // and therefore must be reallocatable (returned by malloc() or similar).
   // The reallocated '(*dst)' is also returned on success.
   //
   // NULL is returned on error.
   char *ds_str_append (char **dst, const char *s1, ...);
   char *ds_str_vappend (char **dst, const char *s1, va_list ap);

   // Perform a printf into a buffer allocated on demand. The parameter
   // '*dst' is allocated by this function and must be freed by the caller.
   // On success the length of '*dst' is returned, on failure zero is
   // returned.
   size_t ds_str_printf (char **dst, const char *fmt, ...);
   size_t ds_str_vprintf (char **dst, const char *fmt, va_list ap);

   // Trim the leading, the trailing or both the leading and the trailing
   // whitespace from the specified string. The operations are performed
   // in-place on the string provided.
   //
   // A pointer to the string is always returned - no errors are possible.
   // If the input is NULL then the return value is NULL as well.
   char *ds_str_ltrim (char *src);
   char *ds_str_rtrim (char *src);
   char *ds_str_trim (char *src);

   // Perform a character substitution on the source string. The caller
   // must free the returned value. NULL is returned on error.
   //
   // All occurrences of 'oldc' will be replaced with 'newc'. Thereafter
   // every two arguments will be interpreted as a new {oldc,newc} pair
   // until oldc is 0.
   //
   // Note that although oldc and newc are both of type int, they are cast
   // to char before usage.
   char *ds_str_chsubst (const char *src, int oldc, int newc, ...);
   char *ds_str_vchsubst (const char *src, int oldc, int newc, va_list ap);


   // Perform a string substitution on the source string. The caller must
   // free the returned value. NULL is returned on error.
   //
   // All occurrences of 'olds' will be replaced by 'news'. Thereafter,
   // every two arguments will be interpreted as a pair of {olds,news} and
   // substitutions continue until an instance of NULL is read for 'olds'.
   char *ds_str_strsubst (const char *src,
                          const char *olds, const char *news, ...);
   char *ds_str_vstrsubst (const char *src,
                           const char *olds, const char *news, va_list ap);

   // Copy nchars from the string src, starting at position from_position. The
   // caller is responsible for freeing the returned value. If the nchars results
   // in a out of bounds access (i.e. the caller specifies more character to copy
   // than exist in the src) the the copy is limited to the number of characters
   // available in the string.
   //
   // If the from_position is out of bounds, then an empty string is returned and
   // must still be freed by the caller.
   //
   // If there is no memory to allocate the return value then NULL is returned.
   char *ds_str_substring (const char *src, size_t from_position, size_t nchars);

#ifdef __cplusplus
};
#endif

#endif

