
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "ds_str.h"

/* ******************************************************************** */

char *ds_str_dup (const char *src)
{
   if (!src)
      return NULL;

   size_t len = strlen (src) + 1;

   char *ret = malloc (len);
   if (!ret)
      return NULL;

   return strcpy (ret, src);
}

/* ******************************************************************** */

char *ds_str_vcat (const char *src, va_list ap)
{
   bool error = true;
   char *ret = NULL;
   const char *tmp = src;
   size_t nbytes = 0;
   va_list apc;

   va_copy (apc, ap);

   while (tmp) {
      nbytes += strlen (tmp);
      tmp = va_arg (apc, const char *);
   }

   va_end (apc);

   if (!(ret = malloc (nbytes + 1)))
      goto errorexit;

   *ret = 0;

   tmp = src;
   size_t idx = strlen (ret);
   while (tmp) {
      // strcat (ret, tmp);
      size_t tmplen = strlen (tmp);
      memcpy (&ret[idx], tmp, tmplen);
      idx += tmplen;
      tmp = va_arg (ap, const char *);
   }
   ret[idx] = 0;

   error = false;

errorexit:

   if (error) {
      free (ret);
      ret = NULL;
   }

   return ret;
}

char *ds_str_cat (const char *src, ...)
{
   va_list ap;

   va_start (ap, src);
   char *ret = ds_str_vcat (src, ap);
   va_end (ap);

   return ret;
}

char *ds_str_vappend (char **dst, const char *s1, va_list ap)
{
   bool error = true;
   char *ret = NULL;


   if (!(*dst))
      (*dst) = ds_str_dup ("");

   if (!(*dst))
      return NULL;

   if (!(ret = ds_str_cat ((*dst), s1, NULL)))
      goto errorexit;

   size_t idx = strlen (ret);
   while ((s1 = va_arg (ap, char *))!=NULL) {
      // char *tmp = ds_str_cat (ret, s1, NULL);
      // if (!tmp)
      //    goto errorexit;

      size_t s1_len = strlen (s1);
      size_t newlen = idx + s1_len;
      char *tmp = realloc (ret, newlen + 1);
      if (!tmp) {
         free (ret);
         return NULL;
      }
      ret = tmp;
      memcpy (&ret[idx], s1, s1_len);
      idx += s1_len;
   }

   ret[idx] = 0;

   free (*dst);
   (*dst) = ret;

   error = false;

errorexit:

   if (error) {
      free (ret);
      ret = NULL;
   }

   return ret;
}

char *ds_str_append (char **dst, const char *s1, ...)
{
   va_list ap;

   va_start (ap, s1);
   char *ret = ds_str_vappend (dst, s1, ap);
   va_end (ap);

   return ret;
}

/* ******************************************************************** */

size_t ds_str_vprintf (char **dst, const char *fmt, va_list ap)
{
   size_t ret = 0;
   size_t tmprc = 0;
   char *tmp = NULL;
   va_list ac;

   *dst = NULL;

   va_copy (ac, ap);
   int rc = vsnprintf (*dst, ret, fmt, ac);
   va_end (ac);

   ret = rc + 1;

   if (!(tmp = realloc (*dst, ret))) {
      return 0;
   }

   *dst = tmp;
   rc = vsnprintf (*dst, ret, fmt, ap);
   tmprc = rc;
   if (tmprc >= ret) {
      free (*dst);
      *dst = NULL;
      return 0;
   }

   return ret;
}

size_t ds_str_printf (char **dst, const char *fmt, ...)
{
   va_list ap;

   va_start (ap, fmt);
   size_t ret = ds_str_vprintf (dst, fmt, ap);
   va_end (ap);

   return ret;
}

/* ******************************************************************** */

char *ds_str_ltrim (char *src)
{
   if (!src)
      return NULL;

   size_t begin = 0;
   size_t slen = strlen (src);

   if (!slen)
      return src;

   while ((src[begin]) && (isspace (src[begin]))) {
      begin++;
   }
   if (!src[begin]) {
      src[0] = 0;
   }

   slen++;

   memmove (&src[0], &src[begin], slen - begin);
   return src;
}

char *ds_str_rtrim (char *src)
{
   if (!src)
      return NULL;

   size_t slen = strlen (src);
   size_t end = slen - 1;

   if (!slen)
      return src;

   while (end!=0 && isspace (src[end])) {
      src[end--] = 0;
   }
   if (end==0 && isspace (src[0])) {
      src[0] = 0;
   }

   return src;
}

char *ds_str_trim (char *src)
{
   return ds_str_rtrim (ds_str_ltrim (src));
}

/* ******************************************************************** */

char *ds_str_vchsubst (const char *src, int oldc, int newc, va_list ap)
{
   char *ret = ds_str_dup (src);
   if (!ret)
      return NULL;

   while (oldc) {
      char *tmp = ret;
      while ((tmp = strchr (tmp, (char)oldc))) {
         *tmp++ = (char)newc;
      }
      oldc = va_arg (ap, int);
      if (oldc)
         newc = va_arg (ap, int);
   }
   return ret;
}

char *ds_str_chsubst (const char *src, int oldc, int newc, ...)
{
   va_list ap;

   va_start (ap, newc);
   char *ret = ds_str_vchsubst (src, oldc, newc, ap);
   va_end (ap);

   return ret;
}

char *ds_str_strsubst (const char *src,
                       const char *olds, const char *news, ...)
{
   va_list ap;

   va_start (ap, news);
   char *ret = ds_str_vstrsubst (src, olds, news, ap);
   va_end (ap);

   return ret;
}

static char *ds_str_subst1 (char *src, const char *olds, const char *news)
{
   bool error = true;
   char *ret = NULL;

   size_t olds_len = strlen (olds);
   char *tmp = NULL;
   char *startstr = src;

   while ((tmp = strstr (startstr, olds))) {

      *tmp++ = 0;

      if (!(ds_str_append (&ret, startstr, news, NULL))) {
         goto errorexit;
      }

      tmp += olds_len - 1;
      startstr = tmp;
   }

   if (!(ds_str_append (&ret, startstr, NULL))) {
      goto errorexit;
   }

   error = false;

errorexit:
   if (error) {
      free (ret);
      ret = NULL;
   }

   return ret;
}

char *ds_str_vstrsubst (const char *src,
                        const char *olds, const char *news, va_list ap)
{
   bool error = true;
   char *ret = NULL;
   char *tmpsrc = NULL;

   error = false;

   if (!(ret = ds_str_dup (src)))
      goto errorexit;

   while (olds) {

      char *tmp = ds_str_subst1 (ret, olds, news);
      if (!tmp)
         goto errorexit;

      free (ret);
      ret = tmp;

      if ((olds = va_arg (ap, const char *))) {
         if (!(news = va_arg (ap, const char *))) {
            break;
         }
      }
   }

   error = false;

errorexit:

   free (tmpsrc);

   if (error) {
      free (ret);
      ret = NULL;
   }

   return ret;
}

char *ds_str_substring (const char *src, size_t from_position, size_t nchars)
{
   size_t srclen = strlen (src);
   size_t lastchar = nchars + from_position;

   if (lastchar > srclen)
      lastchar = srclen;

   char *ret = calloc ((lastchar - from_position) + 2, 1);
   if (!ret)
      return NULL;

   size_t dstidx = 0;
   for (size_t i=from_position; i<lastchar && i<srclen; i++) {
      ret[dstidx++] = src[i];
   }

   return ret;
}

