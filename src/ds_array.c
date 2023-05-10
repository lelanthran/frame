#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <stdint.h>

#include "ds_array.h"

struct ds_array_t {
   size_t nitems;
   void **array;
};

ds_array_t *ds_array_new (void)
{
   ds_array_t *ret = calloc (1, sizeof *ret);
   if (!(ret->array = calloc (1, sizeof *(ret->array)))) {
      free (ret);
      ret = NULL;
   }
   return ret;
}

void ds_array_del (ds_array_t *ll)
{
   if (!ll)
      return;

   free (ll->array);
   free (ll);
}

ds_array_t *ds_array_copy (const ds_array_t *src, size_t from_index, size_t to_index)
{
   size_t nitems;
   ds_array_t *ret = ds_array_new ();
   bool error = true;

   if (!src)
      return NULL;

   nitems = ds_array_length (src);

   for (size_t i=from_index; i>=from_index && i<to_index && i<nitems; i++) {
      if (!(ds_array_ins_tail (ret, src->array[i])))
         goto errorexit;
   }

   error = false;

errorexit:

   if (error) {
      ds_array_del (ret);
      ret = NULL;
   }

   return ret;
}

size_t ds_array_length (const ds_array_t *ll)
{
   if (!ll)
      return 0;

   return ll->nitems;
}

void *ds_array_get (const ds_array_t *ll, size_t i)
{
   if (!ll)
      return NULL;

   if (i >= ll->nitems)
      return NULL;

   return ll->array[i];
}

void ds_array_iterate (const ds_array_t *ll,
                       void (*fptr) (void *, void *), void *param)
{
   if (!ll || !fptr)
      return;

   for (size_t i=0; ll->array[i]; i++) {
      fptr (ll->array[i], param);
   }
}

static bool ds_array_grow (ds_array_t *ll, size_t nelems)
{
   if (!ll)
      return false;

   void **tmp = realloc (ll->array, (sizeof *ll->array) * (ll->nitems + 1 + nelems + 1));
   if (!tmp)
      return false;

   ll->array = tmp;

   memset (&ll->array[ll->nitems], 0, (sizeof *ll->array) * (nelems + 1));
   return true;
}

void ds_array_shrink_to_fit (ds_array_t *ll)
{
   if (!ll)
      return;

   void **tmp = realloc (ll->array, (sizeof *ll->array) * (ll->nitems + 1));
   if (!tmp)
      return;

   ll->array = tmp;
}

void *ds_array_ins_tail (ds_array_t *ll, void *el)
{
   if (!ll || !el)
      return NULL;

   if (!(ds_array_grow (ll, 1)))
      return NULL;

   ll->array[ll->nitems++] = el;

   return el;
}

void *ds_array_ins_head (ds_array_t *ll, void *el)
{
   if (!ll || !el)
      return NULL;

   size_t endpos = ll->nitems;

   if (!(ds_array_grow (ll, 1)))
      return NULL;

   memmove (&ll->array[1], &ll->array[0], (sizeof *(ll->array)) * (endpos + 1));

   ll->array[0] = el;
   ll->nitems++;

   return el;
}

void *ds_array_rm_tail (ds_array_t *ll)
{
   if(!ll || !ll->array[0] || ll->nitems == 0)
      return NULL;

   void *ret = ll->array[ll->nitems - 1];

   ll->nitems--;
   ll->array[ll->nitems] = NULL;

   return ret;
}

void *ds_array_rm_head (ds_array_t *ll)
{
   return ds_array_rm (ll, 0);
}


void *ds_array_rm (ds_array_t *ll, size_t index)
{
   if (!ll || ll->nitems == 0)
      return NULL;

   if (index >= ll->nitems)
      return NULL;

   void *ret = ll->array[index];

   memmove (&ll->array[index], &ll->array[index + 1],
            (sizeof (void *)) * (ll->nitems - index));

   return ret;
}
