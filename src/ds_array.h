
#ifndef H_DS_LL
#define H_DS_LL

#include <stdlib.h>

typedef struct ds_array_t ds_array_t;

// This array stores pointers to objects that must be allocated and freed by the caller.
// Removing an entry from the array does not free the object stored by the caller. The caller
// must free all objects that they have allocated.
#ifdef __cplusplus
extern "C" {
#endif

   //  TODO: Change 'remove' to 'rm'
   ds_array_t *ds_array_new (void);
   void ds_array_del (ds_array_t *ll);
   ds_array_t *ds_array_copy (const ds_array_t *src, size_t from_index, size_t to_index);

   size_t ds_array_length (const ds_array_t *ll);
   void *ds_array_get (const ds_array_t *ll, size_t i);
   void ds_array_iterate (const ds_array_t *ll,
                          void (*fptr) (void *, void *), void *param);

   void *ds_array_ins_tail (ds_array_t *ll, void *el);
   void *ds_array_ins_head (ds_array_t *ll, void *el);

   void *ds_array_rm_tail (ds_array_t *ll);
   void *ds_array_rm_head (ds_array_t *ll);

   void *ds_array_rm (ds_array_t *ll, size_t index);

   void ds_array_shrink_to_fit (ds_array_t *ll);

#ifdef __cplusplus
};
#endif

#endif


