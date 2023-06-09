
/* ************************************************************************** *
 * Frame  (©2023 Lelanthran Manickum)                                         *
 *                                                                            *
 * This program comes with ABSOLUTELY NO WARRANTY. This is free software      *
 * and you are welcome to redistribute it under certain conditions;  see      *
 * the LICENSE file for details.                                              *
 * ****************************************************************************/


#ifndef H_FRM
#define H_FRM

#define FRM_ERROR(...)     do {\
   fprintf (stderr, "[%s:%i] ", __FILE__, __LINE__);\
   fprintf (stderr, __VA_ARGS__);\
} while (0)

#define FRM_MATCH_INVERT        (0x01 << 0)

typedef struct frm_t frm_t;
typedef struct frm_node_t frm_node_t;


#ifdef __cplusplus
extern "C" {
#endif

   /* Some memory functions because FreePascal has poor support for
    * freeing memory allocated by libraries.
    */
   void frm_mem_free (void *ptr);
   void frm_strarray_free (char **array);

   /* A few utility functions: simple ways to read and write entire
    * files.
    */
   char *frm_readfile (const char *fname);
   bool frm_vwritefile (const char *fname, const char *data, va_list ap);
   bool frm_writefile (const char *fname, const char *data, ...);

   /* Create a new framedb, initialise an existing one and close the
    * handle to the framedb.
    */
   frm_t *frm_create (const char *dbpath);
   frm_t *frm_init (const char *dbpath);
   void frm_close (frm_t *frm);

   /* Retrieve information: history, current frame name, current
    * frame payload, current frame date (in two formats).
    */
   char *frm_history (frm_t *frm, size_t count);
   char *frm_current (frm_t *frm);
   char *frm_payload (void);
   uint64_t frm_date_epoch (void);
   char *frm_date_str (void);

   /* Add/create information: new frame (new creates a new one and then
    * returns, push creates a new one and switches to it), replace the
    * payload, append to payload and return the payload filename.
    */
   bool frm_new (frm_t *frm, const char *name, const char *message);
   bool frm_push (frm_t *frm, const char *name, const char *message);
   bool frm_payload_replace (const char *message);
   bool frm_payload_append (const char *message);
   char *frm_payload_fname (void);

   /* Navigational functions, including deletion when popping.
    */
   bool frm_top (frm_t *frm);
   bool frm_up (frm_t *frm);
   bool frm_down (frm_t *frm, const char *target);
   bool frm_switch (frm_t *frm, const char *target);
   bool frm_switch_direct (frm_t *frm, const char *target);
   bool frm_back (frm_t *frm, size_t index);
   bool frm_delete (frm_t *frm, const char *target);
   bool frm_pop (frm_t *frm, bool force);
   bool frm_rename (frm_t *frm, const char *newname);

   /* Search/listing functions.
    */
   char **frm_list (frm_t *frm, const char *from);
   char **frm_match (frm_t *frm, const char *sterm, uint32_t flags);
   char **frm_match_from_root (frm_t *frm, const char *sterm, uint32_t flags);

   /* Tree functions. All the other frame functions are designed to
    * return one of the following:
    *    1. A single value (e.g. frm_current()).
    *    2. A list of values (e.g. frm_history()).
    *
    * Such return values are easy for the caller to consume even if the
    * caller is not a C program. The functions below return or operate
    * on a navigatable tree. This is necessary when the caller wants to
    * present a tree view of the frame tree structure.
    */

   /* Create and destroy the tree of nodes. Creation always returns
    * the tree starting at the root node. Freeing anything but the root
    * node is gauranteed to leak memory.
    */
   frm_node_t *frm_node_create (frm_t *frm);
   void frm_node_free (frm_node_t *rootnode);

   /* Get the tree name, date and full path. Full path is useful to
    * directly navigate to a particular node.
    */
   const char *frm_node_name (const frm_node_t *node);
   uint64_t frm_node_date (const frm_node_t *node);
   char *frm_node_fpath (const frm_node_t *node);

   /* Functions necessary for recursing. Count the number of children,
    * return child node as specifed by the index, return parent node
    * if any, return root node of the tree, return the node identified
    * by full path.
    */
   size_t frm_node_nchildren (const frm_node_t *node);
   const frm_node_t *frm_node_child (const frm_node_t *node, size_t index);
   const frm_node_t *frm_node_parent (const frm_node_t *node);
   const frm_node_t *frm_node_root (const frm_node_t *node);
   const frm_node_t *frm_node_find (const frm_node_t *node, const char *fpath);

   /* These functions are not very useful to the caller and should be avoided
    * in favour of the functions above.
    */
   char *frm_switch_path (frm_t *frm, const char *from);

#ifdef __cplusplus
};
#endif


#endif


