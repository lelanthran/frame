
#ifndef H_FRM
#define H_FRM

#define FRM_ERROR(...)     do {\
   fprintf (stderr, "[%s:%i] ", __FILE__, __LINE__);\
   fprintf (stderr, __VA_ARGS__);\
} while (0)

typedef struct frm_nodeinfo_t frm_nodeinfo_t;

#ifdef __cplusplus
extern "C" {
#endif

   bool frm_create (const char *dbpath);
   bool frm_init (const char *dbpath);
   void frm_close (char *saved_path);

   frm_nodeinfo_t *frm_node_read (const char *dbpath, const char *path, ...);
   void frm_node_free (frm_nodeinfo_t *ni);

#ifdef __cplusplus
};
#endif


#endif


