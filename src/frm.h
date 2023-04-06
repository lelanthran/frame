
#ifndef H_FRM
#define H_FRM

#define FRM_ERROR(...)     do {\
   fprintf (stderr, "[%s:%i] ", __FILE__, __LINE__);\
   fprintf (stderr, __VA_ARGS__);\
} while (0)

typedef struct frm_t frm_t;


#ifdef __cplusplus
extern "C" {
#endif

   frm_t *frm_create (const char *dbpath);
   frm_t *frm_init (const char *dbpath);
   void frm_close (frm_t *frm);

   char *frm_history (frm_t *frm, size_t count);
   char *frm_current (frm_t *frm);
   char *frm_payload (frm_t *frm);
   uint64_t frm_date_epoch (frm_t *frm);
   char *frm_date_str (frm_t *frm);

#ifdef __cplusplus
};
#endif


#endif


