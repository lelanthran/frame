
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

#ifdef __cplusplus
extern "C" {
#endif

   char *frm_readfile (const char *fname);
   bool frm_vwritefile (const char *fname, const char *data, va_list ap);
   bool frm_writefile (const char *fname, const char *data, ...);

   frm_t *frm_create (const char *dbpath);
   frm_t *frm_init (const char *dbpath);
   void frm_close (frm_t *frm);

   char *frm_history (frm_t *frm, size_t count);
   char *frm_current (frm_t *frm);
   char *frm_payload (frm_t *frm);
   uint64_t frm_date_epoch (frm_t *frm);
   char *frm_date_str (frm_t *frm);

   bool frm_push (frm_t *frm, const char *name, const char *message);
   bool frm_payload_replace (frm_t *frm, const char *message);
   bool frm_payload_append (frm_t *frm, const char *message);
   char *frm_payload_fname (frm_t *frm);

   bool frm_top (frm_t *frm);
   bool frm_up (frm_t *frm);
   bool frm_down (frm_t *frm, const char *target);
   bool frm_switch (frm_t *frm, const char *target);
   bool frm_back (frm_t *frm, size_t index);
   bool frm_delete (frm_t *frm, const char *target);
   bool frm_pop (frm_t *frm);

   char **frm_list (frm_t *frm, const char *from);
   char **frm_match (frm_t *frm, const char *sterm, uint32_t flags);
   char **frm_match_from_root (frm_t *frm, const char *sterm, uint32_t flags);

#ifdef __cplusplus
};
#endif


#endif


