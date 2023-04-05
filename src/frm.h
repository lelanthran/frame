
#ifndef H_FRM
#define H_FRM

typedef struct frm_header_t frm_header_t;
typedef struct frm_t frm_t;

#ifdef __cplusplus
extern "C" {
#endif

   frm_header_t *frm_read_header (const char *dbpath);
   void frm_header_del (frm_header_t *header);
   size_t frm_header_id (frm_header_t *header, size_t index);
   const char *frm_header_title (frm_header_t *header, size_t index);

   frm_t *frm_create (const char *dbpath);
   frm_t *frm_open (const char *dbpath);
   void frm_close (frm_t *frm);


#ifdef __cplusplus
};
#endif


#endif


