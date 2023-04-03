
#ifndef H_PHOC
#define H_PHOC

typedef struct phocus_header_t phocus_header_t;
typedef struct phocus_t phocus_t;

#ifdef __cplusplus
extern "C" {
#endif

   phocus_header_t *phocus_read_header (const char *fname);
   void phocus_header_del (phocus_header_t *header);
   size_t phocus_header_id (phocus_header_t *header);
   const char *phocus_header_title (phocus_header_t *header);

   phocus_t *phocus_create (const char *fname);
   phocus_t *phocus_open (const char *fname);
   bool phocus_close (phocus_t *phocus);


#ifdef __cplusplus
};
#endif


#endif


