
#ifndef H_PHOC
#define H_PHOC

typedef struct phocus_t phocus_t;

#ifdef __cplusplus
extern "C" {
#endif

   phocus_t *phocus_create (const char *fname);
   phocus_t *phocus_open (const char *fname);


#ifdef __cplusplus
};
#endif


#endif


