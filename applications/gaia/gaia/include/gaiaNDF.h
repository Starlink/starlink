/*
 *  External prototypes for gaiaNDF.c.
 */

/*  Maximum character length of NDF filename or HDS path */
#define MAXNDFNAME 256

#ifdef __cplusplus
extern "C" {
#endif

/*  Simple NDF interface */
/*  ==================== */

   /*  Open an NDF */
   int gaiaAccessNDF( const char *filename, int *type, int *width, int *height,
                      char **header, int *header_length, int *ndfid,
                      char **error_message );
   /*  Write an NDF */
   int gaiaWriteNDF( const char *filename, int type, int width, int height,
                     void *data, int ndfid, const char *component,
                     const char *header, int lheader, char **error_message );
   /*  Free an NDF */
   int gaiaFreeNDF( int ndfid );

   /*  Copy NDF data component */
   int gaiaCopyNDF( int ndfid, void **data, const char *component,
                    char **error_message );

   /*  Map an NDF data component */
   int gaiaMapNDF( int ndfid, void **data, const char *component,
                   char **error_message );

   /*  Unmap an NDF data component */
   int gaiaNDFUnmap( int ndfid, const char *component, char **error_mess );


/*  Multiple NDFs per container interface */
/*  ===================================== */

   /*  Initialise multiple NDF access */
   int gaiaInitMNDF( const char *name, void **handle, char **error_mess );

   /*  Check component of NDF exists */
   int gaiaCheckMNDF( const void *handle, int index, const char *component );

   /*  Get info about NDF */
   void gaiaGetInfoMNDF( const void *handle, int index, char **name,
                         int *type, int *width, int *height,
                         char **header, int *hlen, int *ndfid,
                         int *hasvar, int *hasqual );

   /*  Get access to NDF data component */
   int gaiaGetMNDF( const void *handle, int index, const char *component,
                    void **data, char **error_mess );

   /*  Release all NDFs and resources */
   void gaiaReleaseMNDF( const void *handle );

   /*  Release a single NDF */
   void gaiaFreeMNDF( void *handle, int index );

   /*  Return number of NDFs available. */
   int gaiaCountMNDFs( const void *handle );

   /*  Set readonly status of component */
   void gaiaSetReadMNDF( const void *handle, int index, int readonly );

   /*  Get readonly status of component */
   int gaiaGetReadMNDF( const void *handle, int index );

#ifdef __cplusplus
}
#endif
