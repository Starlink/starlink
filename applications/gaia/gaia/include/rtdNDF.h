/*
 *  Prototypes for rtdNDF.c.
 */

#ifdef __cplusplus 
extern "C" {
#endif
   
   
   /*  Simple interface */
   int rtdAccessNDF( const char *filename, int *type, int *width, int *height, 
                     char **header, int *header_length, int *ndfid, 
                     char **error_message );
   int rtdWriteNDF( const char *filename, int type, int width, int height, 
                    void *data, int ndfid, const char *component, 
                    const char *header, int lheader, char **error_message ); 
   int rtdFreeNDF( int ndfid );
   int rtdCopyNDF( int ndfid, void **data, const char *component, 
                   char **error_message );
   
   /*  Multiple NDFs per container interface */
   int rtdInitNDF( const char *name, void **handle, char **error_mess ); 
   int rtdCheckDisplayable( const void *handle, int index, 
                            const char *component ); 
   void rtdGetNDFInfo( const void *handle, int index, char **name, 
                       int *type, int *width,
                       int *height, char **header, int *hlen,
                       int *ndfid, int *havvar, int *havqual );
   int rtdCopyDisplayable( const void *handle, int index, 
                           const char *component,
                           void **data, char **error_mess );
   int rtdCountNDFs( const void *handle );

   void rtdReleaseNDF( const void *handle );

#ifdef __cplusplus 
}
#endif
