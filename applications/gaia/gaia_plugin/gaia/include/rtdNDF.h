/*
 *  Prototypes for rtdNDF.c.
 */

#ifdef __cplusplus 
extern "C" {
#endif

int rtdAccessNDF( const char *filename, int *type, int *width, int *height, 
                char **header, int *header_length, int *ndfid, 
                char **error_message );

int rtdWriteNDF( const char *filename, int type, int width, int height, 
                 void *data, int ndfid, const char *component, 
                 const char *header, int lheader, char **error_message ); 

int rtdFreeNDF( int ndfid );

int rtdCopyNDF( int ndfid, void **data, const char *component, 
                char **error_message );
#ifdef __cplusplus 
}
#endif
