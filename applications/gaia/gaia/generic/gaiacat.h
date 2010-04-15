/*
 *  Prototypes for gaiacat.c.
 */

#ifdef __cplusplus
extern "C" {
#endif
int gaiaAccessCat( const char *filename, int *catid, char **error_mess );
int gaiaFreeCat( int catId );
int gaiaReadCat( int catId, char **tabData, char **error_mess );
#ifdef __cplusplus
}
#endif


