/*
 * Already done these definitions?
 */
#if !defined(_ADI_STRING_H_)
#define _ADI_STRING_H_ 1

/*
 * Cater for C++
 */
#ifdef __cplusplus
extern "C" {
#endif

/*
 * Sub-package initialiser
 */
void   strx_init( ADIstatus status );

/*
 * ADI internal routines
 */
char   *strx_alloc( int len, ADIstatus status );
int     strx_cmp( ADIobj s1, ADIobj s2 );
int     strx_cmpc( char *s1, int len1, ADIobj s2 );
int     strx_cmp2c( char *s1, int len1, char *s2, int len2 );
int     strx_cmpi( ADIobj s1, ADIobj s2 );
int     strx_cmpi2c( char *s1, int len1, char *s2, int len2 );
int     strx_cmpic( char *s1, int len1, ADIobj str2 );
char   *strx_dupl( char *str, int len );
void 	ADIstrngExport( ADIobj id, int clang, char *buf, int blen,
			ADIstatus status );
void    strx_free( char *str, int len, ADIstatus status );
int     strx_hash( char *str, int slen, int hsize );
void 	ADIstrngGetLen( ADIobj id, ADIinteger *rval, ADIstatus status );
ADIobj	ADIstrngEnsureNterm( ADIobj id, ADIstatus status );

#ifdef __cplusplus
}
#endif

#endif
