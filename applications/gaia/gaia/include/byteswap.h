/*  Define the byteswap function */

#ifndef _BYTESWAP_INCLUDED_ 
#define _BYTESWAP_INCLUDED_

#if defined(__cplusplus) || defined(c_plusplus)
#define EXTERN extern "C"
#else
#define EXTERN extern
#endif

EXTERN void *byteswap( void *ptr, int size, int nbyte );
EXTERN void byteswapcopy( void *ptr, int size, int nbyte, void *outptr );

#endif /* _BYTESWAP_INCLUDED_ */
