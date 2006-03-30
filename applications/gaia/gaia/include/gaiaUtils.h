#ifndef _GAIAUTILS_INCLUDED_
#define _GAIAUTILS_INCLUDED_
 
/*
 *  External prototypes and definitions for gaiaUtils.c.
 */

#ifdef __cplusplus
extern "C" {
#endif

    /* Constructor an error message string from an ADAM status. */
    char *gaiaUtilsErrMessage( int *status );

#ifdef __cplusplus
}
#endif

#endif
