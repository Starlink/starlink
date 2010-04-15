#ifndef _GAIAHDS_INCLUDED_
#define _GAIAHDS_INCLUDED_

/*
 *  External prototypes and definitions for gaiaHDS.c.
 */

#ifdef __cplusplus
extern "C" {
#endif

    /* Set an HDS tuning parameter */
    int gaiaHDSTune( char *what, int value, char **error_mess );

    /* Get an HDS tuning parameter */
    int gaiaHDSGTune( char *what, int *value, char **error_mess );

#ifdef __cplusplus
}
#endif

#endif
