#ifndef _GAIAHDS_INCLUDED_
#define _GAIAHDS_INCLUDED_
 
/*
 *  External prototypes and definitions for gaiaHDS.c.
 */

#include <star/hds.h>

#ifdef __cplusplus
extern "C" {
#endif

    /* Set an HDS tuning parameter */
    int gaiaHDSTune( char *what, int value, char **error_mess );

    /* Get an HDS tuning parameter */
    int gaiaHDSGTune( char *what, int *value, char **error_mess );

    /* Find a complex structure component */
    void hdsFind( const HDSLoc *loc1, const char *name, const char *mode, 
                  HDSLoc **loc2, int *status );
#ifdef __cplusplus
}
#endif

#endif
