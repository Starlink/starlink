/*+
 *   Name:
 *      gaiaHDS

 *   Purpose:
 *      Utility routines for accessing HDS directly.

 *   Language:
 *      C

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics and Astronomy Research Council

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      29-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#include <sae_par.h>
#include <gaiaHDS.h>
#include <gaiaUtils.h>
#include <star/hds.h>

/**
 * Set a HDS tuning parameter.
 */
int gaiaHDSTune( char *what, int value, char **error_mess )
{
    int status = SAI__OK;
    emsMark();
    hdsTune( what, value, &status );
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage( &status );
        emsRlse();
        return 1;
    }
    emsRlse();
    return 0;
}

/**
 * Get a HDS tuning parameter.
 */
int gaiaHDSGTune( char *what, int *value, char **error_mess )
{
    int status = SAI__OK;
    emsMark();
    hdsGtune( what, value, &status );
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage( &status );
        emsRlse();
        return 1;
    }
    emsRlse();
    return 0;
}
