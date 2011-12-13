/*+
 *  Name:
 *     gaiaHDS

 *  Purpose:
 *     Utility routines for accessing HDS.

 *  Language:
 *     C

 *  Copyright:
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of the
 *     License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *   Authors:
 *      PWD: Peter W. Draper, Starlink - University of Durham

 *   History:
 *      29-MAR-2006 (PWD):
 *         Original version.
 *      {enter_changes_here}
 *-
 */

#if HAVE_CONFIG_H
#include "config.h"
#endif

#include <sae_par.h>
#include <gaiaUtils.h>
#include <star/hds.h>
#include <star/hds_fortran.h>
#include <ems.h>
#include <gaiaHDS.h>

/**
 * Set a HDS tuning parameter.
 */
int gaiaHDSTune( char *what, int value, char **error_mess )
{
    int status = SAI__OK;
    emsMark();
    hdsTune( what, value, &status );
    if ( status != SAI__OK ) {
        *error_mess = gaiaUtilsErrMessage();
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
        *error_mess = gaiaUtilsErrMessage();
        emsRlse();
        return 1;
    }
    emsRlse();
    return 0;
}
