/*+
 *  Name:
 *     gaiaHDS

 *  Purpose:
 *     Utility routines for accessing HDS directly.

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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

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
#include <ems.h>

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
