/*
 *+
 *  Name:
 *     emsGtune

 *  Purpose:
 *     Get the value of an EMS tuning parameter

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsGtune( key, value, status )

 *  Arguments:
 *     key = char* (Given)
 *        The name of the tuning parameter.
 *     value = int* (Returned)
 *        The tuning value (see description in emsTune).
 *     status = int* (Given and Returned)
 *        The global status.

 *  Description:
 *     The value of the given EMS tuning parameter is returned. The tuning
 *     parameters are described in emsTune.
 *
 *     The routine will attempt to execute regardless of the given value of
 *     STATUS. If the given value is not SAI__OK, then it is left unchanged,
 *     even if the routine fails to complete. If the STATUS is SAI__OK on
 *     entry and the routine fails to complete, STATUS will be set and an
 *     error report made.

 *  Copyright:
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful,but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     30-JUL-2008 (PWD):
 *        Original version.
 *     {enter_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include "sae_par.h"                 /* Standard SAE constants */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems_err.h"                 /* EMS_ EMS_ error codes*/
#include "ems.h"                     /* EMS_ function prototypes */
#include "ems1.h"                    /* EMS_ private functions prototypes */
#include "ems_defs.h"                /* EMS_ message table */

void emsGtune( const char *key, int *value, int *status )
{
    int lstat;                /* Local status */

    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "emsGtune" );

    /*  Initialise local status */
    lstat = SAI__OK;

    /*  Check that the given KEY is acceptable. */
    if ( strcasecmp( key, "SZOUT" ) == 0 ) {
        *value = msgtab->msgwsz;
    } 
    else if ( strcasecmp( key, "MSGDEF" ) == 0 ) {
        *value = msgtab->msgdef;
    } 
    else if ( strcasecmp( key, "STREAM" ) == 0 ) {
        *value = msgtab->msgstm;
    } 
    else if ( strcasecmp( key, "REVEAL" ) == 0 ) {
        *value = msgtab->msgrvl;
    } 
    else {
        /*  The given tuning parameter was not in the available set. */
        emsMark();
        lstat = EMS__BDKEY;
        emsSetc( "KEY", key );
        emsRep( "EMS_TUNE_INV", "EMS_TUNE: Invalid tuning parameter: ^KEY",
                &lstat );
        emsRlse();
    }

    /*  Set return status */
    if ( *status == SAI__OK ) *status = lstat;

    return;
}
