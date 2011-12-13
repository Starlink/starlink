/*+
 *  Name:
 *     ems1Imsgtab

 *  Purpose:
 *     Initialize an error message table.

 *  Language:
 *     Starlink ANSI C

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
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     15-MAY-2008 (PWD):
 *        Original version.
 *     {enter_further_changes_here}

 *-
 */
#include <string.h>

#include "sae_par.h"                 /* Standard SAE constants */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems_err.h"                 /* EMS_ error codes */
#include "ems1.h"                    /* EMS_ private functions prototypes */
#include "ems_defs.h"                /* EMS_ message table */

void ems1Imsgtab( ems_msgtab_t *msgtab )
{
    int i;

    /*  Initialize all to zero, and set parts as needed. */
    memset( msgtab, 0, sizeof( ems_msgtab_t ) );

    msgtab->msgdef = EMS__BASE;    /* Default error reporting context */
    msgtab->msglev = EMS__BASE;    /* Error context level */
    msgtab->msglst = SAI__OK;      /* Last reported status (level 1 only) */
    msgtab->msgmrk = EMS__BASE;    /* Number of markers */
    msgtab->msgwsz = EMS__SZOUT;   /* Line wrapping length */
    msgtab->msgrvl = FALSE;        /* Whether EMS tuning is REVEAL */
    msgtab->msgslt = FALSE;        /* Whether EMS tuning is SILENT */
    msgtab->msgstm = FALSE;        /* Whether EMS tuning is STREAM */

    for ( i = 0; i < EMS__MXLEV+1; i++ ) {
        msgtab->msgbgs[i] = EMS__NSTER; /* Given status values to EMS_BEGIN */
    }
}
