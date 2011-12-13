/*+
 *  Name:
 *     ems1Itoktab

 *  Purpose:
 *     Initialize a token message table.

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
#include "ems_defs.h"                /* Error table definitions */
#include "ems1.h"

void ems1Itoktab( ems_toktab_t *toktab )
{
    /*  Initialize all to zero, and set parts as needed. */
    memset( toktab, 0, sizeof( ems_toktab_t ) );

    toktab->toklev = EMS__BASE;
    toktab->tokmrk = EMS__BASE;
}
