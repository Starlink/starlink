/*
 *+
 *  Name:
 *     ems1Tblk

 *  Purpose:
 *     Initialise contents of the message token table.

 *  Language:
 *     Starlink ANSI C

 *  Type of module:
 *     External data initialisation

 *  Description:
 *     This routine initialises the global token table.

 *  Copyright:
 *     Copyright (C) 1987 Science & Engineering Research Council.
 *     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
 *     BDK: B.D. Kelly (ROE)
 *     PCTR: P.C.T. Rees (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     10-JUN-1987 (BDK):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP)
 *        Rewritten in C based on the Fortran routine EMS1_MBLK
 *     14-MAY-2008 (PWD):
 *        Changed to use a struct.
 *     {enter_further_changes_here}

 *-
 */

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private function prototypes */
#include "ems_defs.h"                /* EMS_ token table */

/* The table struct. */
static ems_toktab_t ems_toktab_base = {
    EMS__BASE,       /* toklev */
    EMS__BASE,       /* tokmrk */
    {0},             /* tokcnt */
    {0},             /* tokhiw */
    {0},             /* toklen */
    {""},            /* toknam */
    {""},            /* tokstr */
    0                /* userdata, spare for additional context */
};

ems_toktab_t *ems_toktab = &ems_toktab_base;

void ems1Tblk() {
}
