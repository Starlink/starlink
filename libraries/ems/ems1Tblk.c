/*
*+
*  Name:
*     ems1Tblk

*  Purpose:
*     Initial contents of the message token table.

*  Language:
*     Starlink ANSI C

*  Type of module:
*     External data initialisation

*  Description:
*     This routine initialises the EMSTK_CMN common blocks to perform 
*     the initialisation of the EMS_ message token table.

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council.
*     Copyright (C) 2001 Central Laboratory of the Research Councls.
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
*     BDK: B.D. Kelly (ROE)
*     PCTR: P.C.T. Rees (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1987 (BDK):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP)
*        Rewritten in C based on the Fortran routine EMS1_MBLK
*     {enter_further_changes_here}

*-
*/

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */

int toklev = EMS__BASE;
int tokmrk = EMS__BASE;
int tokcnt[ EMS__MXLEV + 1] = { 0 };
int tokhiw[ EMS__MXLEV + 1] = { 0 };
int toklen[ EMS__MXTOK + 1] = { 0 };
char toknam[ EMS__MXTOK + 1][ EMS__SZNAM + 1 ];
char tokstr[ EMS__MXTOK + 1][ EMS__SZTOK + 1 ];

void ems1Tblk() {
}
