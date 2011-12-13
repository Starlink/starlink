/*+
 *  Name:
 *     emsEload
 *
 *  Purpose:
*  Fortran callable routine

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

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

 *  Authors:

 *-
*/
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_par.h"                   /* EMS constants            */
#include "f77.h"                       /* CNF macros and prototypes */

#include "ems_f77.h"

F77_SUBROUTINE(ems_eload) ( CHARACTER(param), INTEGER(parlen), CHARACTER(opstr),
            INTEGER(oplen), INTEGER(status) TRAIL(param) TRAIL(opstr)) {

   GENPTR_CHARACTER(param)
   GENPTR_INTEGER(parlen)
   GENPTR_CHARACTER(opstr)
   GENPTR_INTEGER(oplen)
   GENPTR_INTEGER(status)

   char str1[EMS__SZMSG+1];
   char str2[EMS__SZMSG+1];

   emsEload(str1, parlen, str2, oplen, status);
   cnfExprt( str1, param, param_length );
   cnfExprt( str2, opstr, opstr_length );

   return;
}
