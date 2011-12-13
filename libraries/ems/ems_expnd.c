/*+
 *  Name:
 *     ems_expnd
 *
 *  Purpose:
 *     Fortran callable routine

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
#include <stdlib.h>
#include "ems.h"                       /* ems_ function prototypes */
#include "f77.h"                       /* CNF macros and prototypes */

#include "ems_f77.h"

F77_SUBROUTINE(ems_expnd) ( CHARACTER(text), CHARACTER(opstr),
      INTEGER(oplen), INTEGER(status) TRAIL(text) TRAIL(opstr) )
{
   GENPTR_CHARACTER(text)
   GENPTR_CHARACTER(opstr)
   GENPTR_INTEGER(oplen)
   GENPTR_INTEGER(status)

   char *str;                 /* Buffer for expanded string */
   char *ctext;               /* Imported text string */

/* Import the given strings */
   ctext = cnfCreim( text, text_length );
   str = cnfCreat( opstr_length );

   /* Note that escaping of % in token values is disabled for fortran
      interface */
   emsExpnd( ctext, str, opstr_length, 0, oplen, status );

   cnfExprt( str, opstr, opstr_length );
   cnfFree( ctext );
   cnfFree( str );
}
