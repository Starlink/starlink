/*+
 *  Name:
 *     ems_setc
 *
 *  Purpose:
 *     Fortran callable implementation of emsSetc

 *  Authors:
 *    AJC: Alan Chipperfield (Starlink)
 *    TIMJ: Tim Jenness (JAC, Hawaii)

 *  History:
 *    2000 (AJC):??
 *      Fortran wrapper around ems1Stok
 *    11-FEB-2009 (TIMJ):
 *      Allow a long string to be imported (longer than EMS__SZNAM)
 *      so that ems1Putc is allowed to truncate it consistently.
 *    4-MAR-2009 (TIMJ):
 *      Do not force token value to be a single space since a blank
 *      Fortran string will result in a single byte being allocated
 *      holding '\0'.

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
 *     Copyright (C) 2009 Science and Technology Faciltiies Council.
 *     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
 *     All Rights Reserved.

 *  Authors:

 *-
 */
#include <string.h>
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_par.h"                   /* EMS constants */
#include "ems_sys.h"                   /* EMS internal constants */
#include "ems1.h"                      /* ems1_ function prototypes */
#include "f77.h"                       /* CNF macros and prototypes */

#include "ems_f77.h"

F77_SUBROUTINE(ems_setc) ( CHARACTER(token), CHARACTER(cvalue)
         TRAIL(token) TRAIL(cvalue) )
{
   char ctok[EMS__SZNAM+1];      /* Imported token name */
   char *ccval;                  /* Imported char value */

   GENPTR_CHARACTER(token)
   GENPTR_CHARACTER(cvalue)

   cnfImpn( token, token_length, EMS__SZNAM, ctok );
   ccval = cnfCreim( cvalue, cvalue_length );

/* Now set the token string */
   emsSetc( ctok, ccval );

/* Free memory */
   cnfFree( ccval );

   return;
}
