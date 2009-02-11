/*+
 *  Name:
 *     emsSetc
 *
 *  Purpose:
*  Fortran callable routine

 *  Authors:
 *    AJC: Alan Chipperfield (Starlink)
 *    TIMJ: Tim Jenness (JAC, Hawaii)

 *  History:
 *    2000 (AJC):??
 *      Fortran wrapper around ems1Stok
 *    11-FEB-2009 (TIMJ):
 *      Allow a long string to be imported (longer than EMS__SZNAM)
 *      so that ems1Putc is allowed to truncate it consistently.

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

*  Copyright:
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

/* Ensure minimum 1 space */
   if ( ! strlen( ccval ) ) {
      strcpy( ccval, " " );
   }

/* Now set the token string */
   emsSetc( ctok, ccval );

/* Free memory */
   cnfFree( ccval );

   return;
}
