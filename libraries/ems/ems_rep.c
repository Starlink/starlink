/*+
 *  Name:
 *     emsRep
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
#include <stdlib.h>
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_par.h"                   /* EMS constatnts           */
#include "f77.h"                       /* CNF macros and prototypes */

#include "ems_f77.h"

F77_SUBROUTINE(ems_rep) ( CHARACTER(err), CHARACTER(text), INTEGER(status)
         TRAIL(err) TRAIL(text) ) {

   char cerr[EMS__SZPAR+1];  /* Imported message name */
   char *ctext;              /* Imported text string */

   GENPTR_CHARACTER(err)
   GENPTR_CHARACTER(text)
   GENPTR_INTEGER(status)

/* Import the given strings
*/
   cnfImpn( err, err_length, EMS__SZPAR, cerr );
   ctext = cnfCreim( text, text_length );

   emsRep( cerr, ctext, status);

   cnfFree( ctext );
   return;
}
