#include <ctype.h>
#include "ary1.h"
#include "ary_ast.h"
#include "sae_par.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"

void ary1Chscn( const char *name, int *status ) {
/*
*+
*  Name:
*     ary1Chscn

*  Purpose:
*     Check for a standard data component name.

*  Synopsis:
*     void ary1Chscn( const char *name, int *status )

*  Description:
*     The routine checks that the data component name supplied conforms
*     to the standard form for naming Starlink data structure
*     components; i.e. that it is not blank, is no more than DAT__SZNAM
*     characters long, begins with an alphabetic character and
*     continues with alphanumeric characters (including underscore)
*     only. If the name is non-standard, then an error is reported.
*     Otherwise, the routine returns without further action.

*  Parameters:
*     name
*        The data component name to be checked.
*     status
*        The global status.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     23-JUN-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local Variables: */
   char ok;          /* Is the supplied component name standard? */
   const char *cp;   /* Pointer to next character in name */
   size_t i;         /* Index of next character */
   size_t tlen;      /* Used length of supplied string */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Assume the component name is OK. */
   ok = 1;

/* Get the used length of the string and check it is neither too short
   nor too long. */
   tlen = astChrLen( name );
   if( tlen == 0  || tlen > DAT__SZNAM ) {
      ok = 0;

/* Check the first character is alphabetical. */
   } else if( !isalpha( name[0] ) ) {
      ok = 0;

/* Check the remaining characters are alphanumeric or underscore. */
   } else {
      cp = name + 1;
      for( i = 1; i < tlen; i++,cp++ ) {
         if( !isalnum( *cp ) && *cp != '_' ) {
            ok = 0;
            break;
         }
      }
   }

/* If it is not OK, then report an error. */
   if( !ok ) {
      *status = ARY__NSDCN;
      msgSetc( "NAME", name );
      errRep( "ARY1_CHSCN_BAD",
              "Non-standard data component name '^NAME' specified (possible"
              " programming error).", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Chscn", status );

}
