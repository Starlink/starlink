#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "ary_ast.h"
#include <string.h>

void ary1Bpp( const char *type, size_t el, void *pntr, char *bad, int *status ) {
/*
*+
*  Name:
*     ary1Bpp

*  Purpose:
*     Determine if bad pixels are present in a vectorised array.

*  Synopsis:
*     void ary1Bpp( const char *type, size_t el, void *pntr, char *bad,
*                   int *status )

*  Description:
*     This function examines the values in a vectorised array of any
*     numeric type and returns a boolean result in "bad" indicating whether
*     any element of the array contains the appropriate "bad" pixel
*     value VAL__BADx (where x corresponds with the array's data type).
*     The vectorised array to be examined is passed by pointer.

*  Parameters:
*     type
*        The data type of the vectorised array; an HDS primitive
*        numeric data type string (case insensitive).
*     el
*        The number of elements in the vectorised array.
*     pntr
*        Pointer to the array.
*     bad
*        Returned holding a flag indicating whether any array element had
*        the value VAL__BADx.
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
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   char utype[ARY__SZTYP+1];  /* Upper case version of TYPE */
   char typok;                /* Whether the TYPE argument is valid */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the supplied string is not too long, convert it to upper case. */
   typok =  ( strlen(type) <= ARY__SZTYP );
   if( typok ) {
      astChrCase( type, utype, 1, ARY__SZTYP + 1 );

/* Compare the data type with each permitted value in turn, calling the
   appropriate routine to examine the array for bad pixels. */

/* ...byte. */
      if( !strcmp( utype, "_BYTE" ) ){
         ary1BppB( el, pntr, bad, status );

/* ...unsigned byte. */
      } else if( !strcmp( utype, "_UBYTE" ) ){
         ary1BppUB( el, pntr, bad, status );

/* ...double precision. */
      } else if( !strcmp( utype, "_DOUBLE" ) ){
         ary1BppD( el, pntr, bad, status );

/* ...integer. */
      } else if( !strcmp( utype, "_INTEGER" ) ){
         ary1BppI( el, pntr, bad, status );

/* ...real. */
      } else if( !strcmp( utype, "_REAL" ) ){
         ary1BppF( el, pntr, bad, status );

/* ...word. */
      } else if( !strcmp( utype, "_WORD" ) ){
         ary1BppW( el, pntr, bad, status );

/* ...unsigned word. */
      } else if( !strcmp( utype, "_UWORD" ) ){
         ary1BppUW( el, pntr, bad, status );

/* ...64-bit integer. */
      } else if( !strcmp( utype, "_INT64" ) ){
         ary1BppK( el, pntr, bad, status );

/* Note if the data type string was not recognised. */
      } else {
         typok = 0;
      }
   }

/* If the TYPE argument was not valid, then report an error. */
   if( !typok && *status == SAI__OK ){
      *status = ARY__FATIN;
      msgSetc( "BADTYPE", type );
      errRep( " ", "Routine ary1Bpp called with an invalid TYPE argument "
              "of '^BADTYPE' (internal programming error).", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Bpp", status );

}
