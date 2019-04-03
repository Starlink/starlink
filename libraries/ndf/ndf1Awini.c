#include <string.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1Awini( const char *type, hdsdim lbnd, hdsdim ubnd,
                const double data[], void *pntr, int *status ){
/*
*+
*  Name:
*     ndf1Awini

*  Purpose:
*     Initialise an axis width array.

*  Synopsis:
*     void ndf1Awini( const char *type, hdsdim lbnd, hdsdim ubnd,
*                     const double data[], void *pntr, int *status )

*  Description:
*     This function initialises an axis width array of any numeric type.
*     The values assigned are calculated from an associated axis data array
*     (giving the positions of the pixel centres) by forming differences
*     between the centre positions of neighbouring pixels.

*  Parameters:
*     type
*        Pointer to a null terminated string holding the numeric type of
*        the axis width array; a primitive numeric type string (case
*        insensitive).
*     lbnd
*        The lower bound of the axis width array.
*     ubnd
*        The upper bound of the axis width array.
*     data
*        Array of axis data values (containing pixel centre coordinates)
*        from which the axis width values should be derived. The supplied
*        "data" array should have at least "ubnd-lbnd+1" elements.
*     pntr
*        Pointer to the axis width array, whose size should be equal to
*        "ubnd-lbnd+1".
*     *status
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the width values cannot be
*     represented using the array's numeric type, then an error will be
*     reported and "status" set.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char utype[ NDF__SZTYP + 1 ];   /* Upper case type string */
   int typok;            /* Whether type string is valid */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the supplied string is not too long, convert it to upper case. */
   typok =  ( strlen(type) < sizeof( utype ) );
   if( typok ) {
      astChrCase( type, utype, 1, sizeof( utype ) );

/* Test the type string against each valid value in turn, calling the
   appropriate function to initialise the array. */

/* ...byte. */
      if( !strcmp( utype, "_BYTE" ) ) {
         ndf1AwiB( lbnd, ubnd, data, pntr, status );

/* ...unsigned byte. */
      } else if( !strcmp( utype, "_UBYTE" ) ) {
         ndf1AwiUB( lbnd, ubnd, data, pntr, status );

/* ...double precision. */
      } else if( !strcmp( utype, "_DOUBLE" ) ) {
         ndf1AwiD( lbnd, ubnd, data, pntr, status );

/* ...integer. */
      } else if( !strcmp( utype, "_INTEGER" ) ) {
         ndf1AwiI( lbnd, ubnd, data, pntr, status );

/* ...real. */
      } else if( !strcmp( utype, "_REAL" ) ) {
         ndf1AwiF( lbnd, ubnd, data, pntr, status );

/* ...word. */
      } else if( !strcmp( utype, "_WORD" ) ) {
         ndf1AwiW( lbnd, ubnd, data, pntr, status );

/* ...unsigned word. */
      } else if( !strcmp( utype, "_UWORD" ) ) {
         ndf1AwiUW( lbnd, ubnd, data, pntr, status );

/* ...64-bit integer. */
      } else if( !strcmp( utype, "_INT64" ) ) {
         ndf1AwiK( lbnd, ubnd, data, pntr, status );

/* Note if the type string was not recognised. */
      } else {
         typok = 0;
      }
   }

/* If the type string was invalid, then report an error. */
   if( *status == SAI__OK ) {
      if( !typok ) {
         *status = NDF__FATIN;
         msgSetc( "ROUTINE", "ndf1Awini" );
         msgSetc( "BADTYPE", type );
         errRep( " ", "Function ^ROUTINE called with an invalid TYPE "
                 "parameter of '^BADTYPE' (internal programming error).",
                 status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Awini", status );

}

