#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "star/util.h"
#include "mers.h"

void ndf1Adext( const char *type, double scale, double zero, int upper,
                hdsdim pix0, hdsdim lbnda, hdsdim ubnda, void *pntr,
                int *status ){
/*
*+
*  Name:
*     ndf1Adext

*  Purpose:
*     Assign extrapolated values to an axis data array.

*  Synopsis:
*     void ndf1Adext( const char *type, double scale, double zero,
*                     int upper, hdsdim pix0, hdsdim lbnda, hdsdim ubnda,
*                     void *pntr, int *status )

*  Description:
*     This function assigns extrapolated values to an axis data array. It
*     is intended for assigning values to those axis data array elements
*     which are not present in an actual NDF data structure, but which are
*     encountered when accessing the axis component of a section which is a
*     super-set of the NDF.  Parameters relating the array element values
*     to the array index are provided as input arguments.

*  Parameters:
*     type
*        Pointer to a null terminated string holding the numeric type of
*        the array to be extrapolated; an HDS primitive numeric type string
*        (case insensitive).
*     scale
*        The scale factor relating the axis array index to the array values
*        according to the formula ARRAY( I ) = I * "scale" + "zero".
*     zero
*        The zero point of the extrapolation formula.
*     upper
*        If a non-zero value is given for this parameter, then
*        extrapolation will be performed towards higher array index values.
*        Otherwise extrapolation will be towards lower array index values.
*     pix0
*        The index of the first "unknown" pixel to be assigned a value. If
*        "upper" is non-zero, this will be the index of the pixel following
*        the last one whose value is known. If "upper" is zero, it will be
*        the index of the pixel before the first one whose value is known.
*     lbnda
*        The lower bound of the axis data array.
*     ubnda
*        The upper bound of the axis data array.
*     pntr
*        Pointer to the 1-dimensional axis data array to be extrapolated,
*        whose size should be equal to "ubnda" - "lbnda" + 1.
*     *status
*        The global status.

*  Notes:
*     -  If overflow occurs because any of the extrapolated values cannot
*     be represented using the array's numeric type, then an error will be
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
*     3-APR-2019 (DSB):
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
   appropriate function to extrapolate the array. */

/* ...byte. */
      if( !strcmp( utype, "_BYTE" ) ) {
         ndf1AdeB( scale, zero, upper, pix0, lbnda, ubnda, pntr, status );

/* ...unsigned byte. */
      } else if( !strcmp( utype, "_UBYTE" ) ) {
         ndf1AdeUB( scale, zero, upper, pix0, lbnda, ubnda, pntr, status );

/* ...double precision. */
      } else if( !strcmp( utype, "_DOUBLE" ) ) {
         ndf1AdeD( scale, zero, upper, pix0, lbnda, ubnda, pntr, status );

/* ...integer. */
      } else if( !strcmp( utype, "_INTEGER" ) ) {
         ndf1AdeI( scale, zero, upper, pix0, lbnda, ubnda, pntr, status );

/* ...real. */
      } else if( !strcmp( utype, "_REAL" ) ) {
         ndf1AdeF( scale, zero, upper, pix0, lbnda, ubnda, pntr, status );

/* ...word. */
      } else if( !strcmp( utype, "_WORD" ) ) {
         ndf1AdeW( scale, zero, upper, pix0, lbnda, ubnda, pntr, status );

/* ...unsigned word. */
      } else if( !strcmp( utype, "_UWORD" ) ) {
         ndf1AdeUW( scale, zero, upper, pix0, lbnda, ubnda, pntr, status );

/* ...64bit integer. */
      } else if( !strcmp( utype, "_INT64" ) ) {
         ndf1AdeK( scale, zero, upper, pix0, lbnda, ubnda, pntr, status );

/* Note if the type string was not recognised. */
      } else {
         typok = 0;
      }
   }

/* If the type string was invalid, then report an error. */
   if( *status == SAI__OK ) {
      if( !typok ) {
         *status = NDF__FATIN;
         msgSetc( "ROUTINE", "ndf1Adext" );
         msgSetc( "BADTYPE", type );
         errRep( " ", "Function ^ROUTINE called with an invalid TYPE "
                 "parameter of '^BADTYPE' (internal programming error).",
                 status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Adext", status );

}

