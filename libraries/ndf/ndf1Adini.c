#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "star/util.h"
#include "mers.h"

void ndf1Adini( const char *type, hdsdim lbnda, hdsdim ubnda, void *pntr,
                int *status ){
/*
*+
*  Name:
*     ndf1Adini

*  Purpose:
*     Initialise an axis data array.

*  Synopsis:
*     void ndf1Adini( const char *type, hdsdim lbnda, hdsdim ubnda,
*                     void *pntr, int *status )

*  Description:
*     This function assigns initial values to an axis data array of any
*     numeric type. The values assigned are chosen so as to define the
*     default axis coordinate system, in which, for each axis, the pixel
*     with index (I) has a central coordinate of (I-0.5).

*  Parameters:
*     type
*        Pointer to a null terminated string holding the numeric type of
*        the axis data array to which initial values are to be assigned (an
*        HDS primitive numeric data type string, case insensitive).
*     lbnda
*        Index of the first pixel on the axis.
*     ubnda
*        Index of the last pixel on the axis.
*     pntr
*        Pointer to the 1-dimensional axis data array, whose size should be
*        equal to "ubnda" - "lbnda" + 1.
*     *status
*        The global status.

*  Notes:
*     -  Successive elements of the array are set to the values
*     "lbnda"-0.5, "lbnda"+0.5, "lbnda"+1.5, etc. (these values are rounded
*     up in the case of non-floating point types, so the values "lbnda",
*     "lbnda"+1, "lbnda"+2, etc. will result).
*     -  If overflow occurs because any of the values to be assigned cannot
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
   appropriate function to assign values to the array. */

/* ...byte. */
      if( !strcmp( utype, "_BYTE" ) ) {
         ndf1AdiB( lbnda, ubnda, pntr, status );

/* ...unsigned byte. */
      } else if( !strcmp( utype, "_UBYTE" ) ) {
         ndf1AdiUB( lbnda, ubnda, pntr, status );

/* ...double precision. */
      } else if( !strcmp( utype, "_DOUBLE" ) ) {
         ndf1AdiD( lbnda, ubnda, pntr, status );

/* ...integer. */
      } else if( !strcmp( utype, "_INTEGER" ) ) {
         ndf1AdiI( lbnda, ubnda, pntr, status );

/* ...real. */
      } else if( !strcmp( utype, "_REAL" ) ) {
         ndf1AdiF( lbnda, ubnda, pntr, status );

/* ...word. */
      } else if( !strcmp( utype, "_WORD" ) ) {
         ndf1AdiW( lbnda, ubnda, pntr, status );

/* ...unsigned word. */
      } else if( !strcmp( utype, "_UWORD" ) ) {
         ndf1AdiUW( lbnda, ubnda, pntr, status );

/* ...64-bit integer. */
      } else if( !strcmp( utype, "_INT64" ) ) {
         ndf1AdiK( lbnda, ubnda, pntr, status );

/* Note if the type string was not recognised. */
      } else {
         typok = 0;
      }
   }

/* If the type string was invalid, then report an error. */
   if( *status == SAI__OK ) {
      if( !typok ) {
         *status = NDF__FATIN;
         msgSetc( "ROUTINE", "ndf1Adini" );
         msgSetc( "BADTYPE", type );
         errRep( " ", "Function ^ROUTINE called with an invalid TYPE "
                 "parameter of '^BADTYPE' (internal programming error).",
                 status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Adini", status );

}

