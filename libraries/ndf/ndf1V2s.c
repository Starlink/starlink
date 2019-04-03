#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "star/util.h"
#include "mers.h"

void ndf1V2s( int bad, const char *type, size_t el, void *pntr, int *dce,
              int *status ){
/*
*+
*  Name:
*     ndf1V2s

*  Purpose:
*     Convert variance values to standard deviations.

*  Synopsis:
*     void ndf1V2s( int bad, const char *type, size_t el, void *pntr,
*                   int *dce, int *status )

*  Description:
*     This function converts a vectorised array of variance values into
*     standard deviations by taking the square root. It will check for
*     "bad" values in the array if required. If a negative variance value
*     is found, then "status" is set to NDF__NGVAR, an error is reported,
*     and a "bad" value is assigned to the affected array element -
*     however, the function continues to process the entire array. The
*     array to be processed is passed by pointer.

*  Parameters:
*     bad
*        Whether it is necessary to check for bad values.
*     type
*        Pointer to a null terminated string holding the numeric data type
*        of the array; a primitive numeric HDS data type string (case
*        insensitive).
*     el
*        Number of array elements to process.
*     pntr
*        Pointer to the array to be processed. On input, an array of
*        variance values is supplied. On output, they are replaced by the
*        standard deviation values. The pointer is not changed.
*     *dce
*        Returned holding the whether a data conversion error occurred
*        resulting in the introduction of new bad values into the array.
*        This will be due to replacement of illegal negative variance
*        values.
*     *status
*        The global status.

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
   char utype[ NDF__SZTYP + 1 ];   /* Upper case data type string */
   int typok;            /* Whether data type string is valid */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the supplied string is not too long, convert it to upper case. */
   typok =  ( strlen(type) < sizeof( utype ) );
   if( typok ) {
      astChrCase( type, utype, 1, sizeof( utype ) );

/* Compare the upper case data type string with each permitted value in
   turn, calling the appropriate function to process the array. */

/* ...Byte data. */
      if( !strcmp( utype, "_BYTE" ) ) {
         ndf1V2sB( bad, el, pntr, dce, status );

/* ...Unsigned byte data. */
      } else if( !strcmp( utype, "_UBYTE" ) ) {
         ndf1V2sUB( bad, el, pntr, dce, status );

/* ...Double precision data. */
      } else if( !strcmp( utype, "_DOUBLE" ) ) {
         ndf1V2sD( bad, el, pntr, dce, status );

/* ...Integer data. */
      } else if( !strcmp( utype, "_INTEGER" ) ) {
         ndf1V2sI( bad, el, pntr, dce, status );

/* ...Real data. */
      } else if( !strcmp( utype, "_REAL" ) ) {
         ndf1V2sF( bad, el, pntr, dce, status );

/* ...Word data. */
      } else if( !strcmp( utype, "_WORD" ) ) {
         ndf1V2sW( bad, el, pntr, dce, status );

/* ...Unsigned word data. */
      } else if( !strcmp( utype, "_UWORD" ) ) {
         ndf1V2sUW( bad, el, pntr, dce, status );

/* ...64-bit integer data. */
      } else if( !strcmp( utype, "_INT64" ) ) {
         ndf1V2sK( bad, el, pntr, dce, status );

/* Note if the data type string was not recognised. */
      } else {
         typok = 0;
      }
   }

/* If the "type" parameter has an invalid value, then report an error. */
   if( *status == SAI__OK ) {
      if( !typok ) {
         *status = NDF__FATIN;
         msgSetc( "ROUTINE", "ndf1V2S" );
         msgSetc( "BADTYPE", type );
         errRep( " ", "Function ^ROUTINE called with an invalid TYPE "
                 "parameter of '^BADTYPE' (internal programming error).",
                 status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1V2S", status );

}

