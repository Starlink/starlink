#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf_ast.h"
#include "mers.h"

void ndf1S2v( int bad, const char *type, size_t el, void *pntr, int *dce,
              int *status ){
/*
*+
*  Name:
*     ndf1S2v

*  Purpose:
*     Convert standard deviation values to variances.

*  Synopsis:
*     void ndf1S2v( int bad, const char *type, size_t el, void *pntr,
*                   int *dce, int *status )

*  Description:
*     This function converts a vectorised array of standard deviation
*     values into variances by squaring them. It will check for "bad"
*     values in the array if required. If a negative standard deviation is
*     found, then "status" is set to NDF__NGSTD, an error is reported and a
*     bad value is assigned to the affected array element - however, the
*     function continues to process the entire array.

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
*        standard deviation values is supplied. On output, they are
*        replaced by the variance values. The pointer is not changed.
*     *dce
*        Returned holding the whether a data conversion error occurred
*        resulting in new bad values being produced. This may result either
*        from numerical overflow (which the function handles) or from
*        replacement of illegal negative standard deviations.
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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   char utype[ NDF__SZTYP + 1 ];   /* Upper case data type string */
   int typok;                      /* Whether data type string is valid */

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
         ndf1S2vB( bad, el, pntr, dce, status );

/* ...Unsigned byte data. */
      } else if( !strcmp( utype, "_UBYTE" ) ) {
         ndf1S2vUB( bad, el, pntr, dce, status );

/* ...Double precision data. */
      } else if( !strcmp( utype, "_DOUBLE" ) ) {
         ndf1S2vD( bad, el, pntr, dce, status );

/* ...Integer data. */
      } else if( !strcmp( utype, "_INTEGER" ) ) {
         ndf1S2vI( bad, el, pntr, dce, status );

/* ...Real data. */
      } else if( !strcmp( utype, "_REAL" ) ) {
         ndf1S2vF( bad, el, pntr, dce, status );

/* ...Word data. */
      } else if( !strcmp( utype, "_WORD" ) ) {
         ndf1S2vW( bad, el, pntr, dce, status );

/* ...Unsigned word data. */
      } else if( !strcmp( utype, "_UWORD" ) ) {
         ndf1S2vUW( bad, el, pntr, dce, status );

/* ...64-bit integer data. */
      } else if( !strcmp( utype, "_INT64" ) ) {
         ndf1S2vK( bad, el, pntr, dce, status );

/* Note if the data type string was not recognised. */
      } else {
         typok = 0;
      }
   }

/* If the "type" parameter has an invalid value, then report an error. */
   if( *status == SAI__OK ) {
      if( !typok ) {
         *status = NDF__FATIN;
         msgSetc( "ROUTINE", "ndf1S2V" );
         msgSetc( "BADTYPE", type );
         errRep( " ", "Function ^ROUTINE called with an invalid TYPE "
                 "parameter of '^BADTYPE' (internal programming error).",
                 status );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1S2V", status );

}

