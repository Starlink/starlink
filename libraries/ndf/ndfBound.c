#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary_err.h"
#include "ndf.h"
#include "mers.h"

void ndfBound_( int indf, int ndimx, hdsdim lbnd[], hdsdim ubnd[],
               int *ndim, int *status ){
/*
*+
*  Name:
*     ndfBound

*  Purpose:
*     Enquire the pixel-index bounds of an NDF.

*  Synopsis:
*     void ndfBound( int indf, int ndimx, hdsdim lbnd[], hdsdim ubnd[],
*                    int *ndim, int *status )

*  Description:
*     This function returns the lower and upper pixel-index bounds of each
*     dimension of an NDF, together with the total number of dimensions.

*  Parameters:
*     indf
*        NDF identifier.
*     ndimx
*        Maximum number of pixel-index bounds to return (i.e. the declared
*        size of the "lbnd" and "ubnd" arguments).
*     lbnd
*        Returned holding the lower pixel-index bounds for each dimension.
*        The supplied "lbnd" array should have at least "ndimx" elements.
*     ubnd
*        Returned holding the upper pixel-index bounds for each dimension.
*        The supplied "ubnd" array should have at least "ndimx" elements.
*     *ndim
*        Returned holding the total number of NDF dimensions.
*     *status
*        The global status.

*  Notes:
*     -  If the NDF has fewer than "ndimx" dimensions, then any remaining
*     elements of the "lbnd" and "ubnd" arguments will be filled with 1"s.
*     -  If the NDF has more than "ndimx" dimensions, then the "ndim"
*     parameter will return the actual number of dimensions. In this case
*     only the first "ndimx" sets of bounds will be returned, except that
*     an error will result if the size of any of the remaining dimensions
*     exceeds 1.
*     -  If this function is called with "status" set, then a value of 1
*     will be returned for all elements of the "lbnd" and "ubnd" arrays and
*     for the "ndim" parameter, although no further processing will occur.
*     The same values will also be returned if the function should fail for
*     any reason.
*     -  The symbolic constant NDF__MXDIM may be used to declare the size
*     of the "lbnd" and "ubnd" arguments so that they will be able to hold
*     the maximum number of NDF bounds that this function can return. This
*     constant is defined in the header file "ndf.h".

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   int i;                /* Loop counter for dimensions */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check inherited global status. */
   if( *status == SAI__OK ) {

/* Import the NDF identifier. */
      ndf1Impid( indf, &acb, status );
      if( *status == SAI__OK ) {

/* Mark the error stack. */
         errMark();

/* Obtain the pixel-index bounds information from the data array
   component. */
         aryBound( acb->did, ndimx, lbnd, ubnd, ndim, status );

/* If the returned status value indicates that significant dimensions
   have been excluded, then annul the error and make a new error report
   referring to the NDF data object. */
         if( *status == ARY__XSDIM ) {
            errAnnul( status );
            *status = NDF__XSDIM;
            ndf1Amsg( "NDF", acb );
            msgSeti( "NDIMX", ndimx );
            errRep( " ", "The NDF structure ^NDF has more than ^NDIMX "
                    "significant dimension(s).", status );
         }

/* Release the error stack. */
         errRlse();
      }

/* If an error occurred, then report context information and call the
   error tracing function. */
      if( *status != SAI__OK ) {
         errRep( " ", "ndfBound: Error enquiring the pixel-index bounds of "
                 "an NDF.", status );
         ndf1Trace( "ndfBound", status );
      }
   }

/* Under error conditions, return "safe" bounds information. */
   if( *status != SAI__OK ) {
      for( i = 0; i < ndimx; i++ ){
         lbnd[ i ] = 1;
         ubnd[ i ] = 1;
      }
      *ndim = 1;
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

