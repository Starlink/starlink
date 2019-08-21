#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfQmf_( int indf, int *qmf, int *status ){
/*
*+
*  Name:
*     ndfQmf

*  Purpose:
*     Obtain the logical value of an NDF's quality masking flag.

*  Synopsis:
*     void ndfQmf( int indf, int *qmf, int *status )

*  Description:
*     This function returns the current value of an NDF's logical quality
*     masking flag. This flag determines whether the NDF's quality
*     component (if present) will be used to generate "bad" pixel values
*     for automatic insertion into the data and variance arrays when these
*     are accessed in READ or UPDATE mode. Normally, this automatic quality
*     masking is used to convert quality information into "bad" pixels so
*     that an application need not consider the quality information
*     explicitly.  If the quality masking flag is set to zero, then
*     automatic masking will not occur so that the application can process
*     the quality component by accessing it directly.

*  Parameters:
*     indf
*        NDF identifier.
*     *qmf
*        Returned holding the value of the quality masking flag.
*     *status
*        The global status.

*  Notes:
*     -  A quality masking flag is associated with each NDF identifier and
*     is initially set to non-zero.  Its value changes to zero whenever the
*     quality component is accessed directly (e.g. using ndfMap or
*     ndfMapql) and reverts to non-zero when access is relinquished (e.g.
*     using ndfUnmap). This default behaviour may also be over-ridden by
*     calling ndfSqmf to set its value explicitly.  ndfQmf allows the
*     current value to be determined.

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
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Obtain the quality masking flag value from the ACB. */
      *qmf = acb->qmf;
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfQmf: Error obtaining the logical value of an NDF's "
              "quality masking flag.", status );
      ndf1Trace( "ndfQmf", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

