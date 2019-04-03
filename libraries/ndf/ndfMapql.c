#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"
#include "star/util.h"

void ndfMapql_( int indf, int **pntr, size_t *el, int *bad, int *status ){
/*
*+
*  Name:
*     ndfMapql

*  Purpose:
*     Map the quality component of an NDF as an array of logical values.

*  Synopsis:
*     void ndfMapql( int indf, int **pntr, size_t *el, int *bad, int *status )

*  Description:
*     This function maps the quality component of an NDF for read access,
*     returning a pointer to an array of logical values. Elements of this
*     array are set to non-zero if the bit-wise "AND" of the corresponding
*     quality value and its effective bad-bits mask gives a zero result,
*     indicating that the corresponding NDF pixel may be used in subsequent
*     processing. Other array elements are set to zero, indicating that
*     corresponding NDF pixels should be excluded from subsequent
*     processing.

*  Parameters:
*     indf
*        NDF identifier.
*     *pntr
*        Returned holding the pointer to the mapped array of logical
*        values.
*     *el
*        Returned holding the number of values mapped.
*     *bad
*        Returned holding the this parameter is set to non-zero if any of
*        the mapped values is set to zero (i.e. if any NDF pixels are to be
*        excluded as a consequence of the associated quality values).
*        Otherwise it is set to zero.
*     *status
*        The global status.

*  Notes:
*     -  If the quality component's values are undefined, then this
*     function will return a pointer to an array of non-zero values.
*     -  Note that this function only obtains read access to the quality
*     component; changes made to the mapped values will not be reflected in
*     changes to the NDF's quality values.
*     -  This function disables automatic quality masking, so that
*     subsequent access to other NDF array components via the same
*     identifier will take no account of the possible presence of
*     associated quality values.
*     -  If this function is called with "status" set, then a value of 1
*     will be returned for the "el" parameter, although no further
*     processing will occur.  The same value will also be returned if the
*     function should fail for any reason.

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
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   hdsdim dim;           /* Dimension array */
   int isqual;           /* Whether a quality array must be read */
   unsigned char badbit; /* Effective bad-bits mask value */
   void *qpntr;          /* Pointer to mapped quality values */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check inherited global status. */
   if( *status == SAI__OK ) {

/* Import the NDF identifier. */
      ndf1Impid( indf, &acb, status );
      if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
         dcb = acb->dcb;

/* If the quality component is already mapped through the specified ACB
   entry, then report an error. */
         if( acb->qmap ) {
            *status = NDF__ISMAP;
            ndf1Amsg( "NDF", acb );
            errRep( " ", "The quality component in the NDF structure ^NDF "
                    "is already mapped for access through the specified "
                    "identifier (possible programming error).", status );

/* Obtain the effective bad-bits mask value. */
         } else {
            ndf1Gtbb( acb, &badbit, status );
            if( *status == SAI__OK ) {

/* If the bad-bits mask is zero, then there is no need to read the
   quality array.  If it is non-zero, then check if there is a quality
   array with defined values to read. */
               isqual = ( badbit != 0 );
               if( isqual ) ndf1Qsta( acb, &isqual, status );

/* Obtain the number of pixel values to be mapped from the NDF's data
   array entry in the ACB. */
               arySize( acb->did, el, status );

/* Create a temporary _LOGICAL array of the correct size and map it for
   write access. */
               dim = *el;
               ndf1Temp( "_LOGICAL", 1, &dim, &acb->qmtlc, status );
               datMapI( acb->qmtlc, "WRITE", 1, &dim, &(acb->qmptr), status );
               if( *status == SAI__OK ) {

/* If a quality array has to be read, then map it. Convert the quality
   values into logical values. Then unmap the quality array. */
                  if( isqual ) {
                     aryMap( acb->qid, "_UBYTE", "READ", &qpntr, el, status );
                     ndf1Qmlog( badbit, *el, qpntr, acb->qmptr, bad, status );
                     aryUnmap( acb->qid, status );

/* If there is no quality array to read, then simply generate non-zero
   logical values and note there are no zero values present. */
                  } else {
                     ndf1True( *el, acb->qmptr, status );
                     *bad = 0;
                  }
               }

/* If an error occurred, then unmap and delete the temporary _LOGICAL
   object which may have been acquired. */
               if( *status != SAI__OK ) ndf1Antmp( &acb->qmtlc, status );
            }
         }

/* If no error has occurred, then note that the quality component is
   mapped and increment the DCB count of quality mappings and total
   mappings for the data object. */
         if( *status == SAI__OK ) {
            acb->qmap = 1;
            dcb->nqmap++;
            dcb->nmap++;

/* Reset the quality masking flag, so that quality masking will not
   subsequently be applied automatically to other NDF components when
   they are mapped. */
            acb->qmf = 0;

/* Store the mapping type and access mode. */
            star_strlcpy( acb->qmtyp, "_LOGICAL", sizeof( acb->qmtyp ) );
            star_strlcpy( acb->qmmod, "READ", sizeof( acb->qmmod ) );

/* Return a pointer to the mapped logical values. */
            *pntr = acb->qmptr;
         }
      }

/* If an error occurred, then report context information and call the
   error tracing function. */
      if( *status != SAI__OK ) {
         errRep( " ", "ndfMapql: Error mapping the quality component of an "
                 "NDF as an array of logical values.", status );
         ndf1Trace( "ndfMapql", status );
      }
   }

/* Under error conditions, return a "safe" value of "el". */
   if( *status != SAI__OK ) *el = 1;

/* Restablish the original AST status pointer */
   NDF_FINAL

}
