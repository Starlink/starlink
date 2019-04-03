#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include <string.h>
#include "mers.h"

void ndf1Adprp( NdfACB *acb1, int adcpf, NdfDCB *dcb2, int *status ){
/*
*+
*  Name:
*     ndf1Adprp

*  Purpose:
*     Propagate axis data array information from one NDF to another.

*  Synopsis:
*     void ndf1Adprp( NdfACB *acb1, int adcpf, NdfDCB *dcb2, int *status )

*  Description:
*     This function propagates axis data array information from an existing
*     NDF to a new one which is being created. Optionally, only the array
*     attributes may be propagated, rather than their values.

*  Parameters:
*     acb1
*        Pointer to ACB for the input NDF.
*     adcpf
*        Whether axis data array values are to be propagated (as opposed to
*        simply propagating their attributes).
*     dcb2
*        Pointer to the DCB for the output (new) NDF.
*     *status
*        The global status.

*  Prior Requirements:
*     -  If axis data array values are being propagated, then the output
*     NDF should already contain an axis structure but should not contain
*     any axis data arrays. If values are not being propagated, then it
*     should not contain an axis structure.

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
   AryPlace *place;      /* ARY_ placeholder for new axis array */
   NdfDCB *dcb1;         /* Pointer to DCB input data object */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper bounds */
   int iax;              /* Loop counter for axes */
   int ndim;             /* Number of NDF dimensions */
   size_t el;            /* Number of mapped elements */
   void *pntr1;          /* Pointer to mapped input values */
   void *pntr2;          /* Pointer to mapped output values */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Obtain an index to the input data object entry in the DCB. */
   dcb1 = acb1->dcb;

/* Determine the bounds and number of dimensions of the input NDF from
   the ARY_ system identifier for its data array held in the ACB. */
   aryBound( acb1->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );
   if( *status == SAI__OK ) {

/* Set an initial null DCB identifier for each possible output axis
   data array. */
      for( iax = 0; iax < ndim; iax++ ){
         dcb2->adid[ iax ] = NULL;
      }

/* Loop to process each input NDF dimension. */
      for( iax = 0; iax < ndim; iax++ ){

/* Determine the numeric type and storage form of each input axis data
   array, storing the results as the defaults for the new DCB entry.
   Convert the storage form to take account of the axis bounds if
   necessary (since we may be propagating an NDF section and need to
   accommodate the new lower bound). */
         ndf1Adtyp( iax, acb1, dcb2->adtyp[ iax ], sizeof( dcb2->adtyp[ iax ] ),
                    status );
         ndf1Adfrm( iax, acb1, dcb2->adfrm[ iax ], sizeof( dcb2->adfrm[ iax ] ),
                    status );
         ndf1Cbfrm( 1, lbnd + iax, ubnd + iax, dcb2->adfrm[ iax ],
                    sizeof( dcb2->adfrm[ iax ] ), status );

/* If axis data array information is to be propagated and an input axis
   structure exists, then axis data array values must be propagated to
   the output axis structure. */
         if( *status == SAI__OK ) {
            if( adcpf && ( dcb1->adid[ 0 ] ) ) {

/* First test if the input axis data array for the current dimension is
   absent (this may occur if a section is being propagated and it has
   more dimensions than the associated base NDF). If so, then a new
   output axis data arrray must be created and filled with default
   values. */
               if( !dcb1->adid[ iax ] ) {
                  ndf1Adcre( lbnd[ iax ], ubnd[ iax ], iax, dcb2, status );

/* Otherwise, obtain an ARY_ system placeholder for the  new axis data
   array in the appropriate element of the output axis structure. */
               } else {
                  aryPlace( dcb2->aloc[ iax ], "DATA_ARRAY", &place, status );

/* If the input NDF is not a section, then the axis data array can
   simply be copied. */
                  if( !acb1->cut ) {
                     aryCopy( dcb1->adid[ iax ], &place, dcb2->adid + iax,
                              status );

/* Otherwise, the array values must be transferred explicitly.  Test
   the storage form against each valid value and take the appropriate
   action. */

/* PRIMITIVE:
   =========
   If the new array is primitive, then create it. */
                  } else if( !strcmp( dcb2->adfrm[ iax ], "PRIMITIVE" ) ) {
                     aryNewp( dcb2->adtyp[ iax ], 1, ubnd + iax, &place,
                              dcb2->adid + iax, status );

/* Map the axis data array of the input NDF section for reading (this
   causes any necessary extrapolation of values to take place), and the
   new array for writing. */
                     ndf1Admap( iax, acb1, dcb2->adtyp[ iax ], "READ",
                                &pntr1, &el, status );
                     aryMap( dcb2->adid[ iax ], dcb2->adtyp[ iax ],
                             "WRITE", &pntr2, &el, status );

/* Copy the values across and unmap the arrays when done. */
                     ndf1Move( dcb2->adtyp[ iax ], el, pntr1, pntr2, status );
                     ndf1Adump( iax, acb1, status );
                     aryUnmap( dcb2->adid[ iax ], status );

/* SIMPLE:
   ======
   If the new array is simple, then create it. */
                  } else if( !strcmp( dcb2->adfrm[ iax ], "SIMPLE" ) ) {
                     aryNew( dcb2->adtyp[ iax ], 1, lbnd + iax, ubnd + iax,
                             &place, dcb2->adid + iax, status );

/* Map the axis data array of the input NDF section for reading (this
   causes any necessary extrapolation of values to take place), and the
   new array for writing. */
                     ndf1Admap( iax, acb1, dcb2->adtyp[ iax ], "READ",
                                &pntr1, &el, status );
                     aryMap( dcb2->adid[ iax ], dcb2->adtyp[ iax ],
                             "WRITE", &pntr2, &el, status );

/* Copy the values across and unmap the arrays when done. */
                     ndf1Move( dcb2->adtyp[ iax ], el, pntr1, pntr2, status );
                     ndf1Adump( iax, acb1, status );
                     aryUnmap( dcb2->adid[ iax ], status );

/* If an unsupported array storage form was encountered, then report an
   error. */
                  } else {
                     *status = NDF__FATIN;
                     msgSetc( "BADFORM", dcb2->adfrm[ iax ] );
                     errRep( " ", "Invalid axis array storage form "
                             "'BADFORM' encountered in the NDF_ system "
                             "Data Control Block (internal programming "
                             "error).", status );
                  }
               }
            }
         }
      }

/* If an error occurred, then delete any new axis data arrays which may
   have been created. */
      if( *status != SAI__OK ) {
         for( iax = 0; iax < ndim; iax++ ){
            aryDelet( dcb2->adid + iax, status );
         }
      }

/* Note if the new DCB data array information is correct. */
      for( iax = 0; iax < ndim; iax++ ){
         dcb2->kad[ iax ] = ( *status == SAI__OK );
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Adprp", status );

}

