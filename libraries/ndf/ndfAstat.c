#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "ndf.h"
#include "mers.h"

void ndfAstat_( int indf, const char *comp, int iaxis, int *state, int *status ){
/*
*+
*  Name:
*     ndfAstat

*  Purpose:
*     Determine the state of an NDF axis component (defined or undefined).

*  Synopsis:
*     void ndfAstat( int indf, const char *comp, int iaxis, int *state,
*                    int *status )

*  Description:
*     This function returns a logical value indicating whether a specified
*     NDF axis component has a defined value (or values).

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        component: "CENTRE", "LABEL", "UNITS", "VARIANCE" or "WIDTH".
*     iaxis
*        Number of the NDF axis for which information is required.
*     *state
*        Returned holding the whether the specified component is defined.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of axis component names may also be given,
*     in which case the function will return the logical "AND" of the
*     states of the specified components (i.e. a non-zero result will be
*     returned only if all the components have defined values).
*     -  A value of zero may be given for the "iaxis" parameter, in which
*     case the function will return the logical "AND" of the results for
*     all the NDF's axes.

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char **comps;         /* Array of component name pointers */
   int iax;              /* Loop counter for axes */
   int iax1;             /* First axis to process */
   int iax2;             /* Last axis to process */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number non-blank components specified */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check the axis number for validity. */
   ndf1Van( acb, iaxis, 1, &iax1, &iax2, status );

/* Split the supplied list of components up into words. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {

/* Initialise the result */
      *state = 1;

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Loop over the list of required components. */
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the component name with each value in turn (allowing
   abbreviation), and take the appropriate action, or report an error
   if an inappropriate component name has been given. */

/* CENTRE component:
   ================
   Loop to process all relevant axes. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "CENTRE", NDF__MINAB ) ||
             ndf1Simlr( comps[ icomp ], 1, 0, "CENTER", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               if( *state ) {

/* Ensure that axis data array information is available. */
                  ndf1Dad( iax, dcb, status );
                  if( *status == SAI__OK ) {

/* If the axis data array does not exist, then its state is zero.
   Otherwise, use its identifier to determine its state. */
                     *state = ( dcb->adid[ iax ] != NULL );
                     if( *state ) aryState( dcb->adid[ iax ], state, status );
                  }
               }
            }

/* EXTENSION component:
   ===================
   Not yet supported, so report an error. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            *status = NDF__NOTSP;
            errRep( " ", "Sorry, axis EXTENSIONs are not yet supported.",
                    status );

/* LABEL component:
   ===============
   Loop to process all relevant axes. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               if( *state ) {

/* Ensure that axis character component information is available. */
                  ndf1Dac( iax, NDF__ALAB, dcb, status );
                  if( *status == SAI__OK ) {

/* See whether the label component exists. */
                     *state = ( dcb->acloc[ iax ][ NDF__ALAB ] != NULL );
                  }
               }
            }

/* UNITS component:
   ===============
   Loop to process all relevant axes. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               if( *state ) {

/* Ensure that axis character component information is available. */
                  ndf1Dac( iax, NDF__AUNI, dcb, status );
                  if( *status == SAI__OK ) {

/* See whether the label component exists. */
                     *state = ( dcb->acloc[ iax ][ NDF__AUNI ] != NULL );
                  }
               }
            }

/* VARIANCE component:
   ==================
   Loop to process all relevant axes. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               if( *state ) {

/* Ensure that axis variance array information is available. */
                  ndf1Dav( iax, dcb, status );
                  if( *status == SAI__OK ) {

/* If the axis variance array does not exist, then its state is
   zero.  Otherwise, use its identifier to determine its state. */
                     *state = ( dcb->avid[ iax ] != NULL );
                     if( *state ) aryState( dcb->avid[ iax ], state, status );
                  }
               }
            }

/* WIDTH component:
   ===============
   Loop to process all relevant axes. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "WIDTH", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               if( *state ) {

/* Ensure that axis width array information is available. */
                  ndf1Daw( iax, dcb, status );
                  if( *status == SAI__OK ) {

/* If the axis width array does not exist, then its state is zero.
   Otherwise, use its identifier to determine its state. */
                     *state = ( dcb->awid[ iax ] != NULL );
                     if( *state ) aryState( dcb->awid[ iax ], state, status );
                  }
               }
            }

/* If the component name was not recognised, then report an error. */
         } else {
            *status = NDF__CNMIN;
            msgSetc( "BADCOMP", comps[ icomp ] );
            errRep( " ", "Invalid axis component name '^BADCOMP' specified "
                    "(possible programming error).", status );
         }

      }

/* If no error has occurred, but no non-blank component names have been
   processed, then report an error. */
      if( ( *status == SAI__OK ) && ( ncomp == 0 ) ) {
         *status = NDF__NOCMP;
         errRep( " ", "No axis component name specified (possible "
                 "programming error).", status );
      }
   }

/* Free the words array. */
   comps = ndf1Freewords( ncomp, comps );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfAstat: Error determining the state of an NDF axis "
              "component.", status );
      ndf1Trace( "ndfAstat", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

