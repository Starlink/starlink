#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfState_( int indf, const char *comp, int *state, int *status ){
/*
*+
*  Name:
*     ndfState

*  Purpose:
*     Determine the state of an NDF component (defined or undefined).

*  Synopsis:
*     void ndfState( int indf, const char *comp, int *state, int *status )

*  Description:
*     This function returns a logical value indicating whether an NDF
*     component has a defined value (or values).

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the
*        component; any NDF component name is valid.
*     *state
*        Returned holding the whether the specified component is defined.
*     *status
*        The global status.

*  Notes:
*     -  If a component name of "EXTENSION" is given, then a non-zero
*     result will be returned if one or more extensions are present in the
*     NDF.
*     -  A comma-separated list of component names may also be given, in
*     which case the function will return the logical "AND" of the states
*     of the specified components (i.e. a non-zero result will be returned
*     only if all the components have defined values).

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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   char **comps;         /* Array of component name pointers */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number non-blank components specified */
   int nextn;            /* Number of NDF extensions present */

/* Make sure that we return an initialised value */
   *state = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Split the supplied list of components up into words. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {

/* Initialise the result. */
      *state = 1;

/* Loop round the components. */
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the component name with each value in turn (allowing
   abbreviation), and take the appropriate action, or report an error
   if an inappropriate component name has been given. */

/* AXIS component:
   ==============
   Obtain an index to the data object entry in the DCB and ensure that
   axis structure information is available. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "AXIS", NDF__MINAB ) ) {
            if( *state ) {
               dcb = acb->dcb;
               ndf1Da( dcb, status );

/* Use the locator to the first axis structure element to determine the
   state. */
               if( *status == SAI__OK ) *state = ( dcb->aloc[ 0 ] != NULL );
            }

/* DATA component:
   ==============
   Use the ARY_ system to enquire about the state of the NDF's data
   array. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "DATA", NDF__MINAB ) ) {
            if( *state ) aryState( acb->did, state, status );

/* EXTENSION component:
   ===================
   Obtain an index to the data object entry in the DCB and ensure that
   extension (MORE) component information is available. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            if( *state ) {
               dcb = acb->dcb;
               ndf1Dx( dcb, status );
               if( *status == SAI__OK ) {

/* Test if the extension (MORE) structure exists. */
                  *state = ( dcb->xloc != NULL );
                  if( *state ) {

/* If so, then see how many components it has. Set "state" to non-zero only
   if there is at least one component. */
                     datNcomp( dcb->xloc, &nextn, status );
                     if( *status == SAI__OK ) *state = ( nextn > 0 );
                  }
               }
            }

/* HISTORY component:
   =================
   Obtain an index to the data object entry in the DCB and ensure that
   history structure information is available. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "HISTORY", NDF__MINAB ) ) {
            if( *state ) {
               dcb = acb->dcb;
               ndf1Dh( dcb, status );

/* Use the component locator to determine the state. */
               if( *status == SAI__OK ) *state = ( dcb->hloc != NULL );
            }

/* LABEL component:
   ===============
   Obtain an index to the data object entry in the DCB. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            if( *state ) {
               dcb = acb->dcb;

/* Ensure that character component information for the label is
   available in the DCB. */
               ndf1Dc( dcb, NDF__LABEL, status );
               if( *status == SAI__OK ) {

/* Use the component locator to determine the state. */
                  *state = ( dcb->cloc[ NDF__LABEL ] != NULL );
               }
            }

/* QUALITY component:
   ==================
   Inspect the component to determine its state. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "QUALITY", NDF__MINAB ) ) {
            if( *state ) ndf1Qsta( acb, state, status );

/* TITLE component:
   ===============
   Obtain an index to the data object entry in the DCB. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "TITLE", NDF__MINAB ) ) {
            if( *state ) {
               dcb = acb->dcb;

/* Ensure that character component information for the title is
   available in the DCB. */
               ndf1Dc( dcb, NDF__TITLE, status );
               if( *status == SAI__OK ) {

/* Use the component locator to determine the state. */
                  *state = ( dcb->cloc[ NDF__TITLE ] != NULL );
               }
            }

/* UNITS component:
   ===============
   Obtain an index to the data object entry in the DCB. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            if( *state ) {
               dcb = acb->dcb;

/* Ensure that character component information for the units is
   available in the DCB. */
               ndf1Dc( dcb, NDF__UNITS, status );
               if( *status == SAI__OK ) {

/* Use the component locator to determine the state. */
                  *state = ( dcb->cloc[ NDF__UNITS ] != NULL );
               }
            }

/* VARIANCE component:
   ==================
   Inspect the component to determine its state. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            if( *state ) ndf1Vsta( acb, state, status );

/* WCS component:
   ==============
   Inspect the component to determine its state. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "WCS", NDF__MINAB ) ) {
            if( *state ) ndf1Wsta( acb, state, status );

/* If the component name was not recognised, then report an error. */
         } else {
            *status = NDF__CNMIN;
            msgSetc( "BADCOMP", comps[ icomp ] );
            errRep( " ", "Invalid component name '^BADCOMP' specified "
                    "(possible programming error).", status );
         }
      }

/* If no error has occurred, but no non-blank component names have been
   processed, then report an error. */
      if( ( *status == SAI__OK ) && ( ncomp == 0 ) ) {
         *status = NDF__NOCMP;
         errRep( " ", "No component name specified (possible programming "
                 "error).", status );
      }
   }

/* Free the words array. */
   comps = ndf1Freewords( ncomp, comps );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfState: Error determining the state of an NDF "
              "component.", status );
      ndf1Trace( "ndfState", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

