#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfCmplx_( int indf, const char *comp, int *cmplx, int *status ){
/*
*+
*  Name:
*     ndfCmplx

*  Purpose:
*     Determine whether an NDF array component holds complex values.

*  Synopsis:
*     void ndfCmplx( int indf, const char *comp, int *cmplx, int *status )

*  Description:
*     This function returns a logical value indicating whether the
*     specified array component of an NDF holds complex values.

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component: "DATA", "QUALITY" or "VARIANCE".
*     *cmplx
*        Returned holding the whether the component holds complex values.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of components may also be specified, in
*     which case the logical "OR" of the results for each component will be
*     returned.
*     -  The value returned for the QUALITY component is always zero.

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

/* Local variables: */
   NdfACB *acb;          /* Pointer to ACB entry */
   int ncomp;            /* Number non-blank components specified */
   int icomp;            /* Index of current component name */
   char **comps;         /* Array of component name pointers */

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
      *cmplx = 0;

/* Loop over the list of components */
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the component name with each value in turn (allowing
   abbreviation), and take the appropriate action, or report an error
   if an inappropriate component name has been given. */

/* AXIS component.
   ==============
   Report an error, since this component cannot have complex values. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "AXIS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "An AXIS component cannot have complex values "
                    "(possible programming error).", status );

/* DATA component.
   ==============
   If the DATA component was specified, then determine whether the
   NDF's data array is complex from its ARY_ system identifier in the
   ACB. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "DATA", NDF__MINAB ) ) {
            if( !*cmplx ) aryCmplx( acb->did, cmplx, status );

/* EXTENSION.
   =========
   Report an error, since extensions cannot have complex values. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "An EXTENSION cannot have complex values "
                    "(possible programming error).", status );

/* HISTORY component.
   =================
   Report an error, since this component cannot have complex values. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "HISTORY", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A HISTORY component cannot have complex values "
                    "(possible programming error).", status );

/* LABEL component.
   ===============
   Report an error, since this component cannot have complex values. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A LABEL component cannot have complex values "
                    "(possible programming error).", status );

/* QUALITY component.
   =================
   If the QUALITY component was specified, then set a zero result,
   since the quality array is always non-complex. In practice, this
   amounts to doing nothing. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "QUALITY", NDF__MINAB ) ) {


/* TITLE component.
   ===============
   Report an error, since this component cannot have complex values. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "TITLE", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A TITLE component cannot have complex values "
                    "(possible programming error).", status );

/* UNITS component.
   ===============
   Report an error, since this component cannot have complex values. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A UNITS component cannot have complex values "
                    "(possible programming error).", status );

/* VARIANCE component.
   ==================
   Inspect the variance component to see if it contains complex values. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            if( !*cmplx ) ndf1Vcpx( acb, cmplx, status );

/* If the NDF component name was not recognised, then report an error. */
         } else {
            *status = NDF__CNMIN;
            msgSetc( "BADCOMP", comps[ icomp ] );
            errRep( " ", "Invalid array component name '^BADCOMP' "
                    "specified (possible programming error).", status );
         }
      }

/* If no error has occurred, but no non-blank component names have been
   processed, then report an error. */
      if( ( *status == SAI__OK ) && ( ncomp == 0 ) ) {
         *status = NDF__NOCMP;
         errRep( " ", "No array component name specified (possible "
                 "programming error).", status );
      }
   }

/* Free the words array. */
   comps = ndf1Freewords( ncomp, comps );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfCmplx: Error determining whether an NDF array "
              "component holds complex values.", status );
      ndf1Trace( "ndfCmplx", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

