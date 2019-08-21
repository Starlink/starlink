#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfArest_( int indf, const char *comp, int iaxis, int *status ){
/*
*+
*  Name:
*     ndfArest

*  Purpose:
*     Reset an NDF axis component to an undefined state.

*  Synopsis:
*     void ndfArest( int indf, const char *comp, int iaxis, int *status )

*  Description:
*     This function resets an NDF axis component so that its value becomes
*     undefined. It may be used to remove unwanted optional NDF axis
*     components.

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        component to be reset: "LABEL", "UNITS", "VARIANCE" or "WIDTH".
*     iaxis
*        Number of the NDF axis to be modified.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of axis component names may also be
*     supplied, in which case each component will be reset in turn.
*     -  A value of zero may be supplied for the "iaxis" parameter, in
*     which case the same component(s) will be reset on all the NDF"s axes.
*     -  An axis component name of "CENTRE" may not be specified for this
*     function because the pixel centre information cannot be reset for
*     each axis of an NDF individually. This information may only be
*     removed from an NDF by resetting the entire axis component. This can
*     be done by calling the function ndfReset and specifying a component
*     name of "AXIS".
*     -  This function may only be used to reset an axis component via a
*     base NDF. If an NDF section is supplied, then it will return without
*     action. No error will result.
*     -  An NDF axis array component cannot be reset while it is mapped for
*     access, even if this is via another NDF identifier. This function
*     will fail, and set a "status" value, if this is the case.

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

/* Check that WRITE access to the NDF is available. */
   ndf1Chacc( acb, "WRITE", status );

/* Split the supplied list of components up into words, and loop round
   them all. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the component name with each value in turn (allowing
   abbreviation), and take the appropriate action. */

/* CENTRE component:
   ================
   The data array cannot be reset on an individual axis basis, so report
   an error. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "CENTRE", NDF__MINAB ) ||
             ndf1Simlr( comps[ icomp ], 1, 0, "CENTER", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "The CENTRE array for an individual NDF axis "
                    "cannot be reset; use ndfReset to reset the entire "
                    "axis coordinate system (possible programming error).",
                    status );

/* EXTENSION component:
   ===================
   Not yet supported, so report an error. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            *status = NDF__NOTSP;
            errRep( " ", "Sorry, axis EXTENSION components are not yet "
                    "supported.", status );

/* LABEL component:
   ===============
   Reset the component(s). */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               ndf1Acrst( iax, NDF__ALAB, acb, status );
            }

/* UNITS component:
   ===============
   Reset the component(s). */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               ndf1Acrst( iax, NDF__AUNI, acb, status );
            }

/* VARIANCE component:
   ==================
   Reset the axis variance array(s). */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               ndf1Avrst( iax, acb, status );
            }

/* WIDTH component:
   ===============
   Reset the axis width array(s). */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "WIDTH", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               ndf1Awrst( iax, acb, status );
            }

/* If the component name is not recognised, then report an error. */
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
      errRep( " ", "ndfArest: Error resetting an NDF axis component to an "
              "undefined state.", status );
      ndf1Trace( "ndfArest", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

