#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfSbad_( int bad, int indf, const char *comp, int *status ){
/*
*+
*  Name:
*     ndfSbad

*  Purpose:
*     Set the bad-pixel flag for an NDF array component.

*  Synopsis:
*     void ndfSbad( int bad, int indf, const char *comp, int *status )

*  Description:
*     This function sets the value of the bad-pixel flag for an NDF array
*     component. A call to this function with "bad" set to non-zero
*     declares that the specified component may contain bad pixel values
*     for which checks must be made by algorithms which subsequently
*     process its values. A call with "bad" set to zero declares that there
*     are definitely no bad values present and that subsequent checks for
*     such values may be omitted.

*  Parameters:
*     bad
*        Bad-pixel flag value to be set.
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component; "DATA" or "VARIANCE".
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied, in
*     which case the bad-pixel flag will be set to the same value for each
*     component in turn.
*     -  If a component is mapped for access when this function is called,
*     then the bad-pixel flag will be associated with the mapped values.
*     This information will only be transferred to the actual data object
*     when the component is unmapped (but only if it was mapped for UPDATE
*     or WRITE access). The value transferred may be modified if conversion
*     errors occur during the unmapping process.
*     -  This function has no effect on components which are in an
*     undefined state; the bad-pixel flag for such components always
*     remains set to non-zero (or zero in the case of the QUALITY
*     component).

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
   char **comps;         /* Array of component name pointers */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number non-blank components specified */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check that write access to the NDF is available. */
   ndf1Chacc( acb, "WRITE", status );

/* Split the supplied list of components up into words, and loop round
   them all. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the component name with each value in turn (allowing
   abbreviation), and take the appropriate action, or report an error
   if an inappropriate component name has been given. */

/* AXIS component:
   ==============
   Report an error, since this component has no bad pixel flag. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "AXIS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A bad-pixel flag value cannot be set for an AXIS "
                    "component (possible programming error).", status );

/* DATA component:
   ==============
   If the data component is mapped for access, then modify the bad pixel
   flag for the mapped values and note this has been done. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "DATA", NDF__MINAB ) ) {
            if( acb->dmap ) {
               acb->dmbad = bad;
               acb->dmbmd = 1;

/* Otherwise, use the ARY_ system to set the bad pixel flag value for
   the data array. */
            } else {
               arySbad( bad, acb->did, status );
            }

/* EXTENSION:
   =========
   Report an error, since extensions have no bad pixel flag. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A bad-pixel flag value cannot be set for an "
                    "EXTENSION (possible programming error).", status );

/* HISTORY component:
   =================
   Report an error, since this component has no bad pixel flag. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "HISTORY", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A bad-pixel flag value cannot be set for a "
                    "HISTORY component (possible programming error).", status );

/* LABEL component:
   ===============
   Report an error, since this component has no bad pixel flag. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A bad-pixel flag value cannot be set for a LABEL "
                    "component (possible programming error).", status );

/* QUALITY component:
   =================
   Report an error, since this component has no bad pixel flag. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "QUALITY", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A bad-pixel flag value cannot be set for a "
                    "QUALITY component (possible programming error).", status );

/* TITLE component:
   ===============
   Report an error, since this component has no bad pixel flag. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "TITLE", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A bad-pixel flag value cannot be set for a TITLE "
                    "component (possible programming error).", status );

/* UNITS component:
   ===============
   Report an error, since this component has no bad pixel flag. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A bad-pixel flag value cannot be set for a UNITS "
                    "component (possible programming error).", status );

/* VARIANCE component:
   ==================
   Set the bad pixel flag. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            ndf1Vsbd( bad, acb, status );

/* If the component name is not recognised, then report an error. */
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
      errRep( " ", "ndfSbad: Error setting the bad-pixel flag for an NDF "
              "array component.", status );
      ndf1Trace( "ndfSbad", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

