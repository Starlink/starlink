#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Ump( NdfACB *acb, const char *comp, int *status ){
/*
*+
*  Name:
*     ndf1Ump

*  Purpose:
*     Unmap an ACB entry or one of its components.

*  Synopsis:
*     void ndf1Ump( NdfACB *acb, const char *comp, int *status )

*  Description:
*     This function unmaps components of an NDF which have previously been
*     mapped for access. The NDF is identified by its ACB entry. The
*     function will either unmap a specified list of components of the NDF
*     (in which case an error will be reported if any of those components
*     has not previously been mapped), or can perform a "wild carded"
*     unmapping operation on all those components which have been mapped
*     (which could be none).

*  Parameters:
*     acb
*        Pointer to the ACB entry to be unmapped.
*     comp
*        Pointer to a null terminated string holding the name(s) of the NDF
*        component(s) to be unmapped; "DATA", "QUALITY", "VARIANCE" or "*"
*        (case insensitive). The last value acts as a wild card, causing
*        all mapped components to be unmapped.
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  If the "AXIS" component is specified, then all mapped axis array
*     will be unmapped. In this case, no error will occur if there are no
*     mapped axis arrays.

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

/*       "Ndf_ACB_dmap"( NDF__MXACB ) = LOGICAL (Read)
            Whether the NDF's data array is mapped for access.
         "Ndf_ACB_qmf"( NDF__MXACB ) = LOGICAL (Write)
            Quality masking flag.
         "Ndf_ACB_vmap"( NDF__MXACB ) = LOGICAL (Read)
            Whether the NDF's variance array is mapped for access. */

/* Local Variables: */
   char **comps;         /* Array of component name pointers */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number non-blank components specified */
   int tstat;            /* Temporary status variable */

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();
   *status = SAI__OK;

/* Split the supplied list of components up into words, and loop round
   them all. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* If a wild card component name was given, then consider all the NDF"s
   components to see if they are mapped.  Call the appropriate function
   to unmap each one. */
         if( !strcmp( comps[ icomp ], "*" ) ) {

/* ...unmap the data array. */
            if( acb->dmap ) ndf1Dump( acb, status );

/* ...unmap the quality array and set the quality masking flag to true
   if successful. */
            if( acb->qmap ) {
               ndf1Qump( acb, status );
               if( *status == SAI__OK ) acb->qmf = 1;
            }

/* ...unmap the variance array. */
            if( acb->vmap ) ndf1Vump( acb, status );

/* ...unmap any axis arrays which may be mapped. */
            ndf1Aump( 0, acb, "*", status );

/* If the component name is not a wild card, then test it against each
   permitted value in turn, calling the appropriate unmapping function. */

/* AXIS component:
   ==============
   Unmap any axis arrays which may be mapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "AXIS", NDF__MINAB ) ) {
            ndf1Aump( 0, acb, "*", status );

/* DATA component:
   ==============
   Unmap the data array. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "DATA", NDF__MINAB ) ) {
            ndf1Dump( acb, status );

/* EXTENSION:
   =========
   Report an error, as an extension cannot be umapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "An EXTENSION cannot be unmapped (possible "
                    "programming error).", status );

/* HISTORY component:
   =================
   Report an error, as this component cannot be unmapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "HISTORY", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A HISTORY component cannot be unmapped (possible "
                    "programming error).", status );

/* LABEL component:
   ===============
   Report an error, as this component cannot be unmapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A LABEL component cannot be unmapped (possible "
                    "programming error).", status );

/* QUALITY component:
   =================
   Unmap the quality array and set the quality masking flag to non-zero
   if successful. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "QUALITY", NDF__MINAB ) ) {
            ndf1Qump( acb, status );
            if( *status == SAI__OK ) acb->qmf = 1;

/* TITLE component:
   ===============
   Report an error, as this component cannot be unmapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "TITLE", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A TITLE component cannot be unmapped (possible "
                    "programming error).", status );

/* UNITS component:
   ===============
   Report an error, as this component cannot be unmapped. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A UNITS component cannot be unmapped (possible "
                    "programming error).", status );

/* VARIANCE component:
   ==================
   Unmap the variance array. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            ndf1Vump( acb, status );

/* If the component name was not recognised, then report an error. */
         } else {
            *status = NDF__CNMIN;
            msgSetc( "BADCOMP", comps[ icomp ] );
            errRep( " ", "Invalid array component name '^BADCOMP' "
                    "specified (possible programming error).", status );
         }
      }
   }

/* Free the words array. */
   comps = ndf1Freewords( ncomp, comps );

/* If no error has occurred, but no non-blank component names have been
   processed, then report an error. */
   if( ( *status == SAI__OK ) && ( ncomp == 0 ) ) {
      *status = NDF__NOCMP;
      errRep( " ", "No array component name specified (possible "
              "programming error).", status );
   }

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Call error tracing function if appropriate. */
      } else {
         ndf1Trace( "ndf1Ump", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();

}

