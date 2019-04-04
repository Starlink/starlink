#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include <string.h>

void ndf1Aump( int iaxis, NdfACB *acb, const char *comp, int *status ){
/*
*+
*  Name:
*     ndf1Aump

*  Purpose:
*     Unmap an axis array for an ACB entry.

*  Synopsis:
*     void ndf1Aump( int iaxis, NdfACB *acb, const char *comp, int *status )

*  Description:
*     This function unmaps an NDF axis array which has previously been
*     mapped for access. The NDF is identified by its ACB entry.  The
*     function will unmap a list of arrays for a specified axis if required
*     (in which case an error will be reported if any of those arrays has
*     not previously been mapped). Alternatively, a "wild card" unmapping
*     operation may be performed on all those axis arrays which have been
*     mapped (which could be none). A similar "wild card" operation can
*     also be used to unmap arrays for all the NDF's axes, by specifying an
*     axis number of zero.

*  Parameters:
*     iaxis
*        Number of the axis whose array is to be unmapped (one-based).  A
*        value of zero may be specified to indicate that mapped arrays on
*        all the NDF's axes are to be unmapped.
*     acb
*        Pointer to the NDF's entry in the ACB.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        axis array to be unmapped: "CENTRE", "VARIANCE", "WIDTH" or "*"
*        (case insensitive).  The last value acts as a wild card, causing
*        all mapped axis arrays to be unmapped.
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

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
   char **comps;         /* Array of component name pointers */
   int iax;              /* Loop counter for axes */
   int iax1;             /* First axis to process */
   int iax2;             /* Last axis to process */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number non-blank names specified */
   int tstat;            /* Temporary status variable */

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* Check the axis number for validity, obtaining the range of axes to be
   processed. */
   *status = SAI__OK;
   ndf1Van( acb, iaxis, 1, &iax1, &iax2, status );

/* Split the supplied list of components up into words, and loop round
   them all. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* If a wild card array name was given, then loop to process all the
   specified axes. */
         if( !strcmp( comp, "*" ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){

/* Consider all the arrays on each axis to see if they are mapped. Call
   the appropriate function to unmap each one. */

/* ...unmap the axis data array. */
               if( acb->admap[ iax ] ) ndf1Adump( iax, acb, status );

/* ...unmap the axis variance array. */
               if( acb->avmap[ iax ] ) ndf1Avump( iax, acb, status );

/* ...unmap the axis width array. */
               if( acb->awmap[ iax ] ) ndf1Awump( iax, acb, status );
            }

/* Otherwise, test the axis array name against each permitted value in
   turn, calling the appropriate unmapping functions. In each case, take
   test if the array is currently mapped only if a "wild card" axis
   specification ("iaxis"=0) was given. */

/* CENTRE array:
   ============
   Unmap the axis data array. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "CENTRE", NDF__MINAB ) ||
                    ndf1Simlr( comps[ icomp ], 1, 0, "CENTER", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ) {
               if( ( iaxis != 0 ) || acb->admap[ iax ] ) ndf1Adump( iax, acb, status );
            }

/* VARIANCE array:
   ==============
   Unmap the axis variance array. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ) {
               if( ( iaxis != 0 ) || acb->avmap[ iax ] ) ndf1Avump( iax, acb, status );
            }

/* WIDTH array:
   ===========
   Unmap the axis width array. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "WIDTH", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ) {
               if( ( iaxis != 0 ) || acb->awmap[ iax ] ) ndf1Awump( iax, acb, status );
            }

/* If the array name is not recognised, then report an error. */
         } else {
            *status = NDF__CNMIN;
            msgSetc( "BADCOMP", comps[ icomp ] );
            errRep( " ", "Invalid axis array component name '^BADCOMP' "
                    "specified (possible programming error).", status );
         }
      }

/* If no error has occurred, but no non-blank array names have been
   processed, then report an error. */
      if( ( *status == SAI__OK ) && ( ncomp == 0 ) ) {
         *status = NDF__NOCMP;
         errRep( " ", "No axis array component name specified (possible "
                 "programming error).", status );
      }
   }

/* Free the words array. */
   comps = ndf1Freewords( ncomp, comps );

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Call the error tracing function if appropriate. */
      } else {
         ndf1Trace( "ndf1Aump", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}

