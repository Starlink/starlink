#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

void ndf1Amap( int iaxis, NdfACB *acb, const char *comp, const char *type,
               const char *mmod, void *pntr[], size_t *el, int *status ){
/*
*+
*  Name:
*     ndf1Amap

*  Purpose:
*     Map an NDF axis array for access.

*  Synopsis:
*     void ndf1Amap( int iaxis, NdfACB *acb, const char *comp,
*                    const char *type, const char *mmod, void *pntr[],
*                    size_t *el, int *status )

*  Description:
*     This function obtains mapped access to an NDF axis array, specified
*     by the axis number, the NDF's ACB index and the name of the axis
*     array.

*  Parameters:
*     iaxis
*        Number of the axis to be accessed.
*     acb
*        Pointer to the NDF entry in the ACB.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        array: "CENTRE", "VARIANCE" (or "ERROR") or "WIDTH" (case
*        insensitive).
*     type
*        Pointer to a null terminated string holding the numeric type to be
*        used for accessing the array (case insensitive).
*     mmod
*        Pointer to a null terminated string holding the mapping mode for
*        access: "READ", "UPDATE" or "WRITE" (case insensitive).
*     pntr
*        Returned holding the pointer(s) to the mapped axis array(s).
*     *el
*        Returned holding the number of array elements mapped.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of axis array names may also be given, in
*     which case each will be mapped in turn. The "pntr" array should have
*     sufficient elements to accommodate the returned pointers.

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
   char inopt[ NDF__SZIOP + 1 ];   /* Initialisation option */
   char mode[ NDF__SZMOD + 1 ];    /* Mapping access mode */
   int iax1;             /* First axis number to process */
   int iax2;             /* Last axis number to process (junk) */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number of requested axis array names */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check the axis number for validity. */
   ndf1Van( acb, iaxis, 0, &iax1, &iax2, status );

/* Check the mapping mode for validity. */
   ndf1Vmmd( mmod, mode, sizeof( mode ), inopt, sizeof( inopt ), status );
   if( *status == SAI__OK ) {

/* If an initialisation option was specified, then report an error, as
   these are not permitted when mapping axis arrays. */
      if( astChrLen( inopt ) > 0 ) {
         *status = NDF__MMDIN;
         msgSetc( "OPTION", inopt );
         errRep( " ", "The mapping mode initialisation option '/^OPTION' "
                 "is not permitted when mapping axis arrays (possible "
                 "programming error).", status );
      }
   }

/* Check that the requested mode of NDF access is available. */
   ndf1Chmod( acb, mode, status );

/* Split the supplied list of components up into words, and loop round
   them all. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the array name with each valid value in turn, calling the
   appropriate function to map the array. */

/* CENTRE array:
   ============
   Map the axis data array. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "CENTRE", NDF__MINAB ) ||
             ndf1Simlr( comps[ icomp ], 1, 0, "CENTER", NDF__MINAB ) ) {
            ndf1Admap( iax1, acb, type, mode, pntr + icomp, el, status );

/* ERROR array:
   ===========
   Map the axis variance array with conversion to standard deviations. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "ERRORS", NDF__MINAB ) ) {
            ndf1Avmap( iax1, acb, type, mode, 1, pntr + icomp, el, status );

/* VARIANCE array:
   ==============
   Map the axis variance array directly. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            ndf1Avmap( iax1, acb, type, mode, 0, pntr + icomp, el, status );

/* WIDTH array:
   ===========
   Map the axis width array. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "WIDTH", NDF__MINAB ) ) {
            ndf1Awmap( iax1, acb, type, mode, pntr + icomp, el, status );

/* If the axis array name was not recognised, then report an error. */
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

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Amap", status );

}

