#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Bad( NdfACB *acb, const char *comp, int check, int *bad, int *status ){
/*
*+
*  Name:
*     ndf1Bad

*  Purpose:
*     Determine if an array component of an ACB entry may contain bad
*     pixels.

*  Synopsis:
*     void ndf1Bad( NdfACB *acb, const char *comp, int check, int *bad,
*                   int *status )

*  Description:
*     This function returns a logical value indicating whether an array
*     component of an NDF may contain bad pixels for which checks must be
*     made when the array's values are processed. Only if the returned
*     value is zero can such checks be omitted. If the "check" parameter to
*     this function is set non-zero, then it will also perform an explicit
*     check (if necessary) to see whether bad pixels are actually present.

*  Parameters:
*     acb
*        Pointer to the NDF entry in the ACB.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component; "DATA", "QUALITY" or "VARIANCE".
*     check
*        Whether to perform an explicit check to see whether bad pixels are
*        actually present.
*     *bad
*        Returned holding the whether it is necessary to check for bad
*        pixels when processing the array's values.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied, in
*     which case the function returns the logical "OR" of the results for
*     each component.
*     -  If "check" is set zero, then the returned value of "bad" will
*     indicate whether bad pixels might be present and should therefore be
*     checked for during subsequent processing. However, even if "bad" is
*     returned non-zero in such circumstances, it is still possible that
*     there may not actually be any bad pixels present (for instance, in an
*     NDF section, the region of the array accessed might happen to avoid
*     all the bad pixels).
*     -  If "check" is set non-zero, then an explicit check will be made,
*     if necessary, to ensure that "bad" is only returned non-zero if bad
*     pixels are actually present in the data.
*     -  A non-zero result will be returned for any components whose value
*     is undefined, except in the case of the QUALITY component, for which
*     a zero result is always returned in these circumstances.
*     -  If a component is mapped for access, then the value of "bad" will
*     refer to the actual mapped values. It may differ from its original
*     (unmapped) value if conversion errors occurred during the mapping
*     process or if an initialisation option of "/ZERO" was specified for a
*     component whose value was initially undefined.

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
   char **comps;         /* Array of component name pointers */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number non-blank components specified */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Initialise the result and the component count. */
   *bad = 0;

/* Split the supplied list of components up into words, and loop round
   them all. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the component name with each value in turn (allowing
   abbreviation), and take the appropriate action, or report an error
   if an inappropriate component name has been given. */

/* AXIS component.
   ==============
   Report an error, since this component cannot have bad pixels. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "AXIS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "An AXIS component cannot have bad pixels "
                    "(possible programming error).", status );

/* DATA component:
   ==============
   Determine the bad pixel flag value for the data component. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "DATA", NDF__MINAB ) ) {
            if( !*bad ) ndf1Dbad( acb, check, bad, status );

/* EXTENSION.
   =========
   Report an error, since extensions cannot have bad pixels. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "An EXTENSION cannot have bad pixels (possible "
                    "programming error).", status );

/* HISTORY component.
   =================
   Report an error, since this component cannot have bad pixels. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "HISTORY", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A HISTORY component cannot have bad pixels "
                    "(possible programming error).", status );

/* LABEL component.
   ===============
   Report an error, since this component cannot have bad pixels. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A LABEL component cannot have bad pixels "
                    "(possible programming error).", status );

/* QUALITY component.
   =================
   If the QUALITY component was specified, then set a zero result,
   since bad pixels are disregarded in this component. In practice,
   this amounts to doing nothing. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "QUALITY", NDF__MINAB ) ) {


/* TITLE component.
   ===============
   Report an error, since this component cannot have bad pixels. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "TITLE", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A TITLE component cannot have bad pixels "
                    "(possible programming error).", status );

/* UNITS component.
   ===============
   Report an error, since this component cannot have bad pixels. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A UNITS component cannot have bad pixels "
                    "(possible programming error).", status );

/* VARIANCE component:
   ==================
   Determine the bad pixel flag value for the variance array. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            if( !*bad ) ndf1Vbad( acb, check, bad, status );

/* If the NDF component name was not recognised, then report an error. */
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

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Bad", status );

}

