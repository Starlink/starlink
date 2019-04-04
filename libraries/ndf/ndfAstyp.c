#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfAstyp_( const char *type, int indf, const char *comp, int iaxis,
               int *status ){
/*
*+
*  Name:
*     ndfAstyp

*  Purpose:
*     Set a new numeric type for an NDF axis array.

*  Synopsis:
*     void ndfAstyp( const char *type, int indf, const char *comp,
*                    int iaxis, int *status )

*  Description:
*     This function sets a new numeric type for an NDF axis array, causing
*     its data storage type to be changed. If the array"s values are
*     defined, they will be converted from from the old type to the new
*     one. If they are undefined, then no conversion will be necessary.
*     Subsequent enquiries will reflect the new numeric type. Conversion
*     may be performed between any numeric types supported by the NDF_
*     functions.

*  Parameters:
*     type
*        Pointer to a null terminated string holding the new numeric type
*        for the axis array (e.g. "_DOUBLE").
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        array component whose numeric type is to be set: "CENTRE",
*        "VARIANCE" or "WIDTH".
*     iaxis
*        Number of the NDF axis whose array is to be modified.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of axis array component names may also be
*     supplied, in which case the numeric type of each array will be set to
*     the same value in turn.
*     -  A value of zero may be supplied for the "iaxis" parameter, in
*     which case the function will set a new numeric type for the specified
*     component(s) of all the NDF's axes.
*     -  This function may only be used to change the numeric type of an
*     axis array via a base NDF. If an NDF section is supplied, then it
*     will return without action. No error will result.
*     -  The numeric type of an axis array component cannot be changed
*     while it, or any part of it, is mapped for access (e.g. via another
*     NDF identifier). This function will fail, and set a "status" value,
*     if this is the case.
*     -  If the numeric type of an axis array component is to be changed
*     without its values being retained, then a call to ndfArest should be
*     made beforehand.  This will avoid the cost of converting all the
*     values.

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
   char vtype[ NDF__SZTYP + 1 ];   /* Validated numeric data type */
   int cmplx;            /* Whether data type is complex */
   int iax;              /* Loop counter for axes */
   int iax1;             /* First axis to process */
   int iax2;             /* Last axis to process */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number non-blank names specified */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check the data type specification for validity. */
   ndf1Chftp( type, vtype, sizeof( vtype ), &cmplx, status );

/* Check that a complex type has not been specified. Report an error if
   it has. */
   if( *status == SAI__OK ) {
      if( cmplx ) {
         *status = NDF__TYPIN;
         msgSetc( "BADTYPE", vtype );
         errRep( " ", "Invalid numeric type '^BADTYPE' specified; complex "
                 "types are not permitted for axis arrays (possible "
                 "programming error).", status );
      }
   }

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check the axis number for validity. */
   ndf1Van( acb, iaxis, 1, &iax1, &iax2, status );

/* Check that "type" access to the NDF is available. */
   ndf1Chacc( acb, "TYPE", status );

/* Split the supplied list of components up into words. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {

/* Loop over the list of components */
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the array name with each value in turn (allowing
   abbreviation), and take the appropriate action. */

/* CENTRE array:
   ============
   Set a new type for the axis data array(s). */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "CENTRE", NDF__MINAB ) ||
             ndf1Simlr( comps[ icomp ], 1, 0, "CENTER", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               ndf1Adstp( vtype, iax, acb, status );
            }

/* VARIANCE array:
   ==============
   Set a new type for the axis variance array(s). */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               ndf1Avstp( vtype, iax, acb, status );
            }

/* WIDTH array:
   ===========
   Set a new type for the axis width array(s). */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "WIDTH", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               ndf1Awstp( vtype, iax, acb, status );
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

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfAstyp: Error setting a new numeric type for an NDF "
              "axis array.", status );
      ndf1Trace( "ndfAstyp", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

