#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfStype_( const char *ftype, int indf, const char *comp, int *status ){
/*
*+
*  Name:
*     ndfStype

*  Purpose:
*     Set a new type for an NDF array component.

*  Synopsis:
*     void ndfStype( const char *ftype, int indf, const char *comp,
*                    int *status )

*  Description:
*     This function sets a new full type for an NDF array component,
*     causing its storage type to be changed. If the component's values are
*     defined, they will be converted from from the old type to the new
*     one. If they are undefined, then no conversion will be necessary.
*     Subsequent enquiries will reflect the new type. Conversion may be
*     performed between any types supported by the NDF_ functions,
*     including from a non-complex type to a complex type (and vice versa).

*  Parameters:
*     ftype
*        Pointer to a null terminated string holding the new full type
*        specification for the NDF component (e.g. "_REAL" or
*        "COMPLEX_INTEGER").
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the array
*        component whose type is to be set: "DATA" or "VARIANCE".
*     *status
*        The global status.

*  Notes:
*     -  The function may only be used to change the type of a component of
*     a base NDF. If it is called for an NDF which is not a base NDF, then
*     it will return without action. No error will result.
*     -  A comma-separated list of component names may also be supplied, in
*     which case the type of each component will be set to the same value
*     in turn.
*     -  An error will result if a component being modified, or any part of
*     it, is currently mapped for access (e.g. through another identifier).
*     -  If the type of a component is to be changed without its values
*     being retained, then a call to ndfReset should be made beforehand.
*     This will avoid the cost of converting all the values.

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
   char type[ NDF__SZTYP + 1 ];    /* Validated numeric data type */
   int cmplx;            /* Whether data type is complex */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number non-blank components specified */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Check the full data type specification for validity. */
   ndf1Chftp( ftype, type, sizeof( type ), &cmplx, status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check that "type" access to the NDF is available. */
   ndf1Chacc( acb, "TYPE", status );

/* Split the supplied list of components up into words. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {

/* Obtain an index to the data object entry in the DCB. */
      dcb = acb->dcb;

/* Loop over the components */
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the component name with each value in turn (allowing
   abbreviation), and take the appropriate action, or report an error
   if an inappropriate component name has been given. */

/* AXIS component:
   ==============
   Report an error, since this component has no data type. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "AXIS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A new type cannot be set for an AXIS component "
                    "(possible programming error).", status );

/* DATA component:
   ============== */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "DATA", NDF__MINAB ) ) {

/* Check that the data array is not already mapped for access through
   the current ACB entry. Report an error if it is. */
            if( acb->dmap ) {
               *status = NDF__ISMAP;
               ndf1Amsg( "NDF", acb );
               errRep( " ", "The data component in the NDF structure ^NDF "
                       "is already mapped for access through the specified "
                       "identifier (possible programming error).", status );

/* Only take further action if this is a base NDF. Check that the data
   component is not mapped at all. Report an error if it is. */
            } else if( !acb->cut ) {
               if( dcb->ndmap != 0 ) {
                  *status = NDF__ISMAP;
                  ndf1Dmsg( "NDF", dcb );
                  errRep( " ", "The data component in the NDF structure "
                          "^NDF is already mapped for access through "
                          "another identifier (possible programming "
                          "error).", status );

/* Use the ARY_ system to set a new full data type for the data array. */
               } else {
                  aryStype( ftype, acb->did, status );
                  ndf1Cmpac( acb->dcb, "DATA", status );
               }
            }

/* EXTENSION:
   ==========
   Report an error, since the data type of extensions cannot be set
   with this function. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A new type cannot be set for an EXTENSION "
                    "(possible programming error).", status );

/* HISTORY component:
   =================
   Report an error, since this component has no data type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "HISTORY", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A new type cannot be set for a HISTORY component "
                    "(possible programming error).", status );

/* LABEL component:
   ===============
   Report an error, since this component has no data type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A new type cannot be set for a LABEL component "
                    "(possible programming error).", status );

/* QUALITY component:
   =================
   Report an error, since this component's data type cannot be changed. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "QUALITY", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A new type cannot be set for a QUALITY component "
                    "(possible programming error).", status );

/* TITLE component:
   ===============
   Report an error, since this component has no data type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "TITLE", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A new type cannot be set for a TITLE component "
                    "(possible programming error).", status );

/* UNITS component:
   ===============
   Report an error, since this component has no data type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A new type cannot be set for a UNITS component "
                    "(possible programming error).", status );

/* VARIANCE component:
   ==================
   Set a new data type for the variance component. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            ndf1Vsftp( ftype, acb, status );

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
      errRep( " ", "ndfStype: Error setting a new type for an NDF array "
              "component.", status );
      ndf1Trace( "ndfStype", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

