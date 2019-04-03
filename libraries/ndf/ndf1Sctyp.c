#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include "star/util.h"

void ndf1Sctyp( NdfACB *acb, const char *comp, int *itype, int *status ){
/*
*+
*  Name:
*     ndf1Sctyp

*  Purpose:
*     Obtain the numeric data type of a scaled NDF array component
*     identified by its ACB entry.

*  Synopsis:
*     void ndf1Sctyp( NdfACB *acb, const char *comp, int *itype, int *status )

*  Description:
*     This function returns the numeric data type of a scaled array
*     component of an NDF as an upper case character string.  The NDF is
*     identified by its entry in the ACB. The returned type describes the
*     values stored in the array, before they are unscaled using the
*     associated scale and zero values. Use ndf1Typ if you need the data
*     type of the array after it has been unscaled.

*  Parameters:
*     acb
*        Pointer to the NDF's entry in the ACB.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component whose type is required; "DATA" or "VARIANCE".
*     *itype
*        Returned holding the numeric data type code of the component; a
*        symbolic constant with a name of the form NDF__SCTYPx, where x
*        identifies the data type (these constants are defined in the
*        header file "ndf1.h").
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of NDF component names may also be supplied
*     to this function. In this case the result returned will be the
*     integer code for the data type with the lowest precision such that
*     the values held in any of the specified components can be converted
*     to that type without loss of numerical precision.

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

/* Local variables: */
   char typec[ NDF__SZTYP + 1 ];   /* Component data type string */
   int itypec;           /* Integer type code of component */
   int mxtype;           /* "Maximised" numeric type code */
   int ncomp;            /* Number non-blank components specified */
   int icomp;            /* Index of current component name */
   char **comps;         /* Array of component name pointers */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

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
   Report an error, since this component does not have a data type. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "AXIS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A numeric type cannot be obtained for an AXIS "
                    "component (possible programming error).", status );

/* DATA component.
   ==============
   Obtain its data type from the ARY_ system data array identifier in
   the ACB. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "DATA", NDF__MINAB ) ) {
            arySctyp( acb->did, typec, status );

/* EXTENSION.
   =========
   Report an error, since extensions do not have numeric data types. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A numeric type cannot be obtained for an "
                    "EXTENSION (possible programming error).", status );

/* HISTORY component.
   =================
   Report an error, since this component does not have a data type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "HISTORY", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A numeric type cannot be obtained for a HISTORY "
                    "component (possible programming error).", status );

/* LABEL component.
   ===============
   Report an error, since this component does not have a data type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A numeric type cannot be obtained for a LABEL "
                    "component (possible programming error).", status );

/* QUALITY component.
   =================
   Set a type value of "_UBYTE", which is the only data type permitted
   for this component. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "QUALITY", NDF__MINAB ) ) {
            star_strlcpy( typec, "_UBYTE", sizeof( typec ) );

/* TITLE component.
   ===============
   Report an error, since this component does not have a data type. */
         } else if( ndf1Simlr( comp, 1, 0, "TITLE", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A numeric type cannot be obtained for a TITLE "
                    "component (possible programming error).", status );

/* UNITS component.
   ===============
   Report an error, since this component does not have a data type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A numeric type cannot be obtained for a UNITS "
                    "component (possible programming error).", status );

/* VARIANCE component.
   ==================
   Inspect the variance component to obtain its numeric data type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            ndf1Vstyp( acb, typec, sizeof( typec ), status );

/* If the NDF component name was not recognised, then report an error. */
         } else {
            *status = NDF__CNMIN;
            msgSetc( "BADCOMP", comps[ icomp ] );
            errRep( " ", "Invalid array component name '^BADCOMP' "
                    "specified (possible programming error).", status );
         }

/* Convert the component data type string into the corresponding
   integer type code. */
         ndf1Pstyp( typec, &itypec, status );

/* "itype" accumulates the type code for the data type of the result. For
   the first component, simply set its value. */
         if( *status == SAI__OK ) {
            if( icomp == 0 ) {
               *itype = itypec;

/* For subsequent components, "maximise" the type code obtained for the
   latest component with that obtained for previous components. */
            } else {
               ndf1Mxtyp( *itype, itypec, &mxtype, status );
               *itype = mxtype;
            }
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
   if( *status != SAI__OK ) ndf1Trace( "ndf1Sctyp", status );

}

