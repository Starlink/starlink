#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "star/util.h"
#include "mers.h"

void ndfFtype_( int indf, const char *comp, char *ftype,
               size_t ftype_length, int *status ){
/*
*+
*  Name:
*     ndfFtype

*  Purpose:
*     Obtain the full type of an NDF array component.

*  Synopsis:
*     void ndfFtype( int indf, const char *comp, char *ftype,
*                    size_t ftype_length, int *status )

*  Description:
*     This function returns the full type of one of the array components of
*     an NDF as an upper-case character string (e.g. "_REAL" or
*     "COMPLEX_BYTE").

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component whose type is required: "DATA", "QUALITY" or
*        "VARIANCE".
*     ftype
*        Pointer to an array in which to return a null terminated string
*        holding the full type of the component.
*     ftype_length
*        The length of the supplied 'ftype' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be supplied to
*     this function. In this case the result returned will be the lowest
*     precision full type to which the values held in all the specified
*     components can be converted without unnecessary loss of information.
*     -  The numeric type of a scaled array is determined by the numeric
*     type of the scale and zero terms, not by the numeric type of the
*     underlying array elements.
*     -  The value returned for the QUALITY component is always "_UBYTE".
*     -  The symbolic constant NDF__SZFTP may be used for declaring the
*     length of a character variable to hold the full type of an NDF array
*     component. This constant is defined in the header file "ndf.h".

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   char **comps;         /* Array of component name pointers */
   char *datyp[ NDF__NTYP ];  /* Data */
   char *text;           /* Pointer to dynamically allocated text string */
   char typec[ NDF__SZTYP + 1 ];   /* Numeric data type of component */
   int cmplx;            /* Complex value flag */
   int icomp;            /* Index of current component name */
   int itype;            /* Integer type code of result */
   int itypec;           /* Integer type code of component */
   int mxtype;           /* "Maximised" numeric type code */
   int nc;               /* String length */
   int ncomp;            /* Number non-blank components specified */

/* Local Data: */
   datyp[ NDF__TYPB ] = "_BYTE";
   datyp[ NDF__TYPD ] = "_DOUBLE";
   datyp[ NDF__TYPI ] = "_INTEGER";
   datyp[ NDF__TYPK ] = "_INT64";
   datyp[ NDF__TYPR ] = "_REAL";
   datyp[ NDF__TYPUB ] = "_UBYTE";
   datyp[ NDF__TYPUW ] = "_UWORD";
   datyp[ NDF__TYPW ] = "_WORD";

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Initialise the complex value flag. */
   cmplx = 0;

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
   Report an error, since this component does not have a type. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "AXIS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A full type cannot be obtained for an AXIS "
                    "component (possible programming error).", status );

/* DATA component.
   ==============
   Obtain its full type and complex value flag from the ARY_ system data
   array identifier in the ACB. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "DATA", NDF__MINAB ) ) {
            aryType( acb->did, typec, status );
            if( !cmplx ) aryCmplx( acb->did, &cmplx, status );

/* EXTENSION.
   =========
   Report an error, since extensions do not have a type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "EXTENSION", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A full type cannot be obtained for an "
                    "EXTENSION (possible programming error).", status );

/* HISTORY component.
   =================
   Report an error, since this component does not have a type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "HISTORY", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A full type cannot be obtained for a "
                    "HISTORY component (possible programming "
                    "error).", status );

/* LABEL component.
   ===============
   Report an error, since this component does not have a type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "LABEL", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A full type cannot be obtained for a LABEL "
                    "component (possible programming error).", status );

/* QUALITY component.
   =================
   Set a numeric type value of "_UBYTE", which is the only type
   permitted for this component. The complex value flag is zero, so
   do nothing to change the current value of "cmplx". */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "QUALITY", NDF__MINAB ) ) {
            star_strlcpy( typec, "_UBYTE", sizeof( typec ) );

/* TITLE component.
   ===============
   Report an error, since this component does not have a type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "TITLE", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A full type cannot be obtained for a TITLE "
                    "component (possible programming error).", status );

/* UNITS component.
   ===============
   Report an error, since this component does not have a type. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "UNITS", NDF__MINAB ) ) {
            *status = NDF__CNMIN;
            errRep( " ", "A full type cannot be obtained for a UNITS "
                    "component (possible programming error).", status );

/* VARIANCE component.
   ==================
   Inspect the variance component to obtain its numeric type and complex
   value flag. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            ndf1Vtyp( acb, typec, sizeof( typec ), status );
            if( !cmplx ) ndf1Vcpx( acb, &cmplx, status );

/* If the NDF component name was not recognised, then report an error. */
         } else {
            *status = NDF__CNMIN;
            msgSetc( "BADCOMP", comps[ icomp ] );
            errRep( " ", "Invalid array component name '^BADCOMP' "
                    "specified (possible programming error).", status );
         }

/* Convert the component type string into the corresponding integer type
   code. */
         ndf1Pstyp( typec, &itypec, status );

/* "itype" accumulates the type code for the type of the result. For the
   first component, simply set its value. */
         if( *status == SAI__OK ) {
            if( icomp == 0 ) {
               itype = itypec;

/* For subsequent components, "maximise" the type code obtained for the
   latest component with that obtained for previous components. */
            } else {
               ndf1Mxtyp( itype, itypec, &mxtype, status );
               itype = mxtype;
            }
         }
      }

/* If no error has occurred, but no non-blank component names have been
   processed, then report an error. */
      if( ( *status == SAI__OK ) && ( ncomp == 0 ) ) {
         *status = NDF__NOCMP;
         errRep( " ", "No array component name specified (possible "
                 "programming error).", status );
      }

/* Convert the final type code and complex value flag into a character
   string and return it. */
      if( *status == SAI__OK ) {
         if( cmplx ) {
            text = astAppendStringf( NULL, &nc, "COMPLEX%s", datyp[ itype ]);
            ndf1Ccpy( text, ftype, ftype_length, status );
            text = astFree( text );
         } else {
            ndf1Ccpy( datyp[ itype ], ftype, ftype_length, status );
         }
      }
   }

/* Free the words array. */
   comps = ndf1Freewords( ncomp, comps );

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfFtype: Error obtaining the full type of an NDF "
              "array component.", status );
      ndf1Trace( "ndfFtype", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

