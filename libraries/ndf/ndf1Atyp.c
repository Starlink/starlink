#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Atyp( int iaxis, NdfACB *acb, const char *comp, int *itype,
               int *status ){
/*
*+
*  Name:
*     ndf1Atyp

*  Purpose:
*     Obtain the numeric type of an NDF axis array identified by its ACB
*     entry.

*  Synopsis:
*     void ndf1Atyp( int iaxis, NdfACB *acb, const char *comp, int *itype,
*                    int *status )

*  Description:
*     This function returns the numeric type of an NDF axis array as a
*     symbolic integer type code. The NDF is identified by its entry in the
*     ACB.

*  Parameters:
*     iaxis
*        Number of the NDF axis for which information is required.
*     acb
*        Pointer to the NDF's entry in the ACB.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        axis array: "CENTRE", "VARIANCE" or "WIDTH".
*     *itype
*        Returned holding the numeric data type code of the axis array; a
*        symbolic constant with a name of the form NDF__TYPx, where x
*        identifies the data type (these constants are defined in the
*        header file "ndf1.h").
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of axis array names may also be supplied to
*     this function. In this case the result returned will be the integer
*     code for the data type with the lowest precision such that the values
*     held in any of the specified arrays can be converted to that type
*     without unnecessary loss of information.
*     -  A value of zero may be given for the "iaxis" parameter. In this
*     case, the results for all the NDF's axes will be combined in the same
*     way as described above.
*

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
   char typea[ NDF__SZTYP + 1 ];   /* Axis array type string */
   int iax;              /* Loop counter for axes */
   int iax1;             /* First axis to process */
   int iax2;             /* Last axis to process */
   int itypea;           /* Integer type code of axis array */
   int mxtype;           /* "Maximised" numeric type code */
   int icomp;            /* Index of current component name */
   int ncomp;            /* Number non-blank names specified */
   int first;            /* First array being processed? */
   char **comps;         /* Array of component name pointers */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check the axis number for validity. */
   ndf1Van( acb, iaxis, 1, &iax1, &iax2, status );

/* Split the supplied list of components up into words, and loop round
   them all. */
   comps = ndf1Findwords( comp, &ncomp, status );
   if( *status == SAI__OK ) {
      first = 1;
      for( icomp = 0; icomp < ncomp; icomp++ ){

/* Compare the axis array name with each value in turn (allowing
   abbreviation), and take the appropriate action. */

/* CENTRE array:
   ============
   Loop through each relevant axis, obtaining the numeric type of its
   data array. */
         if( ndf1Simlr( comps[ icomp ], 1, 0, "CENTRE", NDF__MINAB ) ||
             ndf1Simlr( comps[ icomp ], 1, 0, "CENTER", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               ndf1Adtyp( iax, acb, typea, sizeof( typea ), status );

/* Convert the numeric type string into the corresponding integer type
   code. */
               ndf1Pstyp( typea, &itypea, status );
               if( *status == SAI__OK ) {

/* "itype" accumulates the type code for the numeric type of the result.
   For the first array, simply set its value. */
                  if( first ) {
                     *itype = itypea;
                     first = 0;

/* For subsequent arrays, "maximise" the type code obtained for the
   latest array with that obtained for previous ones. */
                  } else {
                     ndf1Mxtyp( *itype, itypea, &mxtype, status );
                     *itype = mxtype;
                  }
               }
            }

/* VARIANCE array:
   ==============
   Loop through each relevant axis, obtaining the numeric type of its
   variance array. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "VARIANCE", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               ndf1Avtyp( iax, acb, typea, sizeof( typea ), status );

/* Convert the numeric type string into the corresponding integer type
   code. */
               ndf1Pstyp( typea, &itypea, status );
               if( *status == SAI__OK ) {

/* "itype" accumulates the type code for the numeric type of the result.
   For the first array, simply set its value. */
                  if( first ) {
                     *itype = itypea;
                     first = 0;

/* For subsequent arrays, "maximise" the type code obtained for the
   latest array with that obtained for previous ones. */
                  } else {
                     ndf1Mxtyp( *itype, itypea, &mxtype, status );
                     *itype = mxtype;
                  }
               }
            }

/* WIDTH array:
   ===========
   Loop through each relevant axis, obtaining the numeric type of its
   width array. */
         } else if( ndf1Simlr( comps[ icomp ], 1, 0, "WIDTH", NDF__MINAB ) ) {
            for( iax = iax1; iax <= iax2; iax++ ){
               ndf1Awtyp( iax, acb, typea, sizeof( typea ), status );

/* Convert the numeric type string into the corresponding integer type
   code. */
               ndf1Pstyp( typea, &itypea, status );
               if( *status == SAI__OK ) {

/* "itype" accumulates the type code for the numeric type of the result.
   For the first array, simply set its value. */
                  if( first ) {
                     *itype = itypea;
                     first = 0;

/* For subsequent arrays, "maximise" the type code obtained for the
   latest array with that obtained for previous ones. */
                  } else {
                     ndf1Mxtyp( *itype, itypea, &mxtype, status );
                     *itype = mxtype;
                  }
               }
            }

/* If the array name was not recognised, then report an error. */
         } else {
            *status = NDF__CNMIN;
            msgSetc( "BADCOMP", comps[ icomp ] );
            errRep( " ", "Invalid axis array component name '^BADCOMP' "
                    "specified (possible programming error).", status );
         }

      }
   }

/* Free the words array. */
   comps = ndf1Freewords( ncomp, comps );

/* If no error has occurred, but no non-blank array names names have
   been processed, then report an error. */
   if( ( *status == SAI__OK ) && ( ncomp == 0 ) ) {
      *status = NDF__NOCMP;
      errRep( " ", "No axis array component name specified (possible "
              "programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Atyp", status );

}

