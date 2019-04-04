#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "star/util.h"
#include "mers.h"

void ndf1Mtyp( const char *typlst, int n, const int ndfs[],
               const char *comp, char *itype, size_t itype_length,
               char *dtype, size_t dtype_length, int *status ){
/*
*+
*  Name:
*     ndf1Mtyp

*  Purpose:
*     Match the data type of an array component of a sequence of NDFs.

*  Synopsis:
*     void ndf1Mtyp( const char *typlst, int n, const int ndfs[],
*                    const char *comp, char *itype, size_t itype_length,
*                    char *dtype, size_t dtype_length, int *status )

*  Description:
*     This function determines the numeric data type of the specified array
*     components of a series of NDFs and checks a comma-separated list of
*     the numeric types supported explicitly by an application, returning
*     the first type from this list which can be used to process the data
*     without loss of information. If no suitable type appears in the list,
*     then an error is reported to this effect. The function also returns
*     the "maximised" numeric type (i.e. the lowest precision type to which
*     all the NDFs may be converted without losing information).

*  Parameters:
*     typlst
*        Pointer to a null terminated string holding a comma-separated list
*        of the numeric types supported by an application (normally in
*        increasing order of computational cost).
*     n
*        The number of NDFs whose numeric data types are to be matched.
*     ndfs
*        Identifiers for the NDFs to be matched. The supplied "ndfs" array
*        should have at least "n" elements.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        array component whose type is to be considered.
*     itype
*        Pointer to an array in which to return a null terminated string
*        holding the numeric data type (selected from the "typlst"
*        parameter) to which the NDF components should be converted for
*        processing by the application. This value is returned as an upper
*        case character string of maximum length NDF__SZTYP.
*     itype_length
*        The length of the supplied 'itype' array. This should include
*        room for the terminating null.
*     dtype
*        Pointer to an array in which to return a null terminated string
*        holding the lowest precision numeric data type to which all the
*        specified NDF components may be converted without loss of
*        information. This value is returned as an upper case character
*        string of maximum length NDF__SZFTP.
*     dtype_length
*        The length of the supplied 'dtype' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Notes:
*     -  A comma-separated list of component names may also be given, in
*     which case all the specified NDF components will be considered when
*     calculating the "maximised" data type.
*     -  If insufficient precision is available using any of the data types
*     specified in "typlst", then "itype" will return the data type with
*     the highest precision in this list as the "best compromise" result.
*     An error will result, however, and a "status" value of NDF__TYPNI
*     (type not implemented) will be returned. This error may simply be
*     annulled if the "best compromise" result is to be used.
*     -  The value of "dtype" returned by this function is intended as
*     input to the ndfStype function, to set the data type of an
*     application's output NDF appropriately to hold the result without
*     loss of information.

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
   NdfACB *acb;          /* Pointer to ACB entry */
   char **elements;      /* Array of type pointers */
   char *datyp[ NDF__MXTYP ];  /* Data */
   int cdtype;           /* Component data type code */
   int eitype;           /* Element implemented type code */
   int i;                /* Loop counter for NDFs */
   int iele;             /* Index of current type */
   int mxdtyp;           /* Max. data type code */
   int mxityp;           /* Max. implemented type code */
   int nele;             /* Number non-blank type list elements */
   int ok;               /* Whether precision is sufficient */
   int ttype;            /* Temporary type code variable */

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

/* Loop to consider each NDF identifier supplied. */
   for( i = 0; i < n; i++ ){

/* Import the identifier and determine the component data type code. */
      ndf1Impid( ndfs[ i ], &acb, status );
      ndf1Typ( acb, comp, &cdtype, status );
      if( *status == SAI__OK ) {

/* If this is the first NDF, then initialise the maximised data type. */
         if( i == 0 ) {
            mxdtyp = cdtype;

/* For subsequent NDFs, form the new maximised data type. */
         } else {
            ndf1Mxtyp( mxdtyp, cdtype, &ttype, status );
            if( *status == SAI__OK ) mxdtyp = ttype;
         }
      }
   }

/* Convert the maximised data type code into a character string and
   return it. */
   ndf1Ccpy( datyp[ mxdtyp ], dtype, dtype_length, status );

/* Split the supplied list of types up into words, and loop round
   them all. */
   elements = ndf1Findwords( typlst, &nele, status );
   ok = 0;
   if( *status == SAI__OK ) {
      for( iele = 0; iele < nele; iele++ ){

/* Convert the string into the element's implemented type code and
   check if it has sufficient precision to process the maximised data
   type without loss of information. */
         ndf1Pstyp( elements[ iele ], &eitype, status );
         ndf1Qityp( mxdtyp, eitype, &ok, status );
         if( *status == SAI__OK ) {

/* If the current element's precision is adequate, then we can break of
   of the type loop, and go on to use the current element. */
            if( ok ) {
               ndf1Ccpy( datyp[ eitype ], itype, itype_length, status );
               break;
            }

/* If this is the first element, then initialise the maximised
   implemented type code. */
            if( iele == 0 ) {
               mxityp = eitype;

/* Subsequently, accumulate the maximum precision type code which occurs
   in the implemented type list. This will be used as the "best
   compromise" result if a suitable implemented data type cannot be
   found. */
            } else {
               ndf1Mxtyp( mxityp, eitype, &ttype, status );
               if( *status == SAI__OK ) mxityp = ttype;
            }

         } else {
            ok = 0;
         }
      }
   }

/* Free the list of types */
   elements = ndf1Freewords( nele, elements );

/* If this point is reached without STATUS being set, then no adequate
   type was found in the implemented type list.  If this is because no
   non-blank component names have been processed, then report an error. */
   if( !ok && *status == SAI__OK ) {
      if( nele == 0 ) {
         *status = NDF__NOTYP;
         errRep( " ", "No implemented type(s) specified (possible "
                 "programming error).", status );

/* If it is because no implemented type had adequate precision, then
   return the type with maximum precision as the "best compromise"
   solution and report an error. */
      } else {
         ndf1Ccpy( datyp[ mxityp ], itype, itype_length, status );
         if( *status == SAI__OK ) {
            *status = NDF__TYPNI;
            msgSetc( "DTYPE", dtype );
            msgSetc( "ITYPE", itype );
            errRep( " ", "NDF array components of type '^DTYPE' cannot be "
                    "processed by this application without loss of "
                    "precision (the best available precision is "
                    "'^ITYPE').", status );
         }
      }
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Mtyp", status );

}

