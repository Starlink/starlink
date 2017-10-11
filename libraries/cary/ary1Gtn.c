#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"
#include "ary_ast.h"
#include <string.h>

void ary1Gtn( char bad, const char *htype, HDSLoc *loc, int ndim,
              const hdsdim *lbndd, const hdsdim *ubndd, const hdsdim *lsub,
              const hdsdim *usub, const char *atype, const hdsdim *lbnda,
              const hdsdim *ubnda, char pad, HDSLoc *scloc, void *pntr,
              char *dce, int *status ) {
/*
*+
*  Name:
*     ary1Gtn

*  Purpose:
*     Get an n-dimensional subregion from an HDS object.

*  Synopsis:
*     void ary1Gtn( char bad, const char *htype, HDSLoc *loc, int ndim,
*                   const hdsdim *lbndd, const hdsdim *ubndd, const hdsdim *lsub,
*                   const hdsdim *usub, const char *atype, const hdsdim *lbnda,
*                   const hdsdim *ubnda, char pad, HDSLoc *scloc, void *pntr,
*                   char *dce, int *status )

*  Description:
*     This function extracts an n-dimensional subregion of any numeric
*     data type from a primitive numeric HDS array, making use of lower
*     and upper bounds information for both arrays. Data type conversion
*     and scaling is performed if necessary, with bad pixel testing if
*     required. Optionally, the surrounding region of the output array which
*     does not receive data may be padded with "bad" values. The output array
*     which receives the extracted data is passed by pointer.

*  Parameters:
*     bad
*        Whether it is necessary to test for "bad" values during data
*        type conversion.
*     htype
*        The data type of the HDS object. This should be a primitive
*        numeric HDS data type string (case insensitive).
*     loc
*        Locator to the HDS object.
*     ndim
*        Number of object dimensions.
*     lbndd
*        Lower bounds of the HDS object.
*     ubndd
*        Upper bounds of the HDS object.
*     lsub
*        Lower bounds of subregion to be extracted.
*     usub
*        Upper bounds of subregion to be extracted.
*     atype
*        The data type of the output array. This should be a primitive
*        numeric HDS data type string (case insensitive).
*     lbnda
*        Lower bounds of output array.
*     ubnda
*        Upper bounds of output array.
*     pad
*        Whether to fill regions of the output array which do not
*        receive data with "bad" values.
*     scloc
*        Locator to an HDS object containing the scale and zero terms to
*        apply to the stored values. If this is NULL then no scaling will
*        be performed.
*     pntr
*        Pointer to the output array which is to receive the extracted
*        data. The pointer value itself is not changed by this routine,
*        although the array elements are.
*     dce
*        Returned holding a flag indicating whether an error occurred during
*        data type comversion.
*     status
*        The global status.

*  Notes:
*     -  It is assumed that the input data object and the output array
*     have the same number of dimensions. If this is not the case
*     intrinsically, then the "ndim" argument should be set to match
*     whichever object has the larger dimensionality and the dimension
*     bounds of the other object (and possibly of the subregion also)
*     should be padded to match this dimensionality, normally with 1's.
*     It does not matter that the value of "ndim" may not match the
*     actual dimensionality of the HDS object in such cases.
*     -  The lower and upper bounds of the subregion to be extracted
*     must lie within the bounds of both the input data object and the
*     output array, although the routine does not check for this.
*     -  The input data object must be suitable for vectorisation using
*     the HDS routine datVec.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Local variables: */
   char type[ARY__SZTYP+1];    /* Upper case data type for output array */
   char typok;                 /* Is the supplied atype value valid? */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* If the supplied string is not too long, convert it to upper case. */
   typok =  ( strlen(atype) <= ARY__SZTYP );
   if( typok ) {
      astChrCase( atype, type, 1, ARY__SZTYP + 1 );

/* Test the output data type string against each permitted value in turn,
   calling the appropriate routine to extract the data subregion. */
      if( !strcmp( type,  "_BYTE" ) ){
         ary1GtnB( bad, htype, loc, ndim, lbndd, ubndd, lsub, usub, lbnda,
                   ubnda, pad, scloc, pntr, dce, status );

      } else if( !strcmp( type,  "_UBYTE" ) ){
         ary1GtnUB( bad, htype, loc, ndim, lbndd, ubndd, lsub, usub, lbnda,
                    ubnda, pad, scloc, pntr, dce, status );

      } else if( !strcmp( type,  "_DOUBLE" ) ){
         ary1GtnD( bad, htype, loc, ndim, lbndd, ubndd, lsub, usub, lbnda,
                   ubnda, pad, scloc, pntr, dce, status );

      } else if( !strcmp( type,  "_INTEGER" ) ){
         ary1GtnI( bad, htype, loc, ndim, lbndd, ubndd, lsub, usub, lbnda,
                   ubnda, pad, scloc, pntr, dce, status );

      } else if( !strcmp( type,  "_REAL" ) ){
         ary1GtnF( bad, htype, loc, ndim, lbndd, ubndd, lsub, usub, lbnda,
                   ubnda, pad, scloc, pntr, dce, status );

      } else if( !strcmp( type,  "_WORD" ) ){
         ary1GtnW( bad, htype, loc, ndim, lbndd, ubndd, lsub, usub, lbnda,
                   ubnda, pad, scloc, pntr, dce, status );

      } else if( !strcmp( type,  "_UWORD" ) ){
         ary1GtnUW( bad, htype, loc, ndim, lbndd, ubndd, lsub, usub, lbnda,
                    ubnda, pad, scloc, pntr, dce, status );

      } else if( !strcmp( type,  "_INT64" ) ){
         ary1GtnK( bad, htype, loc, ndim, lbndd, ubndd, lsub, usub, lbnda,
                   ubnda, pad, scloc, pntr, dce, status );

/* Note if the data type string is not recognised. */
      } else {
         typok = 0;
      }
   }

/* If the string supplied for the "atype" argument is not valid, then report
   an error. */
   if( !typok && *status == SAI__OK ){
      *status = ARY__FATIN;
      msgSetc( "BADATYPE", atype );
      errRep( " ", "Function ary1Gtn called with an invalid 'atype' "
              "argument of '^BADATYPE' (internal programming error).",
              status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Gtn", status );

}
