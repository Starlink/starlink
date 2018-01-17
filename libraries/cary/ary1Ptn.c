#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include <string.h>

void ary1Ptn( int bad, int ndim, const hdsdim *lbnda, const hdsdim *ubnda,
              const char *type, const void *pntr, const hdsdim *lsub,
              const hdsdim *usub, const hdsdim *lbndd, const hdsdim *ubndd,
              const char *htype, HDSLoc *loc, int *dce, int *status ) {
/*
*+
*  Name:
*     ary1Ptn

*  Purpose:
*     Write an n-dimensional subregion to an HDS object.

*  Synopsis:
*     void ary1Ptn( int bad, int ndim, const hdsdim *lbnda, const hdsdim *ubnda,
*                   const char *type, const void *pntr, const hdsdim *lsub,
*                   const hdsdim *usub, const hdsdim *lbndd, const hdsdim *ubndd,
*                   const char *htype, HDSLoc *loc, int *dce, int *status )

*  Description:
*     The routine writes to an n-dimensional subregion of a numeric HDS
*     array, taking the data from an n-dimensional subregion of a
*     C array and making use of lower and upper bounds information for
*     both arrays. Data type conversion is performed if necessary, with
*     bad pixel testing if required.

*  Parameters:
*     bad
*        A boolean flag indicating whether it is necessary to test for "bad"
*        values during data type conversion.
*     ndim
*        Number of array (and HDS object) dimensions.
*     lbnda
*        Lower bounds of input array.
*     ubnda
*        Upper bounds of input array.
*     type
*        The data type of the input array; a primitive numeric HDS data
*        type string (case insensitive).
*     pntr
*        Pointer to the input array.
*     lsub
*        Lower bounds of subregion to be written.
*     usub
*        Upper bounds of subregion to be written.
*     lbndd
*        Lower bounds of the HDS object.
*     ubndd
*        Upper bounds of the HDS object.
*     htype
*        The data type of the HDS object; a primitive numeric HDS data
*        type string (case insensitive).
*     loc
*        Locator to the HDS object.
*     dce
*        Returned holinding a flag indicating whether an error occurred
*        during data type conversion.
*     status
*        The global status.

*  Notes:
*     -  It is assumed that the input array and the output data object
*     have the same number of dimensions.  If this is not the case
*     intrinsically, then the "ndim" argument should be set to match
*     whichever object has the larger dimensionality and the dimension
*     bounds of the other object (and possibly of the subregion also)
*     should be padded to match this dimensionality, normally with 1's.
*     It does not matter that the value of "ndim" may not match the
*     actual dimensionality of the HDS object in such cases.
*     -  The lower and upper bounds of the subregion to be written must
*     lie within the bounds of both the input array and the output data
*     object, although the routine does not check for this.
*     -  The output data object must be suitable for vectorisation using
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

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Test the input data type string against each permitted value in turn,
   calling the appropriate routine to write the data subregion. */
   if( !strcasecmp( type, "_BYTE" ) ){
      ary1PtnB( bad, ndim, lbnda, ubnda, pntr, lsub, usub, lbndd, ubndd,
                htype, loc, dce, status );

   } else if( !strcasecmp( type, "_UBYTE" ) ){
      ary1PtnUB( bad, ndim, lbnda, ubnda, pntr, lsub, usub, lbndd,
                 ubndd, htype, loc, dce, status );

   } else if( !strcasecmp( type, "_DOUBLE" ) ){
      ary1PtnD( bad, ndim, lbnda, ubnda, pntr, lsub, usub, lbndd, ubndd,
                htype, loc, dce, status );

   } else if( !strcasecmp( type, "_INTEGER" ) ){
      ary1PtnI( bad, ndim, lbnda, ubnda, pntr, lsub, usub, lbndd, ubndd,
                htype, loc, dce, status );

   } else if( !strcasecmp( type, "_REAL" ) ){
      ary1PtnF( bad, ndim, lbnda, ubnda, pntr, lsub, usub, lbndd, ubndd,
                htype, loc, dce, status );

   } else if( !strcasecmp( type, "_WORD" ) ){
      ary1PtnW( bad, ndim, lbnda, ubnda, pntr, lsub, usub, lbndd, ubndd,
                htype, loc, dce, status );

   } else if( !strcasecmp( type, "_UWORD" ) ){
      ary1PtnUW( bad, ndim, lbnda, ubnda, pntr, lsub, usub, lbndd,
                 ubndd, htype, loc, dce, status );

   } else if( !strcasecmp( type, "_INT64" ) ){
      ary1PtnK( bad, ndim, lbnda, ubnda, pntr, lsub, usub, lbndd, ubndd,
                htype, loc, dce, status );

/* If the string supplied for the ATYPE argument is not valid, then report
   an error. */
   } else {
      *status = ARY__FATIN;
      msgSetc( "BADATYPE", type );
      errRep( " ", "Routine ary1Ptn called with an invalid TYPE argument of"
              "'^BADATYPE' (internal programming error).", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Ptn", status );

}
