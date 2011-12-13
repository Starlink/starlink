#include "sx.h"

Error m_SXConstruct( Object *in, Object*out){
/*
*+
*  Name:
*     SXConstruct

*  Purpose:
*     constructs a regular field with regular connections

*  Language:
*     ANSI C

*  Syntax:
*     output = SXConstruct( object, lower, upper, deltas, counts );

*  Classification:
*     Realization

*  Description:
*     The SXConstruct module constructs a field with regular positions
*     and connections covering a volume with specified bounds. It is
*     similar to the standard Construct module, but is somewhat easier to
*     use if a simple grid is required.

*     If "object" is given, its bounds define the extent of the output
*     field. Otherwise, the vectors given for "upper" and "lower" define
*     the extent of the output field.

*     If "deltas" is supplied, it defines the distances between adjacent
*     positions on each axis. It should be a vector with the same number
*     of dimensions as "upper" and "lower", or a single value (in which
*     case the supplied value is used for all axes). The upper and lower
*     bounds are expanded if necessary until they span an integer number
*     of deltas.
*
*     If "deltas" is not supplied, then "counts" must be supplied and
*     should be an integer vector giving the number of positions on each
*     axis, or a single integer (in which case the same value is used for
*     all axes).

*  Parameters:
*     object = field (given)
*        object to define extent of new field [none]
*     lower = vector (Given)
*        explicit lower bounds of new field [none]
*     upper = vector (Given)
*        explicit upper bounds of new field [none]
*     deltas = scalar or vector (Given)
*        increment for each axis
*     counts = integer or vector (Given)
*        number of positions along each axis
*     output = field (Returned)
*        output field

*  Components:
*     The output has "positions", "connections" and "box" components, but
*     no "data" component.

*  Examples:
*     This example imports a scattered data set from "C02.general",
*     extracts a single frame, uses SXConstruct to make a grid covering the
*     bounds of the frame, with increments of 10.0 along each axis, and
*     then uses SXBIN to find the mean data value in each of the square
*     connections of this new grid. the resulting field is displayed.
*
*     data = Import("/usr/lpp/dx/samples/data/CO2.general");
*     frame17 = Select(data,17);
*     newgrid = SXConstruct(frame17,deltas=10);
*     binned = SXBin(frame17,newgrid);
*     coloured = AutoColor(binned);
*     camera = AutoCamera(coloured);
*     Display(coloured,camera);

*  See Also:
*     Construct, Grid

*  Returned Value:
*     OK, unless an error occurs in which case ERROR is returned and the
*     DX error code is set.

*  Copyright:
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


/*  Local Variables. */


      int      cnt[3];       /* Counts */
      int     *counts;       /* Pointers to counts */
      float    del[3];       /* Deltas */
      float    del3d[9];     /* 3-D Deltas */
      float   *deltas;       /* Pointers to deltas */
      float   *lower;        /* Pointer to lower bounds */
      float   *upper;        /* Pointer to upper bounds */
      float    lbnd[3];      /* lower bounds */
      float    ubnd[3];      /* upper bounds */
      int      ndim;         /* No. of components for each vector */
      int      ndim2;        /* No. of dimensions in second object */
      Object   o=NULL;       /* Output object */
      float    bnd;          /* Current bound value */
      Point    box[8];       /* Bounding box */
      Category cat;          /* Array category */
      Array    array;        /* An array */
      float   *box_ptr;      /* Pointer to box array */
      int      i;            /* Loop count */
      int      j;            /* Loop count */
      int      ncorn;        /* No. of corners in the bounding box */
      int      rank;         /* Array rank */
      float    tmp;          /* Swapping space */
      Type     type;         /* Array numeric type */


/*  If OBJECT was supplied, check it is a field. */

      if( in[0] ){

         if( DXGetObjectClass(in[0]) != CLASS_FIELD ){
            DXSetError( ERROR_BAD_TYPE, "input object is not a field" );
            goto error;
         }


/*  Take a copy of it. */

         o = DXCopy( in[0], COPY_STRUCTURE );


/*  Get the bounding box component from the object */

         array = (Array) DXGetComponentValue( (Field) o, "box" );


/*  If no bounding box was found, create one. */

         if( !array ){
            if( !DXBoundingBox( o, box ) ){
               DXSetError( ERROR_UNEXPECTED, "cannot obtain a bounding box." );
               goto error;
            }
            array = (Array) DXGetComponentValue( (Field) o, "box" );
         }


/*  Get a pointer to the bounding box array and the number of items in
 *  the array, etc. */

         box_ptr = (float *) DXGetArrayData( array );
         DXGetArrayInfo( array, &ncorn, &type, &cat, &rank, &ndim );

         if( type != TYPE_FLOAT ){
            DXSetError( ERROR_BAD_TYPE, "input object positions are not TYPE_FLOAT");
            goto error;
         }

         if( cat != CATEGORY_REAL ){
            DXSetError( ERROR_BAD_TYPE, "input object positions are not REAL");
            goto error;
         }

         if( rank > 1 ){
            DXSetError( ERROR_BAD_TYPE, "input object positions have rank larger than 1");
            goto error;
         }

         if( rank == 0 ) ndim = 1;

         if( ndim > 3 ){
            DXSetError( ERROR_BAD_TYPE, "input object positions have more than 3 dimensions");
            goto error;
         }


/*  Store the bounds for each dimension. */

         for( j=0; j<ndim; j++){
            lbnd[j] = FLT_MAX;
            ubnd[j] = FLT_MIN;
         }

         for( i=0; i<ncorn; i++ ){
            for( j=0; j<ndim; j++ ){
               bnd = *(box_ptr++);
               if( bnd < lbnd[j] ) lbnd[j]=bnd;
               if( bnd > ubnd[j] ) ubnd[j]=bnd;
            }
         }


/*  Delete the copy of the input object */

         DXDelete( o );
         o = NULL;
         array = NULL;


/*  If OBJECT was not supplied, get the bounds from LOWER and UPPER. */

      } else {

         if( !in[1] ) {
            DXSetError( ERROR_MISSING_DATA, "No lower bounds supplied");
            goto error;

         } else {
            lower = SXGet1r( "lower", in[1], &ndim );
            if( !lower ) goto error;

            if( ndim > 3 ){
               DXSetError( ERROR_BAD_TYPE, "lower bounds have more than 3 dimensions");
               goto error;
            }

         }

         if( !in[2] ) {
            DXSetError( ERROR_MISSING_DATA, "No upper bounds supplied");
            goto error;

         } else {
            upper = SXGet1r( "upper", in[2], &ndim2 );
            if( !upper ) goto error;

            if( ndim2 != ndim ){
               DXSetError( ERROR_BAD_TYPE, "number of upper and lower bounds does not match");
               goto error;
            }

         }

         for( j=0; j<ndim; j++){
            lbnd[j] = lower[j];
            ubnd[j] = upper[j];
         }

      }


/*  Ensure bounds are OK. */

      for( j=0; j<ndim; j++ ){
         if( ubnd[j] < lbnd[j] ) {
            tmp = ubnd[j];
            ubnd[j] = lbnd[j];
            lbnd[j] = tmp;
         }
      }


/*  If DELTAS was supplied, get its values and check dimensionality. */

      if( in[3] ){
         deltas = SXGet1r( "deltas", in[3], &ndim2 );

         if( ndim2 == 1 ){
            for( j=0; j<ndim; j++){
               del[j] = *deltas;
            }

         } else {
            if( ndim2 != ndim ){
               DXSetError( ERROR_BAD_TYPE, "incorrect number of deltas given");
               goto error;
            } else {
               for( j=0; j<ndim; j++){
                  del[j] = deltas[j];
               }
            }
         }


/*  Find the corresponding counts and adjust lower bounds */

         for( j=0; j<ndim; j++ ){
            if( del[j] > 0.0 ){
               cnt[j] = 1 + (int) ( 0.9999 + (ubnd[j]-lbnd[j])/del[j] );
               lbnd[j] = 0.5*( ubnd[j] + lbnd[j] - ( cnt[j] - 1 )*del[j] );
            } else {
               DXSetError( ERROR_BAD_TYPE, "negative or zero delta given");
               goto error;
            }
         }


/*  If COUNTS was supplied, get its values and check dimensionality. */

      } else if( in[4] ){
         counts = SXGet1i( "counts", in[4], &ndim2 );

         if( ndim2 == 1 ){
            for( j=0; j<ndim; j++){
               cnt[j] = *counts;
            }

         } else {
            if( ndim2 != ndim ){
               DXSetError( ERROR_BAD_TYPE, "incorrect number of counts given");
               goto error;
            } else {
               for( j=0; j<ndim; j++){
                  cnt[j] = counts[j];
               }
            }
         }


/*  Find corresponding DELTAS */

         for( j=0; j<ndim; j++ ){
            if( cnt[j] > 1 ) {
               del[j] = ( ubnd[j] - lbnd[j] )/( (float) cnt[j] - 1 );

            } else {
               cnt[j] = 1;
               del[j] = 1.0;
               tmp = 0.5*( ubnd[j]+ lbnd[j] );
               ubnd[j] = tmp;
               lbnd[j] = tmp;
            }

         }


/*  report an error if neither COUNTS nor DELTAS was supplied. */

      } else {
         DXSetError( ERROR_MISSING_DATA, "no deltas or counts given");
         goto error;
      }


/*  Construct the n-d delta vectors, form the increments on each axis. */

      for( j=0; j<9; j++ ) del3d[j] = 0.0;
      for( j=0; j<ndim; j++ ) del3d[ j*(ndim+1) ] = del[j];


/*  Create the output field. */

      o = (Object) DXNewField();
      if( !o ) goto error;


/* Create the positions array and put it in the field. */

      array = DXMakeGridPositionsV( ndim, cnt, lbnd, del3d );
      if( !array ) goto error;
      if( !DXSetComponentValue( (Field) o, "positions", (Object) array ) ) goto error;
      array = NULL;


/* Create the connections array and put it in the field. */

      array = DXMakeGridConnectionsV( ndim, cnt );
      if( !array ) goto error;
      if( !DXSetComponentValue( (Field) o, "connections", (Object) array ) ) goto error;
      array = NULL;


/*  Finish the field */

      if( !DXEndField( (Field) o ) ) goto error;


/*  Return the output field. */

      out[0] = o;
      return( OK );

error:
      DXDelete( o );
      return( ERROR );

}
