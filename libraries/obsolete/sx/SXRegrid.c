#include <dx/dx.h>
#include "sx.h"

#define MAXFLD 20

extern Fpair *head;

Error m_SXRegrid( Object *in, Object *out ){
/*
*+
*  Name:
*     SXRegrid

*  Purpose:
*     samples a field at positions defined by a another field

*  Language:
*     ANSI C

*  Syntax:
*     output = SXRegrid( input, grid, nearest, radius, scale, exponent,
*                        coexp, type );

*  Classification:
*     Realisation

*  Description:
*     The SXRegrid module samples the "data" component of the "input"
*     field at the positions held in the "positions" component of the
*     "grid" field. It is similar to the standard "Regrid" module, but
*     provides more versatility in assigning weights to each input position,
*     the option of returning the sums of the weights or the weighted sum
*     instead of the weighted mean, and seems to be much faster. Both
*     supplied fields can hold scattered or regularly gridded points, and
*     need not contain "connections" components. The "data" component in the
*     "input" field must depend on "positions".
*
*     For each grid position, a set of near-by positions in the input
*     field are found (using "nearest" and "radius"). Each of these input
*     positions is given a weight dependant on its distance from the current
*     grid position. The output data value (defined at the grid position) can
*     be the weighted mean or weighted sum of these input data values, or
*     the sum of the weights (selected by "type").
*
*     The weight for each input position is of the form:
*
*        (d/d0)**exponent
*
*     where "d" is the distance from the current grid position to the
*     current input position. If a single value is given for "scale" then
*     that value is used for the d0 constant for all the near-by input
*     positions. If more than 1 value is given for "scale" then the first
*     value is used for the closest input position, the second value for the
*     next closest, etc. The last supplied value is used for any remaining
*     input positions. A value of zero for "scale" causes the
*     corresponding input position to be given zero weight.
*
*     If "coexp" is not zero, then the above weights are modified to
*     become:
*
*        exp( coexp*( (d/d0)**exponent ) )
*
*     If "nearest" is given an integer value, it specifies N, the maximum
*     number of near-by input positions to use for each output position.
*     The N input positions which are closest to the output position are
*     used. If the string "infinity" is given, then all input positions
*     closer than the distance given by "radius" are used. Using "radius",
*     you may specify a maximum radius (from the output position) within
*     which to find the near-by input positions. If the string "infinity"
*     is given for "radius" then no limit is placed on the radius.

*  Parameters:
*     input = field (Given)
*        field or group with positions to regrid [none]
*     grid = field (Given)
*        grid to use as template [none]
*     nearest = integer or string (Given)
*        number of nearest neighbours to use, or "infinity" [1]
*     radius = scalar or string (Given)
*        radius from grid point to consider, or "infinity" ["infinity"]
*     scale = scalar or vector or scalar list (Given)
*        scale lengths for weights [1.0]
*     exponent = scalar (Given)
*        weighting exponent [1.0]
*     coexp = scalar (Given)
*        exponential co-efficient for weights [0.0]
*     type = integer (Given)
*        type of output values required: 0 - weighted mean, 1 - weighted sum,
*                                        2 - sum of weights [0]
*     output = field (Returned)
*        regridded field

*  Components:
*     All components except the "data" component are copied from the "grid"
*     field. The output "data" component added by this module depends on
*     "positions". An "invalid positions" component is added if any output
*     data values could not be calculated (e.g. if there are no near-by input
*     data values to define the weighted mean, or if the weights are too
*     large to be represented, or if the input grid position was invalid).

*  Examples:
*     This example maps the scattered data described in "CO2.general" onto a
*     regular grid, and displays it. SXRegrid is used to find the data value
*     at the nearest input position to each grid position.
*
*        input = Import("/usr/lpp/dx/samples/data/CO2.general")$
*        frame17 = Select(input,17);
*        camera = AutoCamera(frame17);
*        grid = Construct([-100,-170],deltas=[10,10],counts=[19,34]);
*        regrid = SXRegrid(frame17,grid);
*        coloured = AutoColor(regrid);
*        Display(coloured,camera);
*
*     The next example produces a grid containing an estimate of the density
*     of the scattered points (i.e. the number of points per unit area). The
*     positions of the original scattered points are shown as dim grey
*     circles. SXRegrid finds the 5 closest input positions at each grid
*     position. Zero weight is given to the closest 3 positions. The fourth
*     position has a weight which is half the density of the points within the
*     circle passing through the fourth point (i.e. if the fourth point
*     is at a distance D from the current grid position, there are 3 points
*     within a circle of radius D, so the density within that circle is
*     3/(PI*(D**2)) ). The fifth position has a weight which is half the
*     density of the points within the circle passing through the fifth
*     point. The output data value is the sum of the weights (because
*     "type" is set to 2), which is the mean of the densities within the
*     circles touching the fourth and fifth points.
*
*        input = Import("/usr/lpp/dx/samples/data/CO2.general")$
*        frame17 = Select(input,17);
*        camera = AutoCamera(frame17);
*        glyphs=AutoGlyph(frame17,scale=0.1,ratio=1);
*        glyphs=Color(glyphs,"dim grey");
*        grid = Construct([-100,-170],deltas=[10,10],counts=[19,34]);
*        density=SXRegrid(frame17,grid,nearest=5,scale=[0,0,0,0.691,0.798],
*                         exponent=-2,type=2);
*        coloured = AutoColor(density);
*        collected=Collect(coloured,glyphs);
*        Display(collected,camera);

*  See Also:
*     SXBin, ReGrid, Map, Construct

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
*     3-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


/*  Local Variables: */

      Category cat;          /* Array category type */
      float    coexp;        /* Exponential co-efficient for weights */
      Type     dtype;        /* Type of current input data array */
      float    ext;          /* Amount by which to extend grid bounds */
      int      fld;          /* Field counter */
      Object   grid;         /* The grid object */
      float   *gridpos;      /* Pointer to grid positions array */
      int      i;            /* Loop count */
      void    *indata[MAXFLD];/* Pointer to input data array */
      float   *inpos;        /* Pointer to input positions array */
      Array    inpos_array;  /* Input positions array */
      Object   input;        /* A copy of the input object */
      int      iopt;         /* Index of selected option */
      int      j;            /* Loop count */
      float    lbnd[3];      /* Lower bounds of grid */
      char    *opt;          /* Textual option for a parameter value */
      float    radius;       /* Max. radius for contributing input positions */
      char     more;         /* Are there more input fields to do? */
      int      nearest;       /* Max. no. of input positions which can contribute to an output position */
      Fpair   *next;         /* The next Fpair structure in the linked list */
      int      ndim;         /* No. of dimensions in grid positions array */
      int      nfld;         /* No. of fields sharing current positions array */
      int      npos;         /* No. of input positions */
      int      npindim;      /* No. of dimensions in input positions array */
      int      nsamp;        /* No. of grid positions */
      int      nscale;       /* No. of scale distances supplied */
      int      outbad[MAXFLD];/* No. of invalid output positions in each field*/
      void    *outdata[MAXFLD];/* Pointers to output data arrays */
      Array    outdata_array;/* Output data array */
      Object   output;       /* The output object */
      int      outtype;      /* Type of output values required */
      float    exponent;     /* Power for weights */
      int      rank;         /* Array rank */
      float    rsc;          /* Reciprocal squared scale distance*/
      float   *rscale;       /* Scale distances for weights */
      int      tag;          /* The tag for the current input positions array */
      Type     type;         /* Array numeric type */
      float    ubnd[3];      /* Upper bounds of grid */
      int      veclen[MAXFLD];/* Dimensionality of each input data array */


/*  Initialise all created objects so that they can safely be deleted if
 *  an error occurs. */

      input = NULL;
      output = NULL;
      grid = NULL;
      outdata_array = NULL;


/*  Check that the "input" object has been supplied. */

      if( !in[0] ) {
         DXSetError( ERROR_BAD_PARAMETER, "missing parameter \"input\"." );
         goto error;
      }


/*  Remove (cull) all invalid positions and connections from the input. It is
 *  necessary to take a copy of the input first, because the input object
 *  itself cannot be modified. */

      input = DXCopy( in[0], COPY_STRUCTURE );
      if( !DXCull( input ) ) goto error;


/*  Check that the "grid" object has been supplied. */

      if( !in[1] ) {
         DXSetError( ERROR_BAD_PARAMETER, "missing parameter \"grid\"." );
         goto error;
      }


/*  Get a pointer to an array holding the grid positions, and get the
 *  size and shape of the grid. Any invalid positions are flagged with the
 *  value FLT_MAX (defined in float.h). */

      gridpos = SXGetGrid( in[1], &nsamp, &ndim, lbnd, ubnd, &grid );
      if( !gridpos ) goto error;


/*  Get the number of input positions allowed to contribute to each
 *  output position. */

      if( !in[2] ){
         nearest = 1;

      } else {

         opt = "infinity";
         if( !SXGet0is( "nearest", in[2], INT_MAX, 1, 1, &opt, &nearest, &iopt ) ) goto error;
         if( iopt == 0 ) nearest = INT_MAX;

      }


/*  Get the maximum radius for input positions which contribute to each
 *  output position. */

      if( !in[3] ){
         radius = FLT_MAX;

      } else {

         opt = "infinity";
         if( !SXGet0rs( "radius", in[3], FLT_MAX, 0.0, 1, &opt, &radius, &iopt ) ) goto error;
         if( iopt == 0 ) radius = FLT_MAX;

      }


/*  If a maximum radius has been given, extend the bounds by one radius
 *  at each end to catch some extra input positions. Otherwise, extend
 *  the bounds by 10%. */

      if( radius < FLT_MAX ){
         for( j=0; j<ndim; j++ ){
            lbnd[j] -= radius;
            ubnd[j] += radius;
         }

      } else {
         for( j=0; j<ndim; j++ ){
            ext = 0.1*( ubnd[j] - lbnd[j] );
            lbnd[j] -= ext;
            ubnd[j] += ext;
         }

      }


/*  Get the scale distances used to create weights for each input
 *  position. Convert them to squared reciprocal scale distances. If
 *  no value is supplied for the "scale" parameter, use a single scale
 *  length of 1.0 */

      if( !in[4] ){
         rsc = 1.0;
         rscale = &rsc;
         nscale = 1;

      } else {
         rscale = SXGet1r( "scale", in[4], &nscale );
         if( !rscale ) goto error;

         for( i=0; i<nscale; i++ ) {
            rsc = rscale[i];
            if( rsc != 0.0 ){
               rscale[i] = 1.0/(rsc*rsc);
            } else {
               rscale[i] = 0.0;
            }
         }

      }


/*  Get the exponent used to create weights for each input position. */

      if( !in[5] ){
         exponent = 1.0;
      } else {
         if( !SXGet0rs( "exponent", in[5], FLT_MAX, -FLT_MAX, 0, &opt, &exponent, &iopt ) ) goto error;
      }


/*  Get the co-efficient to used in the exponential when creating weights for
 *  each input position. */

      if( !in[6] ){
         coexp = 0.0;
      } else {
         if( !SXGet0rs( "coexp", in[6], FLT_MAX, -FLT_MAX, 0, &opt, &coexp, &iopt ) ) goto error;
      }


/*  Get the type of output value required. */

      if( !in[7] ){
         outtype = 0;
      } else {
         if( !SXGet0is( "type", in[7], 2, 0, 0, &opt, &outtype, &iopt ) ) goto error;
      }


/*  Produce a copy of the "input" object to use as the output, replacing all
 *  fields within it with the grid field. Also form a linked list of Fpair
 *  structures describing the fields. */

      output = SXMakeOut( input, (Field) grid, 1, 1, 3, "positions" );
      if( !output ) goto error;


/*  Abort if no fields were found. */

      if( !head ) {
         DXSetError( ERROR_DATA_INVALID, "no fields found in \"input\"." );
         goto error;
      }


/*  Go through the list of fields looking for fields which share the same
 *  positions component. */

      more = 1;
      while( more ){


/*  Find the first field with a non-zero positions tag. */

         next = head;
         while( next && !next->postag ) next = next->next;


/*  If no non-zero positions tags were found, we've finished. */

         if( !next ){
            more = 0;
            break;
         }


/*  Find the input positions array. Get its shape, size and type. Check it is
 *  usable. */

         inpos_array = (Array) next->pos;
         if( !DXGetArrayInfo( inpos_array, &npos, &type, &cat, &rank, &npindim ) ) goto error;

         if( type != TYPE_FLOAT ){
            DXSetError( ERROR_DATA_INVALID, "positions component in \"input\" is not of type FLOAT." );
            goto error;
         }

         if( cat != CATEGORY_REAL ){
            DXSetError( ERROR_DATA_INVALID, "positions component in \"input\" is not of category REAL." );
            goto error;
         }

         if( rank > 1 ){
            DXSetError( ERROR_DATA_INVALID, "rank %d positions component found in \"input\".", rank );
            goto error;
         }

         if( rank == 0 ){   /* Scalar data is equivalent to 1-d vector data */
            rank = 1;
            npindim = 1;
         }

         if( npindim != ndim ){
            DXSetError( ERROR_DATA_INVALID, "dimensionality of \"input\" (%d) does not match \"grid\" (%d).", npindim, ndim );
            goto error;
         }


/*  Get a pointer to the positions values. */

         inpos = (float *) DXGetArrayData( inpos_array );


/*  Find all fields which have the same positions tag and the same data
 *  type. */

         tag = next->postag;
         dtype = next->datatype;

         nfld = 0;
         while( next ){

            if( next->postag == tag && next->datatype == dtype ){


/*  Increment the number of fields found so far which share this
 *  positions component. */

               nfld++;

               if( nfld > MAXFLD ){
                  DXSetError( ERROR_MAX, "\"input\" has too many fields.", MAXFLD );
                  goto error;
               }


/*  Store a pointer to the input data array, and its dimensionality. */

              indata[nfld-1] = (void *) DXGetArrayData( (Array) next->data );
              veclen[nfld-1] = next->datalen;


/*  Make a new array to hold the output data values. The output data will
 *  have the same dimensionality as the input data unless the required output
 *  data is "sum of weights" (i.e. if parameter "type" is 2), in which case the
 *  output data will be scalar. */

              if( outtype == 2 ) veclen[nfld-1] = 1;

              outdata_array = DXNewArrayV( dtype, CATEGORY_REAL, 1, &veclen[nfld-1] );
              if( !outdata_array ) goto error;

              if( !DXAddArrayData( outdata_array, 0, nsamp, NULL ) ) goto error;


/*  Get a pointer to the output data array. */

              outdata[nfld-1] = (void *) DXGetArrayData( outdata_array );
              if( !outdata[nfld-1] ) goto error;


/*  Place the new data component in the output field, and indicate that
 *  it now does not need to be deleted explicitly in the event of an error. */

              if( !DXSetComponentValue( next->outfld, "data", (Object) outdata_array ) ) goto error;
              outdata_array = NULL;


/*  Indicate that the data component of the output field has been
 *  changed. */

              DXChangedComponentValues( next->outfld, "data" );


/*  Indicate that the data values are dependant on positions. */

              if( !DXSetComponentAttribute( next->outfld, "data", "dep",
                                        (Object) DXNewString("positions")) ) goto error;

            }

            next = next->next;

         }


/*  Now sample the input data arrays at the output positions, storing the
 *  resulting sample values in the output data arrays. */

         if( dtype == TYPE_FLOAT ){
            if( ! SXSampleF( nfld, ndim, veclen, npos, inpos, (float **)indata,
                             nsamp, gridpos, (float **) outdata, lbnd, ubnd,
                             nearest, radius, rscale, nscale, exponent, coexp,
                             outtype, outbad ) ) goto error;
         } else {
            if( ! SXSampleD( nfld, ndim, veclen, npos, inpos, (double **)indata,
                             nsamp, gridpos, (double **) outdata, lbnd, ubnd,
                             nearest, radius, rscale, nscale, exponent, coexp,
                             outtype, outbad ) ) goto error;
         }


/*  Loop round all the fields that have just been created. */

         next = head;
         fld = 0;
         while( next ){
            if( next->postag == tag ){


/*  Create invalid positions components in each output field which have any
 *  undefined data values */

               if( outbad[fld] ){
                  if( dtype == TYPE_FLOAT ){
                     if( !SXSetInvPosF( (Object) next->outfld, nsamp, veclen[fld],
                                        (float *) outdata[fld], "positions" ) ) goto error;
                  } else {
                     if( !SXSetInvPosD( (Object) next->outfld, nsamp, veclen[fld],
                                        (double *) outdata[fld], "positions" ) ) goto error;
                  }
               }



/*  Complete the construction of this output field. */

               DXEndField( next->outfld );


/*  Increment the field index, and indicate that this input field has
 *  been done. */

               fld++;
               next->postag = 0;

            }

            next = next->next;

         }


      }

error:

/*  Free the storage used to hold the link list of Fpair structures
 *  describing the fields in the "input" object. */

      while( head ){
         next = head->next;
         DXFree( (Pointer) head );
         head = next;
      }


/*  Delete the copy of the input objects. Return the "output" object with a good status. */

      DXDelete( grid );
      DXDelete( input );


/*  If all is OK, return the "output" object with a good status. */

      if( DXGetError() == ERROR_NONE ){
         out[0] = output;
         return( OK );


/*  If an error has occurred, ensure temporary objects are deleted and return
 *  with a bad status. */

      } else {
         DXDelete( (Object) outdata_array );
         DXDelete( output );
         return( ERROR );
      }

}
