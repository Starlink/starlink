#include "sx.h"

#define MAXFLD 20

extern Fpair *head;

Error m_SXBin( Object *in, Object *out ){
/*
*+
*  Name:
*     SXBin

*  Purpose:
*     bins a field into a grid defined by a another field

*  Language:
*     ANSI C

*  Syntax:
*     output = SXBin( input, grid, type );

*  Classification:
*     Realization

*  Description:
*     The SXBin module bins the "data" component of the "input" field into
*     the bins defined by the "connections" component of the "grid" field.
*     The input field can hold scattered or regularly gridded points, but
*     the "data" component must depend on "positions". The "grid" field must
*     contain "connections" and "positions" components but need not contain
*     a "data" component. The input"data" component must be either TYPE_FLOAT
*     or TYPE_DOUBLE.
*
*     The "data" component in the "output" field contains either the mean
*     or sum of the "input" data values falling within each connection, or
*     the number of data values falling within each connection, as specified
*     by "type".
*
*     When binning a regular grid into another regular grid, beware of the
*     tendancy to produce artificial large scale structure representing the
*     "beat frequency" of the two grids.

*  Parameters:
*     input = field (Given)
*        field or group with positions to bin [none]
*     grid = field (Given)
*        grid to define the bins [none]
*     type = integer (Given)
*        type of output values required: 0 - mean, 1 - sum,
*                                        2 - count [0]
*     output = field (Returned)
*        bined field

*  Components:
*     All components except the "data" component are copied from the "grid"
*     field. The output "data" component added by this module depends on
*     "connections". An "invalid connections" component is added if any output
*     data values could not be calculated (e.g. if the mean is required of an
*     empty bin).

*  Examples:
*     This example bins the scattered data described in "CO2.general" onto a
*     regular grid, and displays it. SXBin is used to find the mean data
*     value in each grid connection.
*
*        input = Import("/usr/lpp/dx/samples/data/CO2.general")$
*        frame17 = Select(input,17);
*        camera = AutoCamera(frame17);
*        grid = Construct([-100,-170],deltas=[10,10],counts=[19,34]);
*        bin = SXBin(frame17,grid);
*        coloured = AutoColor(bin);
*        Display(coloured,camera);
*
*     This example produces a grid containing an estimate of the density of
*     the scattered points (i.e. the number of points per unit area). The
*     positions of the original scattered points are shown as dim grey
*     circles. SXBin finds the number of input positions in each bin,
*     Measure finds the area of each bin, and Compute divides the counts
*     by the areas to get the densities:
*
*        input = Import("/usr/lpp/dx/samples/data/CO2.general")$
*        frame17 = Select(input,17);
*        camera = AutoCamera(frame17);
*        glyphs = AutoGlyph(frame17,scale=0.1,ratio=1);
*        glyphs = Color(glyphs,"dim grey");
*        grid = Construct([-100,-170],deltas=[40,40],counts=[6,10]);
*        counts = SXBin(frame17,grid,type=2);
*        areas = Measure(counts,"element");
*        density = Compute("$0/$1",counts,areas);
*        coloured = AutoColor(density);
*        collected=Collect(coloured,glyphs);
*        Display(collected,camera);

*  See Also:
*     SXRegrid, Map, Construct, Measure

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
*     9-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


/*  Local Variables: */

      Array    a;            /* Array to hold reduced dimension positions */
      float   *a_ptr;        /* Pointer to reduced dimension positions */
      Category cat;          /* Array category type */
      Type     dtype;        /* Type of current input data array */
      int      fld;          /* Field counter */
      Object   grid;         /* The grid object */
      int      i;            /* Loop count */
      void    *indata[MAXFLD];/* Pointer to input data array */
      float   *inpos;        /* Pointer to input positions array */
      Array    inpos_array;  /* Input positions array */
      Object   input;        /* A copy of the input object */
      Interpolator interp;   /* Interpolator for grid */
      int      j;            /* Loop count */
      Array    map;          /* Map from input position to output bin number */
      int     *map_ptr;      /* Pointer to map */
      char     more;         /* Are there more input fields to do? */
      Fpair   *next;         /* The next Fpair structure in the linked list */
      int      ndim;         /* No. of dimensions in grid positions array */
      int      nfld;         /* No. of fields sharing current positions array */
      int      npos;         /* No. of input positions */
      int      npindim;      /* No. of dimensions in input positions array */
      int      nbin;         /* No. of grid positions */
      int      outbad[MAXFLD];/* No. of invalid output positions in each field*/
      void    *outdata[MAXFLD];/* Pointers to output data arrays */
      Array    outdata_array;/* Output data array */
      Object   output;       /* The output object */
      int      outtype;      /* Type of output values required */
      float   *pa;           /* Pointer to next reduced dimension position */
      float   *pin;          /* Pointer to next full dimension position */
      int      rank;         /* Array rank */
      int      tag;          /* The tag for the current input positions array */
      Type     type;         /* Array numeric type */
      int      veclen[MAXFLD];/* Dimensionality of each input data array */
      int     *work;         /* Pointer to work array */


/*  Initialise all created objects so that they can safely be deleted if
 *  an error occurs. */

      input = NULL;
      output = NULL;
      grid = NULL;
      outdata_array = NULL;
      work = NULL;
      a = NULL;


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


/*  Create an interpolator which identifies the grid connection containing any
 *  given position. */

      interp = SXGetIntp( in[1], &nbin, &ndim, &grid );
      if( !interp ) goto error;


/*  Allocate a work array for use by SXBinD or SXBinF. */

      work = (int *) DXAllocate( sizeof( int )*nbin );
      if( !work ) goto error;


/*  Get the type of output value required. */

      if( !in[2] ){
         outtype = 0;
      } else {
         if( !SXGet0is( "type", in[2], 2, 0, 0, NULL, &outtype, NULL ) ) goto error;
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

         if( npindim < ndim ){
            DXSetError( ERROR_DATA_INVALID, "dimensionality of \"input\" (%d) is less than \"grid\" (%d).", npindim, ndim );
            goto error;
         }


/*  Get a pointer to the positions values. */

         inpos = (float *) DXGetArrayData( inpos_array );


/*  If the number of dimensions in the input positions is greater than the
 *  number of dimensions in the grid, remove trailing dimensions from the
 *  input positions so that they match the dimensionality of the grid. */

         if( npindim > ndim ){
            a = DXNewArrayV( TYPE_FLOAT, CATEGORY_REAL, 1, &ndim );
            if( !DXAddArrayData( a, 0, npos, NULL ) ) goto error;
            a_ptr = (float *) DXGetArrayData( a );

            for( i=0; i<npos; i++ ){
               pin = inpos + i*npindim;
               pa = a_ptr + i*ndim;
               for(j=0;j<ndim;j++) pa[j] = pin[j];
            }

            inpos_array = a;

         } else {
            a = NULL;
         }

/*  Create an array of the same shape and size as the input positions
 *  array, which holds integer identifiers for the grid connections
 *  containing each input position. These identifiers start at 1 and
 *  go upto nbin. Positions returned holding an identifier of zero do
 *  not fall within the supplied grid. */

         map = (Array) DXMap( (Object) inpos_array, (Object) interp,
                              NULL, NULL );
         map_ptr = (int *) DXGetArrayData( map );


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
 *  data is "counts (i.e if parameter "type" is 2), in which case the
 *  output data will be scalar. */

              if( outtype == 2 ) veclen[nfld-1] = 1;

              outdata_array = DXNewArrayV( dtype, CATEGORY_REAL, 1, &veclen[nfld-1] );
              if( !outdata_array ) goto error;

              if( !DXAddArrayData( outdata_array, 0, nbin, NULL ) ) goto error;


/*  Get a pointer to the output data array. */

              outdata[nfld-1] = (void *) DXGetArrayData( outdata_array );
              if( !outdata[nfld-1] ) goto error;


/*  Place the new data component in the output field, and indicate that
 *  it now does not need to be deleted explicitly in the event of an error. */

              if( !DXSetComponentValue( next->outfld, "data", (Object) outdata_array ) ) goto error;
              outdata_array = NULL;

            }

            next = next->next;

         }


/*  Now bin the input data arrays into the output connections, storing the
 *  resulting bin values in the output data arrays. */

         if( dtype == TYPE_FLOAT ){
            if( ! SXBinF( nfld, veclen, npos, (float **)indata, nbin,
                          (float **) outdata, map_ptr, work, outtype,
                          outbad ) ) goto error;
         } else {
            if( ! SXBinD( nfld, veclen, npos, (double **)indata, nbin,
                          (double **) outdata, map_ptr, work, outtype,
                          outbad ) ) goto error;
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
                     if( !SXSetInvPosF( (Object) next->outfld, nbin, veclen[fld],
                                        (float *) outdata[fld], "connections" ) ) goto error;
                  } else {
                     if( !SXSetInvPosD( (Object) next->outfld, nbin, veclen[fld],
                                        (double *) outdata[fld], "connections" ) ) goto error;
                  }
               }


/*  Indicate that the data values are dependant on connections. */

               if( !DXSetComponentAttribute( next->outfld, "data", "dep",
                                        (Object) DXNewString("connections")) ) goto error;


/*  Indicate that the data component of the output field has been
 *  changed. */

               DXChangedComponentValues( next->outfld, "data" );


/*  Complete the construction of this output field. */

               DXEndField( next->outfld );


/*  Increment the field index, and indicate that this input field has
 *  been done. */

               fld++;
               next->postag = 0;

            }

            next = next->next;

         }


/*  Delete the array used to store the reduced dimensionality input positions
 *  (if used). */

         if( a ) {
            DXDelete( (Object) a );
            a = NULL;
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


/*  Free the work array. */

      if( work ) DXFree( (Pointer) work );


/*  Delete the copy of the input and grid objects, and the array used to
 *  store the reduced dimensionality input positions (if used). */

      DXDelete( grid );
      DXDelete( input );
      if( a ) DXDelete( (Object) a );


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
