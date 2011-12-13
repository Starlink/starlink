#include "sx.h"

Error doperc( int np, float *p, int nel, float *indata, int npos,
              float *inposns, float *outvals );

Error m_SXPercents( Object *in, Object *out ){
/*
*+
*  Name:
*     SXPercents

*  Purpose:
*     searches a histogram of data values for requested percentiles

*  Language:
*     ANSI C

*  Syntax:
*     outputlist, output = SXPercents( input, percents );

*  Classification:
*     Transformation

*  Description:
*     The SXPercents module searches the histogram supplied by "input",
*     for the data values corresponding to the requested percentiles.
*     The "input" histogram should have been created using the "Histogram"
*     module.
*
*     Each value supplied in "percents" should be a value in the range 0.0 to
*     100.0. For each value, P, the corresponding "output" value is the
*     data value below which P percent of the data values in the histogram
*     lie. Linear interpolation is performed to find this value.
*
*     The "outputlist" parameter is a list containing all the individual
*     "output" values.

*  Parameters:
*     input = field (Given)
*        field containing the histogram
*     percents = scalar list (Given)
*        the required percentiles [{5.0,95.0}]
*     outputlist = scalar list (Returned)
*        list containing the data values at all the requested percentiles
*     output = scalar (Returned)
*        data value at a single requested percentile

*  Examples:
*     This example displays the electron density field, with a colour
*     table which ignores the lowest and highest 2 percent of the data
*     values.
*
*        electrondensity = Import("/usr/lpp/dx/samples/data/watermolecule");
*        histogram = Histogram(electrondensity)
*        plist, lo, hi = SXPercents(histogram,{2.0,98.0});
*        camera = AutoCamera(electrondensity);
*        coloured = AutoColor(electrondensity,min=lo,max=hi);
*        Display(coloured,camera);

*  See Also:
*     Histogram, Statistics

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
*     5-OCT-1995 (DSB):
*        Original version
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/


/*  Local Variables: */

      int np,i,nel,rank,shape[4],pnel,npos;
      int nsingle={0};
      char *attr;
      Type type;
      Category cat;
      float defp[ 2 ] = { 5.0, 95.0 };
      float *p, *indata, *inposns, *outvals, *outval;
      Array data, posns;


/*  Check a field has been supplied for parameter 0 ("input"). */

      if( !in[0] ){
         DXSetError( ERROR_BAD_PARAMETER, "Missing parameter \"input\".");
         goto error;
      }

      if( DXGetObjectClass( in[0] ) != CLASS_FIELD ){
         DXSetError( ERROR_DATA_INVALID,
                     "Parameter \"input\" should be a field." );
         goto error;
      }


/*  Get the data component from the input field. */

      data  = (Array) DXGetComponentValue( (Field) in[0], "data" );
      if( !data ) {
         DXSetError( ERROR_MISSING_DATA,
                     "Input field has no data component.");
         goto error;
      }


/*  Get information describing global properties of the input field.
 *  NB; (rank=1,shape=1) is the same as (rank=0) i.e. scalar. */

      if( !DXGetArrayInfo( data, &nel, &type, &cat, &rank, shape ) ){
         DXSetError( ERROR_UNEXPECTED,
                     "Unable to get information about data component.");
         goto error;
      }
      if( rank == 1 && shape[0] == 1 ) rank = 0;


/*  Check the input field contains real (i.e. non-complex), scalar, floating
 *  point data. */

      if( ( type != TYPE_FLOAT ) || ( cat !=CATEGORY_REAL ) ||
          ( rank != 0 ) ){
         DXSetError( ERROR_DATA_INVALID,
                     "Input data should be 1-D array of real floating point values.");
         goto error;
      }


/*  Check that the input field data component is dependant on connections.
 *  NB; since the data is dependant on connections, there should be one
 *  more position than the number of data values. */

      attr = (char *) DXGetString(
                      (String) DXGetComponentAttribute( (Field) in[0],
                                                        "data", "dep" ) );
      if( !attr ){
         DXSetError( ERROR_DATA_INVALID,
                     "Missing data dependancy attribute in parameter \"input\".");
         goto error;
      }

      if( !strcmp( attr, "connections" ) ){
         npos = nel + 1;

      } else {
         DXSetError( ERROR_DATA_INVALID,
                     "Input data (%s) must be dependant on connections.",attr);
         goto error;

      }


/*  Get a pointer the data array. */

      indata = (float *) DXGetArrayData( data );


/*  Get the "positions" component from the input field. */

      posns  = (Array) DXGetComponentValue( (Field) in[0], "positions" );
      if( !posns ) {
         DXSetError( ERROR_MISSING_DATA,
                     "Input field has no positions component.");
         goto error;
      }


/*  Get information about the positions component, remembering that
 *  scalar data can be described either as (rank=1,shape=1) or as
 *  (rank=0).  */

      if( !DXGetArrayInfo( posns, &pnel, &type, &cat, &rank, shape ) ){
         DXSetError( ERROR_UNEXPECTED,
                     "Unable to get information about positions component.");
         goto error;
      }
      if( rank == 1 && shape[0] == 1 ) rank = 0;


/*  Check that the positions component is consistent with the data
 *  component. */

      if( ( type != TYPE_FLOAT ) || ( cat !=CATEGORY_REAL ) ||
          ( rank != 0 ) || ( pnel != npos ) ){
         DXSetError( ERROR_DATA_INVALID,
                     "Input \"positions\" component does not match \"data\" component.");
         goto error;
      }


/*  Get a pointer to the positions array. */

      inposns = (float *) DXGetArrayData( posns );


/*  If the second input parameter ("Percents") has not been supplied, use
 *  5% and 95% as the defaults. */

      if( !in[1] ){
         np = 2;
         p = defp;


/*  If the second parameter was supplied, get a pointer to an array of
 *  floating point values supplied by it. Local variable np is returned
 *  holding the number of values supplied. */

      } else {
         p = SXGet1r( "percents", in[1], &np );
         if( p == NULL ) goto error;
      }


/*  Make a new scalar array structure to hold the list of all output data
 *  values. This is assigned to output parameter "valuelist". A pointer
 *  to the array storage is returned. */

      outvals = SXPut1r( "valuelist", np, &out[0] );


/*  Fill the array with the percentile values. */

      if( !doperc( np, p, nel, indata, npos, inposns, outvals ) ) goto error;


/*  Store a single output data value in each of the remaining output
 *  parameters. */

      for( nsingle=0; nsingle<np; nsingle++ ){
         if( out[ nsingle + 1 ] == NULL ){
            outval = SXPut1r( "value", 1, &out[ nsingle + 1 ] );
            if( !outval ) goto error;
            *outval = *(outvals + nsingle);
         } else {
            break;
         }
      }

/*  Return without error. */

      return OK;


/*  If an error has occurred, arrive here. Delete all objects created by
 *  this module. */

error:

      for(i=0; i<nsingle+1; i++){
         DXDelete( out[i] );
         out[i] = NULL;
      }

      return ERROR;

}


Error doperc( int np, float *p, int nel, float *indata, int npos,
              float *inposns, float *outvals ){
/*
*+
*  Name:
*     doperc

*  Purpose:
*     Find the requested percentiles using the supplied histogram

*  Language:
*     ANSI C

*  Prototype:
*     doperc( int np, float *p, int nel, float *indata, int npos,
*             float *inposns, float *outvals );

*  Returned:
*     Error

*  Description:

*  Arguments:
*     int np
*        The number of percentiles required.
*     float *p
*        Pointer to an array containing the percentage values at which
*        the corresponding data values are required.
*     int nel
*        The number of bins in the histogram
*     float *indata
*        Point to the histogram array.
*     int npos
*        The size of the positions array.
*     float *inposns
*        Pointer to an array holding the data values which defined each
*        bin of the histogram.
*     float *outvals
*        A pointer to an array in which to store the returned data values.

*  Returned:
*     Error doperc
*        Returns OK (non-zero) if the outval array was succefully filled.
*        Otherwise returns ERROR (0).

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-AUG-1995 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     -  It would be faster to sort the percentiles before finding them,
*     because then they could all be found on a single pass through the
*     histogram array.
*     {note_any_bugs_here}

*-
*/

      float sum,pval,target,*ip,psum,lsum,jfrac;
      int i,j,jlo,jhi;


/*  Find the total data sum in the histogram. */

      sum = 0.0;
      for( i=0; i<nel; i++ ) {
         sum += indata[i];
      }


/*  Loop round each percentage value. This is not the fastest way of doing
 *  this! */

      for( i=0; i<np; i++ ){
         pval = p[i];


/*  Check the percentage is between 0 and 100. */

         if( pval < 0.0 || pval > 100.0 ) {
            DXSetError( ERROR_UNEXPECTED,
                        "Bad percentage value (%f) given.", pval );
            return( ERROR );
         }


/*  Go through the histogram array until the accumulated data sum exceeds
 *  the target value (i.e. the current percentage of the total data sum),
 *  or the end of the histogram array is reached. */

         target = 0.01*pval*sum;

         psum = indata[0];
         lsum = psum*2;
         j = 0;

         while( psum <= target && j < nel ){
            j++;
            lsum = psum;
            psum += indata[j];
         }


/*  If the percentile was found immediately, use the smallest data value. */

         if( j == 0 ){
            outvals[ i ] = inposns[0];


/*  If the perentile was  found mid-histogram, do linear interpolation
 *  to find the data value. */

         } else if( j < nel ) {
            outvals[ i ] = inposns[j] +
                       ( inposns[j+1] - inposns[j] )*( target - lsum )/
                                                     ( psum - lsum );

/*  If the percentile was not found, use the largest data value. */

         } else {
            outvals[ i ] = inposns[npos];

         }

      }

      return( OK );
}
