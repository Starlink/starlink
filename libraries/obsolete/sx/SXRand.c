#include "sx.h"

static Error DoSXRand( Object o, int ndim, int dist, double a,
                       double b );
Array SXRandFill( int nvec, int ndim, int dist, double a, double b );

int zufalli_( int seed );
int zufall_( int n, double *u );
int normalen_( int n, double *g );
int fische_( int n, double mu, int *q );

static entry=0;

Error m_SXRand( Object *in, Object*out){
/*
*+
*  Name:
*     SXRand

*  Purpose:
*     create a set of random vectors

*  Language:
*     ANSI C

*  Syntax:
*     output = SXRand( nvec, ndim, dist, a, b );

*  Classification:
*     Realisation

*  Description:
*     The SXRand module creates a set of random vectors.
*     Each vector has "ndim" components, each chosen independantly from the
*     distibution specified by "dist". The vectors can be returned as a list
*     of "nvec" vectors (if "nvec" is an integer), or as the "data"
*     component of a field (if "nvec" is a field).
*
*     If "dist" is 1, then a uniform distribution between "a" and "b" is
*     used. If "dist" is 2 then a normal distribution with mean "a" and
*     standard deviation "b" is used. If "dist" is 3, then a Poisson
*     distribution is used with mean "a". Integer values are returned for
*     a Poisson distribution, and double precision values are returned for
*     uniform and normal distributions.

*  Parameters:
*     nvec = integer or field or group (Given)
*        number of vectors required [1]
*     ndim = integer (Given)
*        dimensionality for each vector [1]
*     dist = integer (Given)
*        distribution to use: 1 - uniform, 2 - normal, 3 - Poisson [1]
*     a = scalar (given)
*        first parameter describing the distribution [0.0]
*     b = scalar (given)
*        second parameter describing the distribution [1.0]
*     output = vector list or field (Returned)
*        output vectors

*  Components:
*     If "nvec" is a field, a "data" component is added to
*     the output field, replacing any existing "data" component. The output
*     data will be dependant on "positions" if the input field had no "data"
*     component. Otherwise, it will have the same dependancy as the input
*     field. Any components which are dependant on "data" are modified. All
*     other components are copied from the input field.

*  Examples:

*  See Also:
*     Compute, SXEnum, Construct

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


/*  Local Variables. */

      double  a;        /* First distribution parameter */
      double  b;        /* Second distribution parameter */
      int     dist;     /* The type of distribution to use */
      int     ndim;     /* No. of components for each vector */
      int     nvec;     /* No. of vectors */
      Object  o=NULL;   /* Output object */
      float   rval;     /* Float value */


/*  If the "ndim" object was not supplied, create random scalars. */

      if( !in[1] ) {
         ndim = 1;

      } else {
         if( !SXGet0is( "ndim", in[1], 10, 0, 0, NULL, &ndim, NULL ) ) goto error;
      }

/*  If the "dist" object was not supplied, use a uniform distribution. */

      if( !in[2] ) {
         dist = 1;

      } else {
         if( !SXGet0is( "dist", in[2], 3, 1, 0, NULL, &dist, NULL ) ) goto error;
      }


/*  Get the values of the two distribution parameters (defaults = 0,1). */

      if( !in[3] ) {
         a = 0.0;

      } else {
         if( !SXGet0rs( "a", in[3], FLT_MAX, -FLT_MAX, 0, NULL, &rval, NULL ) ) goto error;
         a = (double) rval;
      }

      if( !in[4] ) {
         b = 1.0;

      } else {
         if( !SXGet0rs( "b", in[4], FLT_MAX, -FLT_MAX, 0, NULL, &rval, NULL ) ) goto error;
         b = (double) rval;
      }


/*  If the "nvec" object was not supplied, create a single vector. */

      if( !in[0] ){
         nvec = 1;


/*  If "nvec" was supplied, but is not an array, assume it is a field or
 *  group. */

      } else {

         if( DXGetObjectClass(in[0]) != CLASS_ARRAY ){


/*  If so, create a modifiable copy of it to use as the output field. */

            o =  DXCopy( in[0], COPY_STRUCTURE );
            if( !o ) goto error;


/*  Create the output "data" component(s). */

            if( !DoSXRand( o, ndim, dist, a, b ) ) goto error;


/*  If the supplied value for "nvec" was an array, extract an integer
 *  value from it. */

         } else {
            if( !SXGet0is( "nvec", in[0], INT_MAX, 1, 0, NULL, &nvec, NULL ) ) goto error;

         }

      }


/*  If no field was given for "nvec", create an array filled with random
 *  vectors. */

      if( !o ){
         o = (Object) SXRandFill( nvec, ndim, dist, a, b );
         if( !o ) goto error;
      }


/*  Return the output object. */

      out[0] = o;
      return( OK );

error:
      DXDelete( o );
      return( ERROR );

}


static Error DoSXRand( Object o, int ndim, int dist, double a,
                       double b ){

      Array  data, pos;
      char  *dep;
      int    i, nvec;
      Object oo;


/*  If the supplied object is a field... */

      switch( DXGetObjectClass( o ) ){
      case CLASS_FIELD:


/*  See if there is a "data" component in the input */

         data = (Array) DXGetComponentValue( (Field) o, "data" );


/*  If so, try to get the "dep" attribute of the input "data" component. */

         if( data ){
            if( !DXGetStringAttribute( (Object) data, "dep", &dep ) ){
               DXSetError( ERROR_MISSING_DATA, "missing data dependancy attribute" );
               return( ERROR );
            }


/*  Get the number of items in the data array. */

            if( !DXGetArrayInfo( data, &nvec, NULL, NULL, NULL, NULL ) ) return( ERROR );


/*  If there is no data component in the input, get the "positions"
 *  component, and find out how many items there are in it. */

         } else {
            pos = (Array) DXGetComponentValue( (Field) o, "positions" );
            if( !pos ){
               DXSetError( ERROR_MISSING_DATA, "missing positions component" );
               return( ERROR );
            }

            if( !DXGetArrayInfo( pos, &nvec, NULL, NULL, NULL, NULL ) ) return( ERROR );

            dep = "positions";

         }


/*  Create a new array and fill it with random vectors. */

         oo = (Object) SXRandFill( nvec, ndim, dist, a, b );


/*  Add this new array to the field. */

         if( !DXSetComponentValue( (Field) o, "data", oo ) ){
            DXDelete( oo );
            return( ERROR );
         }


/*  Store a value for the "dep" attribute of the new array. */

         DXSetComponentAttribute( (Field) o, "data", "dep", (Object) DXNewString( dep ) );


/*  Indicate that the data component values have changed, and complete the
 *  output field. */

         DXChangedComponentValues( (Field) o, "data" );
         if( !DXEndField( (Field) o ) ) return( ERROR );

         break;


/*  If the supplied object is a group, call this function recursively for
 *  each member of the group. */

      case CLASS_GROUP:

         for( i=0; oo=(Object)DXGetEnumeratedMember((Group)o,i,NULL); i++ ){
            if( !DoSXRand( oo, ndim, dist, a, b ) ) return( ERROR );
         }

         break;

      }

      return( OK );

}






Array SXRandFill( int nvec, int ndim, int dist, double a, double b ){
/*
*+
*  Name:
*     SXRandFill

*  Purpose:
*     Create an an array filled with random vectors

*  Language:
*     ANSI C

*  Prototype:
*     Array SXRandFill( int nvec, int ndim, int dist, double a, double b );

*  Description:
*     An array is created and filled with random vectors.

*  Arguments:
*     nvec = int (Given)
*        number of vectors
*     ndim = int (Given)
*        dimensionality for each vector
*     dist = integer (Given)
*        distribution to use: 1 - uniform, 2 - normal, 3 - Poisson
*     a = double (given)
*        first parameter describing the distribution
*     b = double (given)
*        second parameter describing the distribution

*  Returned Value:
*     The DX object describing the created array, or NULL if an error
*     occurs.

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

/*  Local Variables. */

      double  bma;    /* b minus a */
      double *data;   /* Pointer to double array */
      int     i;      /* Loop count */
      int    *idata;  /* Pointer to int array */
      Array   o;      /* The new array */
      int     seed;   /* Seed */


/*  Initialise the random number generator seed to the current time plus
 *  thenumber of entries into this routine. */

      seed = (int) time( NULL ) + (entry++);
      (void) zufalli_( seed );


/*  Do Uniform distributions. */

      if( dist == 1 ) {


/*  Create a new array to hold the vectors, and get a pointer to it */

         o = (Array) DXNewArrayV( TYPE_DOUBLE, CATEGORY_REAL, 1, &ndim );
         if( !o ) return( NULL );

         if( !DXAddArrayData( o, 0, nvec, NULL ) ) {
            DXDelete( (Object) o );
            return( NULL );
         }

         data = (double *) DXGetArrayData( o );


/*  Fill the array with the random values, scaling value from [0,1] to
 *  [a,b] */

         (void) zufall_( nvec*ndim, data );

         bma = b - a;
         for( i=0; i<ndim*nvec; i++ ){
            data[i] = bma*data[i] + a;
         }


/*  Now do normal distributions. */

      } else if( dist == 2 ) {

         o = (Array) DXNewArrayV( TYPE_DOUBLE, CATEGORY_REAL, 1, &ndim );
         if( !o ) return( NULL );

         if( !DXAddArrayData( o, 0, nvec, NULL ) ) {
            DXDelete( (Object) o );
            return( NULL );
         }

         data = (double *) DXGetArrayData( o );

         (void) normalen_( nvec*ndim, data );

         for( i=0; i<ndim*nvec; i++ ){
            data[i] = b*data[i] + a;
         }


/*  Now do Poisson distributions. */

      } else if( dist == 3 ) {

         if( a <= 0.0){
            DXSetError( ERROR_BAD_PARAMETER, "\"a\" must be positive for Poisson distribution");
            return( NULL );
         }

         o = (Array) DXNewArrayV( TYPE_INT, CATEGORY_REAL, 1, &ndim );
         if( !o ) return( NULL );

         if( !DXAddArrayData( o, 0, nvec, NULL ) ) {
            DXDelete( (Object) o );
            return( NULL );
         }

         idata = (int *) DXGetArrayData( o );

         (void) fische_( nvec*ndim, a, idata );

      }


/*  Return the new array object */

      return( o );

}

