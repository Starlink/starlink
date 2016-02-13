#include "star/hds.h"
#include "ast.h"
#include "mers.h"
#include "sae_par.h"
#include "kaplibs.h"
#include <string.h>

static void kpg1Chpc1( double *, double *, int, int *, int *, int * );
static void kpg1Shpc1( double, double, int, double *, double *, int * );
static void kpg1Chpc2( double *, int, int, double, double, double, double, double *, int * );

double *kpg1Chcof( int nloc,  HDSLoc **loc, int *ncoeff_f, int *nin,
                   int *status ){
/*
*  Name:
*     kpg1Chcof

*  Purpose:
*     Returns coefficients for a set of Starlink POLYNOMIAL structures.

*  Language:
*     C.

*  Invocation:
*     double *kpg1Chcof( int nloc, HDSLoc **loc, int &ncoeff_f, int *nin,
*                        int *status )

*  Description:
*     This routine reads coefficient data from one or more Starlink
*     POLYNOMIAL structures (see SGP/38), and converts them into a form
*     suitable for use with the AST PolyMap constructor.

*  Arguments:
*     nloc
*        The number of locators supplied by "loc".
*     loc
*        A pointer to an array of "nloc" HDSLoc pointers. Each locator
*        should be for a scalar POLYNOMIAL structure. Each such structure
*        defines one output of the AST PolyMap that could be constructed
*        from the returned array.
*     ncoeff_f
*        The number of coefficients described by the returned array,
*        sutable for passing to the PolyMap constructor.
*     nin
*        Pointer to integer in which to return the number of inputs for
*        the polynomial. The number of outputs will be equal to "nloc".
*     status
*        The inherited status.

*  Returned Value:
*     A pointer to a dynamically allocated array of doubles that can be
*     passed directly to the AST PolyMap constructor as the "coeff_f"
*     argument. The returned array should be freed using astFree when no
*     longer needed. A NULL pointer is returned if an error occurrs.

*  Notes:
*     - Both CHEBYSHEV and SIMPLE variants of the POLYNOMIAL structure
*     are supported. But currently only 1- or 2-dimensional Chebyshev
*     polynomials can be handled. An error is reported for Chebyshev
*     polynomials of higher dimensionality.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-NOV-2009 (DSB):
*        Original version.
*     3-DEC-2009 (DSB):
*        Include linear coefficients in returned array, even if they are
*        zero-valued.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/

/* Local Varianles: */
   HDSLoc *dloc = NULL;
   HDSLoc *tmaxloc = NULL;
   HDSLoc *tminloc = NULL;
   HDSLoc *vloc = NULL;
   char type[ DAT__SZTYP + 1 ];
   char variant[ 12 ];
   double *data;
   double *p;
   double *q;
   double *result;
   double *tmax;
   double *tmin;
   double *w1;
   double *w2;
   hdsdim dims[ DAT__MXDIM ];
   int *iw1;
   int *iw2;
   int *power;
   int i;
   int iel;
   int iloc;
   int j;
   int k;
   int linear;
   int ndim;
   size_t nc;
   size_t nel;
   size_t size;

/* Initialise */
   result = NULL;
   *ncoeff_f = 0;
   *nin = 0;
   w2 = NULL;
   iel = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Loop round each locator. */
   for( iloc = 0; iloc < nloc; iloc++ ) {

/* Check the supplied locator is for a scalar. */
      datSize( loc[ iloc ], &size, status );
      if( size > 1 && *status == SAI__OK) {
         *status = SAI__ERROR;
         errRep( " ", "kpg1Chcof: Supplied HDS object is not scalar "
                 "(programming error).", status );
      }

/* Check the supplied locator is for structure with type "POLYNOMIAL". */
      datType( loc[ iloc ], type, status );
      if( strcmp( type, "POLYNOMIAL" ) && *status == SAI__OK) {
         msgSetc( "T", type );
         *status = SAI__ERROR;
         errRep( " ", "kpg1Chcof: Supplied HDS object is not a POLYNOMIAL "
                 "structure - it has type '^T' (programming error).", status );
      }

/* Get the shape of the data array, and map it as a vector. */
      datFind( loc[ iloc ], "DATA_ARRAY", &dloc, status );
      datShape( dloc, DAT__MXDIM, dims, &ndim, status);
      datMapV( dloc, "_DOUBLE", "Read", (void *) &data, &nc, status );

/* Check all polynomials have the same number of inputs. */
      if( iloc == 0 ) {
         *nin = ndim;
      } else if( ndim != *nin && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( " ", "kpg1Chcof: Supplied polynomials have different "
                 "numbers of inputs.", status );
      }

/* Get the POLYNOMIAL variant. */
      datFind( loc[ iloc ], "VARIANT", &vloc, status );
      datGet0C( vloc, variant, 11, status );
      datAnnul( &vloc, status );

/* If this is a Chebyshev polynomial, convert it to simple form. */
      if( !strcmp( variant, "CHEBYSHEV" ) && *status == SAI__OK ) {

/* 2D Chebyshev polynomials are defined on a square (-1:1,-1:1), which is
   mapped onto a rectangle of size (xmin:xmax, ymin:ymax ) in user-space.
   Get the limits of this rectangle. NOTE - as of 13/11/2009 there is a
   mistake in the description of the CHEBYSHEV variant in SGP/38 - where
   it says:

      Sn = ( AXISn - TMIN(n) )/( TMAX(n) - TMIN(n) )

   it should say:

      Sn = ( 2*AXISn - ( TMAX(n) + TMIN(n) ) )/( TMAX(n) - TMIN(n) ) */

         datFind( loc[ iloc ], "TMIN", &tminloc, status );
         datMapV( tminloc, "_DOUBLE", "Read", (void *) &tmin, &nel, status );
         if( nel != ndim && *status == SAI__OK) {
            msgSeti( "N", nel );
            msgSeti( "M", ndim );
            *status = SAI__ERROR;
            errRep( " ", "kpg1Chcof: Corrupt POLYNOMIAL structure supplied: TMIN "
                    "has length ^N but DATA_ARRAY has ^M axes.", status );
         }

         datFind( loc[ iloc ], "TMAX", &tmaxloc, status );
         datMapV( tmaxloc, "_DOUBLE", "Read", (void *) &tmax, &nel, status );
         if( nel != ndim && *status == SAI__OK) {
            msgSeti( "N", nel );
            msgSeti( "M", ndim );
            *status = SAI__ERROR;
            errRep( " ", "kpg1Chcof: Corrupt POLYNOMIAL structure supplied: TMAX "
                    "has length ^N but DATA_ARRAY has ^M axes.", status );
         }

/* Handle 1D Chebyshev polynomials. */
         if( ndim == 1 && *status == SAI__OK ) {

/* Allocate workspace. */
            w1 = astMalloc( nc*sizeof( double ) );
            w2 = astMalloc( nc*sizeof( double ) );
            iw1 = astMalloc( nc*sizeof( int ) );
            iw2 = astMalloc( nc*sizeof( int ) );

/* Get an array holding the polynomial coefficients in simple form,
   rather than Chebyshev form. These coefficients still give the polynomial
   as a function of the normalised axes in the range [-1,+1]. */
            kpg1Chpc1( data, w1, nc, iw1, iw2, status );

/* Scale the coefficients so that the polynomial becomes a function of
   the real axis positions in the range TMIN to TMAX, rather than the
   normalised axes in the range [-1,+1]. */
            kpg1Shpc1( tmin[ 0 ], tmax[ 0 ], nc, w1, w2, status );

/* Free workspace. */
            w1 = astFree( w1 );
            iw1 = astFree( iw1 );
            iw2 = astFree( iw2 );

/* Handle 2D Chebyshev polynomials. */
         } else if( ndim == 2 && *status == SAI__OK ) {

/* Allocate workspace. */
            w2 = astMalloc( nc*sizeof( double ) );

/* Get an array holding the polynomial coefficients in simple form,
   rather than Chebyshev form, and scale the coefficients so that the
   polynomial becomes a function of the real axis positions in the range
   TMIN to TMAX, rather than the normalised axes in the range [-1,+1]. */
            kpg1Chpc2( data, dims[ 0 ], dims[ 1 ], tmin[ 0 ], tmax[ 0 ],
                       tmin[ 1 ], tmax[ 1 ], w2, status );

/* Currently, we can only handle 1D or 2D Chebyshev polynomials. */
         } else if( *status == SAI__OK) {
            msgSeti( "N", ndim );
            *status = SAI__ERROR;
            errRep( " ", "kpg1Chcof: Supplied polynomial is ^N-dimensional. "
                    "This routine can only handle 2 dimensions.", status );
         }

/* Free resources */
         datAnnul( &tmaxloc, status );
         datAnnul( &tminloc, status );

/* For simple variants, just use the coefficients as read from the
   POLYNOMIAL. */
      } else if( !strcmp( variant, "SIMPLE" ) && *status == SAI__OK ) {
         w2 = astStore( NULL, data, sizeof( double )*nc );

/* Report an error for an unknown polynomial variant. */
      } else if( *status == SAI__OK ) {
         msgSetc( "V", variant );
         *status = SAI__ERROR;
         errRep( " ", "kpg1Chcof: Unsupported POLYNOMIAL variant '^V' "
                 "supplied (programming error).", status );
      }

/* The simple coefficients are no in "w", which is an array with "ndim"
   axes having dimensions in "dims". We now append these coefficients to
   the returned array. Allocate workspace. */
      power = astMalloc( ndim*sizeof( int ) );
      if( *status == SAI__OK ) {

/* Initialise the array holding the current power for each axis. The
   power can also be thought of as the zero-based indices into the
   POLYNOMIAL.DATA_ARRAY array that holds the coefficients. */
         for( i = 0; i < ndim; i++ ) power[ i ] = 0;

/* Loop round all the simple coefficients currently in "w2". */
         k = 0;
         q = w2;
         for( i = 0; i < nc; i++,q++ ) {

/* If the coefficient is non-zero, append it to the returned array,
   extending it as necessary. For linear terms, append them even if they
   are zero. */
            if( *q != 0.0 || linear ) {
               result = astGrow( result, (++(*ncoeff_f))*( 2 + ndim ),
                                 sizeof( double ) );
               if( *status == SAI__OK ) {
                  p = result + iel;
                  *(p++) = *q;
                  *(p++) = iloc + 1.0;
                  for( j = 0; j < ndim; j++ ) *(p++) = power[ j ];
                  iel += 2 + ndim;
               }
            }

/* Update the power for each axis. */
            j = 0;
            while( j < ndim && ++( power[ j ] ) == dims[ j ] ) {
               power[ j ] = 0;
               j++;
            }

/* Set a flag if the next pass will be for a linear term. */
            if( power[ k ] == 1 ) {
               linear = 1;
               k++;
            } else {
               linear = 0;
            }

         }
      }

/* Free workspace. */
      power = astFree( power );
      w2 = astFree( w2 );
      datAnnul( &dloc, status );
   }

/* If all has gone well, return the result. */
   if( *status != SAI__OK ) {
      result = astFree( result );
      *ncoeff_f = 0;
   }
   return result;
}

static void kpg1Chpc1( double *c, double *d, int n, int *w0, int *w1,
                       int *status ){
/*
*  Name:
*     kpg1Chpc1

*  Purpose:
*     Converts a one-dimensional Chebyshev polynomial to standard form.

*  Invocation:
*     void kpg1Chpc1( double *c, double *d, int n, int *w0, int *w1,
*                     int *status )

*  Description:
*     Given the coefficients of a one-dimensional Chebychev polynomial P(u),
*     find the coefficients of the equivalent standard 1D polynomial Q(u).
*     The allowed range of u is assumed to be the unit interval.

*  Arguments:
*     c
*        An array of n elements supplied holding the coefficients of
*        P, such that the coefficient of (Ti(u)) is held in element
*        (i), where "Ti(u)" is the Chebychev polynomial (of the
*        first kind) of order "i" evaluated at "u". Note, there is a
*        convention in Starlink POLYNOMIAL structures (see SGP/38) to
*        double the value of the zeroth coefficient. This was
*        inherited from NAG (see E02AKF).
*     d
*        An array of n elements returned holding the coefficients of
*        Q, such that the coefficient of (u^i) is held in element (i).
*     n
*        One more than the highest power of u in P.
*     w0
*        Pointer to a work array of n elements.
*     w1
*        Pointer to a work array of n elements.
*     status
*        Inherited status value

*  Notes:
*    - Vaguely inspired by the Numerical Recipes routine "chebpc". But the
*    original had bugs, so I wrote this new version from first principles.

*  Authors:
*     DSB: David S. Berry

*  History:
*     13-NOV-2009 (DSB):
*        Original version.

*/

/* Local Variables: */
   int sv;
   int j;
   int k;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Initialise the returned coefficients array. */
   for( j = 0; j < n; j++ ) d[ j ] = 0.0;

/* Use the recurrence relation

   T{k+1}(x) = 2.x.T{k}(x) - T{k-1}(x).

   w0[i] holds the coefficient of x^i in T{k-1}. w1[i] holds the
   coefficient of x^i in T{k}. Initialise them for T0 (="1") and
   T1 (="x"). */
   for( j = 0; j < n; j++ ) w0[ j ] = w1[ j ] = 0;
   w0[ 0 ] = 1;
   w1[ 1 ] = 1;

/* Update the returned coefficients array to include the T0 and T1 terms.
   Note, there is a convention in Starlink POLYNOMIAL structures (see
   SGP/38) to double the value of the zeroth coefficient. This was
   inherited from NAG (see E02AKF). Not sure why it's done, but as a
   consequence we half the supplied zeroth coefficient value. */
   d[ 0 ] = 0.5*c[ 0 ];
   d[ 1 ] = c[ 1 ];

/* Loop round using the above recurrence relation until we have found
   T{n-1}. */
   for( k = 1; k < n - 1; k++ ){

/* To get the coefficients of T{k+1} shift the contents of w1 up one
   element, introducing a zero at the low end, and then double all the
   values in w1. Finally subtract off the values in w0. This implements
   the above recurrence relationship. Starting at the top end and working
   down to the bottom, store a new value for each element of w1. */
      for( j = n - 1; j > 0; j-- ) {

/* First save the original element of w1 in w0 for use next time. But we
   also need the original w0 element later on so save it first. */
         sv = w0[ j ];
         w0[ j ] = w1[ j ];

/* Double the lower neighbouring w1 element and subtract off the w0
   element saved above. This forms the new value for w1. */
         w1[ j ] = 2*w1[ j - 1 ] - sv;
      }

/* Introduce a zero into the lowest element of w1, saving the original
   value first in w0. Then subtract off the original value of w0. */
      sv = w0[ 0 ];
      w0[ 0 ] = w1[ 0 ];
      w1[ 0 ] = -sv;

/* W1 now contains the coefficients of T{k+1} in w1, and the coefficients
   of T{k} in w0. Multiply these by the supplied coefficient for T{k+1},
   and add them into the returned array. */
      for( j = 0; j <= k + 1; j++ ){
         d[ j ] += c[ k + 1 ]*w1[ j ];
      }
   }
}

static void kpg1Shpc1( double xmin, double xmax, int n, double *d, double *w,
                       int *status ){
/*
*  Name:
*    kpg1Shpc1

*  Purpose:
*     Modifies a one-dimensional polynomial to scale the polynomial argument.

*  Invocation:
*     void kpg1Shpc1( double xmin, double xmax, int n, double *d, double *w,
*                     int *status )

*  Description:
*    Given the coefficients of a one-dimensional polynomial P(u) defined on a 
*    unit interval (i.e. -1 <= u <= +1 ), find the coefficients of another
*    one-dimensional polynomial Q(x) where:
*
*       Q(x) = P(u)
*       u = ( 2*x - ( xmax + xmin ) ) / ( xmax - xmin )
*
*    That is, u is a scaled version of x, such that the unit interval in u
*    maps onto (xmin:xmax) in x.

*  Arguments:
*    xmin
*       X value corresponding to u = -1
*    xmax
*       X value corresponding to u = +1
*    n
*       One more than the maximum power of u within P.
*    d
*       An array of n elements supplied holding the coefficients of P such
*       that the coefficient of (u^i) is held in element (i).
*    w
*       An array of n elements returned holding the coefficients of Q such
*       that the coefficient of (x^i) is held in element (i).
*    status
*       Inherited status value

*  Notes:
*    - Vaguely inspired by the Numerical Recipes routine "pcshft". But the
*    original had bugs, so I wrote this new version from first principles.

*  Authors:
*     DSB: David S. Berry

*  History:
*     13-NOV-2009 (DSB):
*        Original version.

*/

/* Local Variables: */
   double b;
   double a;
   int j;
   int i;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get the scale and shift terms so that u = a*x + b */
   a = 2.0/( xmax - xmin );
   b = ( xmin + xmax )/( xmin - xmax );

/* Initialise the returned coeffs */
   for( i = 0; i < n; i++ ) w[ i ] = 0.0;

/* The supplied Polynomial is

   P(u) = d0 + d1*u + d2*u^2 + ...

        = d0 + u*( d1 + u*( d2 + ... u*( d{n-1} ) ) )  . . . . . (1)

        = d0 + (a*x+b)*( d1 + (a*x+b)*( d2 + ... (a*x+b)*( d[n-1] ) ) )

   The inner-most parenthesised expression is a polynomial of order zero
   (a constant - d[n-1]). Store the coefficients of this zeroth order
   polynomial in the returned array. The "w" array is used to hold the
   coefficients of Q, i.e. coefficients of powers of "x", not "u", but
   since the inner-most polynomial is a constant, it makes no difference
   (x^0 == u^0 == 1). */
   w[ 0 ] = d[ n - 1 ];

/* Now loop through each remaining level of parenthetic nesting in (1). At
   each level, the parenthesised expression represents a polynomial of order
   "i". At the end of each pass though this loop, the returned array "w"
   holds the coefficients of this "i"th order polynomial. So on the last
   loop, i = n-1, "w" holds the required coefficients of Q. */
   for( i = 1; i < n; i++ ) {

/* If "R" is the polynomial at the "i-1"th level of nesting (the
   coefficiemts of which are currently held in "w"), and "S" is the
   polynomial at the "i"th level of nesting, we can see from (1) that:

   S = d[ n - 1 - i ] + u*R

   Substituting for "u", this becomes

   S = d[ n - 1 - i ] + ( a*x + b )*R
     = d[ n - 1 - i ] + a*R*x + b*R

   Looking at each of these three terms in reverse order:

   1) The "b*R" term is implemented by simply scaling the current contents
   of the "w" array by "b"; in the "a*R*x" term.

   2) In "a*R*x", the effect of multiplying by "x" is to move the existing
   coefficients in "w" up one element. We then multiply the shifted
   coefficients by "a" and add them onto the coefficients produced at
   step 1) above.

   We know that "w" still contains the initial zeros at indices higher than
   "i" so we only need to scale the bottom "i" elements. We do not do the
   zeroth term in this loop since there is no lower term to shift up into
   it. */

      for( j = i; j > 0; j-- ){
         w[ j ] = b*w[ j ] + a*w[ j - 1 ];
      }

/* Now do the zeroth term. Scale the existing zeroth term by "b" as
   required by step 1) and add on the first term, the constant
   "d[ n - 1 - i ]". Step 2) is a no-op, since in effect the value of
   "w[-1]" is zero. */
      w[ 0 ] = d[ n - i - 1 ] + b*w[ 0 ];
   }

}

static void kpg1Chpc2( double *c, int nx, int ny, double xmin, double xmax,
                       double ymin, double ymax, double *d, int *status ){
/*
*  Name:
*    kpg1Chpc2

*  Purpose:
*     Converts a two-dimensional Chebyshev polynomial to standard form and 
*     scale the arguments.

*  Invocation:
*     void kpg1Chpc2( double *c, int nx, int ny, double xmin, double xmax,
*                     double ymin, double ymax, double *d, int *status )

*  Description:
*    Given the coefficients of a two-dimensional Chebychev polynomial P(u,v),
*    find the coefficients of the equivalent standard two-dimensional
*    polynomial Q(x,y). The allowed range of u and v is assumed to be the
*    unit square, and this maps on to the rectangle in (x,y) given by
*    (xmin:xmax,ymin:ymax).

*  Arguments:
*    c
*       An array of (nx,ny) elements supplied holding the coefficients of
*       P, such that the coefficient of (Ti(u)*Tj(v)) is held in element
*       (i + j*nx), where "Ti(u)" is the Chebychev polynomial (of the
*       first kind) of order "i" evaluated at "u", and "Tj(v)" is the
*       Chebychev polynomial of order "j" evaluated at "v".
*    nx
*       One more than the maximum power of u within P.
*    ny
*       One more than the maximum power of v within P.
*    xmin
*       X value corresponding to u = -1
*    xmax
*       X value corresponding to u = +1
*    ymin
*       Y value corresponding to v = -1
*    ymax
*       Y value corresponding to v = +1
*    d
*       An array of (nx,ny) elements returned holding the coefficients of Q,
*       such that the coefficient of (x^i*y^j) is held in element (i + j*nx).
*    status
*       Inherited status value

*  Authors:
*     DSB: David S. Berry

*  History:
*     13-NOV-2009 (DSB):
*        Original version.
*/

/* Local Variables: */
   double *pa;
   double *pw;
   double *work1;
   double *work2;
   double *work3;
   int *iw1;
   int *iw2;
   int i;
   int j;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Allocate workspace. */
   work1 = astMalloc( sizeof( *work1 )*ny );
   work2 = astMalloc( sizeof( *work2 )*ny );
   work3 = astMalloc( sizeof( *work2 )*nx );
   iw1 = astMalloc( sizeof(int)*( nx > ny ? nx : ny ) );
   iw2 = astMalloc( sizeof(int)*( nx > ny ? nx : ny ) );

/* Thinking of P as a 1D polynomial in v, each coefficient would itself then
   be a 1D polynomial in u:

   P = (   c[0] +      c[1]*T1(u) +      c[2]*T2(u) + ... ) +
       (  c[nx] +   c[nx+1]*T1(u) +   c[nx+2]*T2(u) + ... )*T1(v) +
       (c[2*nx] + c[2*nx+1]*T1(u) + c[2*nx+2]*T2(u) + ... )*T2(v) +
       ...
       (c[(ny-1)*nx] + c[(ny-1)*nx+1]*T1(u) + c[(ny-1)*nx+2]*T2(u) + ... )T{ny-1}(v)

   Use kpg1Chpc1 to convert these "polynomial coefficients" to standard
   form, storing the result in the corresponding row of "d" . Also,
   convert them from u to x. */

   for( j = 0; j < ny; j++ ) {
      kpg1Chpc1( c + j*nx, work3, nx, iw1, iw2, status );
      kpg1Shpc1( xmin, xmax, nx, work3, d + j*nx, status );
   }

/* The polynomial value is now:

    (   d[0] +      d[1]*x +      d[2]*x*x + ... ) +
    (  d[nx] +   d[nx+1]*x +   d[nx+2]*x*x + ... )*T1(v) +
    (d[2*nx] + d[2*nx+1]*x + d[2*nx+2]*x*x + ... )*T2(v) +
    ...
    (d[(ny-1)*nx] + d[(ny-1)*nx+1]*x + d[(ny-1)*nx+2]*x*x + ... )*T{ny-1}(v)

   If we rearrange this expression to view it as a 1D polynomial in x,
   rather than v, each coefficient of the new 1D polynomial is then
   itself a polynomial in v:

    ( d[0] +   d[nx]*T1(v) +   d[2*nx]*T2(v) + ... d[(ny-1)*nx]*T{ny-1}(v) ) +
    ( d[1] + d[nx+1]*T1(v) + d[2*nx+1]*T2(v) + ... d[(ny-1)*nx+1]T{ny-1}(v)... )*x +
    ( d[2] + d[nx+2]*T1(v) + d[2*nx+2]*T2(v) + ... d[(ny-1)*nx+2]T{ny-1}(v)... )*x*x +
    ...
    ( d[nx-1] + d[2*nx-1]*T1(v) + d[3*nx-1]*T2(v) + ... d[ny*nx-1]*T{ny-1}(v) )*x*x*...


   Now use kpg1Chpc1 to convert each of these "polynomial coefficients"
   to standard form. We copy each column of the d array into a 1D work array,
   use kpg1Shpc1 to modify the values in the work array, and then write
   the modified values back into the current column of d. Also convert
   from v to y. */

   for( i = 0; i < nx; i++ ) {
      pa = d + i;
      pw = work1;
      for( j = 0; j < ny; j++ ) {
         *(pw++) = *pa;
         pa += nx;
      }

      kpg1Chpc1( work1, work2, ny, iw1, iw2, status );
      kpg1Shpc1( ymin, ymax, ny, work2, work1, status );

      pa = d + i;
      pw = work1;
      for( j = 0; j < ny; j++ ) {
         *pa = *(pw++);
         pa += nx;
      }
   }

/* So the polynomial is now:

    ( d[0] +   d[nx]*y +   d[2*nx]*y*y + ... d[(ny-1)*nx]*y*y*... ) +
    ( d[1] + d[nx+1]*y + d[2*nx+1]*y*y + ... d[(ny-1)*nx+1]*y*y*... )*x +
    ( d[2] + d[nx+2]*y + d[2*nx+2]*y*y + ... d[(ny-1)*nx+2]*y*y*... )*x*x +
    ...
    ( d[nx-1] + d[2*nx-1]*y + d[3*nx-1]*y*y + ... d[ny*nx-1]*y*y*... )*x*x*...

  Re-arranging, this is:

    (   d[0] +      d[1]*x +      d[2]*x*x + ... ) +
    (  d[nx] +   d[nx+1]*x +   d[nx+2]*x*x + ... )*y +
    (d[2*nx] + d[2*nx+1]*x + d[2*nx+2]*x*x + ... )*y*y +
    ...
    (d[(ny-1)*nx] + d[(ny-1)*nx+1]*x + d[(ny-1)*nx+2]*x*x + ... )*y*y*...

   as required. */

/* Free the workspace. */
   work1 = astFree( work1 );
   work2 = astFree( work2 );
   work3 = astFree( work3 );
   iw1 = astFree( iw1 );
   iw2 = astFree( iw2 );

}









