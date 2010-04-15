/*
*+
*  Name:
*     Sumsl

*  Purpose:
*     Tests the C interface to the pda_sumsl minimisation routine.

*  Description:
*     This is a direct transliteration of Sumsl.f into C. See Sumsl.f for
*     further details. Note, the C interface to PDA_SUMSL differs from
*     the Fortran interface in that pdaSumsl has no parameters for
*     communicating values to the service routines - calcf and calcg. Such
*     communication should be achieved using C external variables instead.

*-
*/

#include "pda.h"
#include <stdio.h>
#include <math.h>

#define n 5
#define lv (71+n*(n+15)/2)
#define nx 20
#define ny 20
#define nz 20

void calcf( int, double *, int *, double * );
void calcg( int, double *, int *, double * );
double model( int, int, int, int, double *, int * );

static int dims[3];
static double array[ nx*ny*nz ];

int main(){

   int i, j, k;
   int iv[60];
   int ivec;
   int nf;
   double v[ lv ];
   double d[ n ];
   double x[ n ];

   d[ 0 ] = 1.0;
   d[ 1 ] = 1.0;
   d[ 2 ] = 1.0;
   d[ 3 ] = 1.0;
   d[ 4 ] = 1.0;

   dims[ 0 ] = nx;
   dims[ 1 ] = ny;
   dims[ 2 ] = nz;

   x[ 0 ] = 1;
   x[ 1 ] = 2;
   x[ 2 ] = 3;
   x[ 3 ] = 4;
   x[ 4 ] = 5;

   ivec = 0;
   nf = 1;
   for( i = 0; i < dims[ 0 ]; i++ ) {
      for( j = 0; j < dims[ 1 ]; j++ ) {
         for( k = 0; k < dims[ 2 ]; k++ ) {
            array[ ivec ] = model( 0, i, j, k, x, &nf );
            ivec++;
         }
      }
   }

   x[ 0 ] = 1.5;
   x[ 1 ] = 1.5;
   x[ 2 ] = 2.5;
   x[ 3 ] = 2.5;
   x[ 4 ] = 3.5;

   iv[0]=0;
   pdaSumsl( n, d, x, calcf, calcg, iv, 60, lv, v );

   if( fabs( x[0] - 1.000 ) > 0.01 ||
       fabs( x[1] - 1.952 ) > 0.01 ||
       fabs( x[2] - 3.074 ) > 0.01 ||
       fabs( x[3] - 4.099 ) > 0.01 ||
       fabs( x[4] - 4.880 ) > 0.01 ) {
      printf("Sumsl: Test of pdaSumsl failed\n");
   } else {
      printf("Sumsl: Test of pdaSumsl passed\n");
   }

  return 0;
}



void calcf( int nn, double *x, int *nf, double *f ){
   int i, j, k, ivec;
   double m,r;

   *f = 0.0;
   ivec = 0;
   for( i = 0; i < dims[ 0 ]; i++ ){
      for( j = 0; j < dims[ 1 ]; j++ ){
         for( k = 0; k < dims[ 2 ]; k++ ){
            m = model( 0, i, j, k, x, nf );
            r = array[ ivec ] - m;
            *f += r*r;
            ivec++;
         }
      }
   }
}



void calcg( int nn, double *x, int *nf, double *g ){
   int i, j, k, ivec;
   double res;

   g[ 0 ] = 0.0;
   g[ 1 ] = 0.0;
   g[ 2 ] = 0.0;
   g[ 3 ] = 0.0;
   g[ 4 ] = 0.0;

   ivec = 0;
   for( i = 0; i < dims[ 0 ]; i++ ) {
      for( j = 0; j < dims[ 1 ]; j++ ) {
         for( k = 0; k < dims[ 2 ]; k++ ) {
            res = array[ ivec ] - model( 0, i, j, k, x, nf );
            ivec++;

            g[0] += res*model( 1, i, j, k, x, nf );
            g[1] += res*model( 2, i, j, k, x, nf );
            g[2] += res*model( 3, i, j, k, x, nf );
            g[3] += res*model( 4, i, j, k, x, nf );
            g[4] += res*model( 5, i, j, k, x, nf );

         }
      }
   }

   g[0] = -2*g[0];
   g[1] = -2*g[1];
   g[2] = -2*g[2];
   g[3] = -2*g[3];
   g[4] = -2*g[4];

}


double model( int axis, int i, int j, int k, double *x, int *nf ){
   double a, b, c, ret;

   if( x[ 1 ] == 0.0 || x[ 3 ] == 0.0 ) {
      *nf = 0;

   } else {
      a = ( i + 1 ) - x[ 0 ];
      b = ( j + 1 )/x[ 1 ] - x[ 2 ];
      c = ( k + 1 )/x[ 3 ] - x[ 4 ];

      if( axis == 0 ) {
         ret = a*b*c;

      } else if( axis == 1 ) {
         ret = -b*c;

      } else if( axis == 2 ) {
         ret = -a*c*j/( x[1] * x[1] );

      } else if( axis == 3 ) {
         ret = -a*c;

      } else if( axis == 4 ) {
         ret = -a*b*k/( x[3] * x[3] );

      } else if( axis == 5 ) {
         ret = -a*b;

      } else {
         *nf = 0;
      }

   }

   if( *nf == 0 ) ret = 0.0;

   return ret;
}


