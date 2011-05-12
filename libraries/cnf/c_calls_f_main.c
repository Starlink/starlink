#if HAVE_CONFIG_H
# include <config.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include "f77.h"

/* Some compilers need a dummy main */
#if HAVE_FC_MAIN
void FC_MAIN () {}
#endif

#define NI 10
#define NR 10
#define ND 10
#define NL  9
#define NB 11
#define NW 20
#define NUB 12
#define NUW 21
#define NC 5

int main()
{
   int j;
   DECLARE_INTEGER(ni);
   DECLARE_INTEGER(i);
   DECLARE_INTEGER_ARRAY(ia,NI);
   DECLARE_INTEGER(nr);
   DECLARE_REAL(r);
   DECLARE_REAL_ARRAY(ra,NR);
   DECLARE_INTEGER(nd);
   DECLARE_DOUBLE(d);
   DECLARE_DOUBLE_ARRAY(da,ND);
   DECLARE_INTEGER(nl);
   DECLARE_LOGICAL(l);
   DECLARE_LOGICAL_ARRAY(la,NL);
   DECLARE_INTEGER(nb);
   DECLARE_BYTE(b);
   DECLARE_BYTE_ARRAY(ba,NB);
   DECLARE_INTEGER(nw);
   DECLARE_WORD(w);
   DECLARE_WORD_ARRAY(wa,NW);
   DECLARE_INTEGER(nub);
   DECLARE_UBYTE(ub);
   DECLARE_UBYTE_ARRAY(uba,NUB);
   DECLARE_INTEGER(nuw);
   DECLARE_UWORD(uw);
   DECLARE_UWORD_ARRAY(uwa,NUW);
   DECLARE_CHARACTER(c1f,80);
   DECLARE_CHARACTER_ARRAY(caf,80,NC);
   DECLARE_INTEGER(nc);

   char c;
   char c1c[81];

   ni = NI;
   nr = NR;
   nd = ND;
   nl = NL;
   nb = NB;
   nw = NW;
   nub= NUB;
   nuw = NUW;

   printf( "--> This is a test of C calling FORTRAN\n" );

   /* Initialise fortran run time library */
   cnfInitRTL( 0, NULL );

/* Test the passing of int arguments */
   for( j=0 ; j<ni ; j++ )
      ia[j] = j+1;
   F77_LOCK( F77_CALL(ti)( INTEGER_ARRAY_ARG(ia), INTEGER_ARG(&ni), INTEGER_ARG(&i) ); )
   printf( "The sum of the first %d integers is %d\n", ni, i );

/* Test the passing of float arguments */
   for( j=0 ; j<nr ; j++ )
      ra[j] = j+1;
   F77_LOCK( F77_CALL(tr)( REAL_ARRAY_ARG(ra), INTEGER_ARG(&nr), REAL_ARG(&r) ); )
   printf( "The mean of the first %d integers is %f\n", nr, r );

/* Test the passing of double arguments */
   for( j=0 ; j<nd ; j++ )
      da[j] = j+1;
   F77_LOCK( F77_CALL(td)( DOUBLE_ARRAY_ARG(da), INTEGER_ARG(&nd), DOUBLE_ARG(&d) ); )
   printf( "The sum of squares of the first %d integers is %lf\n", nd, d );

/* Test the passing of int arguments as logical values */
   for( j=0 ; j<nl ; j++ )
      la[j] = (j+1)%2;
   F77_LOCK( F77_CALL(tl)( LOGICAL_ARRAY_ARG(la), INTEGER_ARG(&nl), LOGICAL_ARG(&l) ); )
   if( l )
      c = 'T';
   else
      c = 'F';
   printf(
"The statement 'The number of odd values in the first %d integers is 5' is %c\n"
, nl, c );

/* Test the passing of char arguments as bytes */
   for( j=0 ; j<nb ; j++ )
      ba[j] = j+1;
   F77_LOCK( F77_CALL(tb)( BYTE_ARRAY_ARG(ba), INTEGER_ARG(&nb), BYTE_ARG(&b) ); )
   printf( "The sum of the first %d integers is %d\n", nb, b );

/* Test the passing of short arguments */
   for( j=0 ; j<nw ; j++ )
      wa[j] = j+1;
   F77_LOCK( F77_CALL(tw)( WORD_ARRAY_ARG(wa), INTEGER_ARG(&nw), WORD_ARG(&w) ); )
   printf( "The sum of the first %d integers is %d\n", nw, w );

/* Test the passing of unsigned char arguments as bytes */
   for( j=0 ; j<nub ; j++ )
      uba[j] = j+1;
   F77_LOCK( F77_CALL(tub)( UBYTE_ARRAY_ARG(uba), INTEGER_ARG(&nub), UBYTE_ARG(&ub) ); )
   printf( "The sum of the first %d integers is %d\n", nub, ub );

/* Test the passing of unsigned short arguments */
   for( j=0 ; j<nuw ; j++ )
      uwa[j] = j+1;
   F77_LOCK( F77_CALL(tuw)( UWORD_ARRAY_ARG(uwa), INTEGER_ARG(&nuw), UWORD_ARG(&uw) ); )
   printf( "The sum of the first %d integers is %d\n", nuw, uw );

/* Test the passing of character variables */

   F77_LOCK( F77_CALL(tc1)( CHARACTER_ARG(c1f) TRAIL_ARG(c1f) ); )
   cnf_imprt( c1f, sizeof c1f, c1c );
   printf( "The character variable has been set to: %s\n", c1c );

   nc = NC;
   F77_LOCK( F77_CALL(tc2)( CHARACTER_ARRAY_ARG(caf), INTEGER_ARG(&nc) TRAIL_ARG(caf) ); )
   for( j=0; j<NC ; j++ )
   {
      cnf_imprt( caf[j], (sizeof caf)/NC, c1c );
      printf( "The %dth element of the character array is: %s\n", j, c1c );
   }

   return EXIT_SUCCESS;
}
