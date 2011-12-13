/*+
 * Name:
 *   gsd2_testcp

 * Purpose:
 *   Test the gsd2_copya.c functions

 * Language:
 *    ANSI C

 * Type of Module:
 *    C function.

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

 * Authors:
 *    hme: Horst Meyerdierks (UoE, Starlink)
 *    timj: Tim Jenness (JAC, Hawaii)

 * History:
 *    12 Dec 1994 (hme):
 *       Original.
 *    28 Mar 2004 (timj):
 *       Factor out into separate routine for autoconf tests

 * Copyright:
 *    Copyright (C) 1994-2004 Particle Physics and Astronomy Research Council.
 *    All Rights Reserved.

 *-

 */

#include <stdlib.h>
#include <stdio.h>
#include "gsd1.h"
#include "gsd2.h"

#define BUFSIZE 20

/* Generic routine for dumping the results from the conversions */
void dumpoutput ( int nelem, double darray[], float farray[], int iarray[],
		  short warray[], char larray[], signed char barray[] ) {
  int i;
  printf("  %23s %15s %12s %7s %2s %5s\n","D","R","I","W","L","B");
  for ( i = 0; i < BUFSIZE; i++ )
    (void) printf( "%2d %22.15g%s %14.8g%s %11d%s %6d%s %1d%s %4d%s\n",
		   i,
		   darray[i], (darray[i] == VAL__BADD ? "*" : " "),
		   farray[i], (farray[i] == VAL__BADR ? "*" : " "),
		   iarray[i], (iarray[i] == VAL__BADI ? "*" : " "),
		   warray[i], (warray[i] == VAL__BADW ? "*" : " "),
		   larray[i], (larray[i] == VAL__BADB ? "*" : " "),
		   barray[i], (barray[i] == VAL__BADB ? "*" : " ")
		   );

}

int main( void )
{
   double      darray[BUFSIZE];
   float       farray[BUFSIZE];
   int         iarray[BUFSIZE];
   short       warray[BUFSIZE];
   char        larray[BUFSIZE];
   signed char barray[BUFSIZE];
   int         i, status;

   darray[0]  =   0.;
   darray[1]  =  12.3456;
   darray[2]  = -78.9012;
   darray[3]  =  VAL__BADB;
   darray[4]  =  VAL__BADW;
   darray[5]  =  VAL__BADI;
   darray[6]  =  VAL__BADR;
   darray[7]  =  VAL__BADD;
   darray[8]  =  234.567;
   darray[9]  =  567.89;
   darray[10] =  34567.8;
   darray[11] =  67890.1;
   darray[12] =  2.34567e9;
   darray[13] =  4.56789e38;
   darray[14] = -234.567;
   darray[15] = -567.89;
   darray[16] = -34567.8;
   darray[17] = -67890.1;
   darray[18] = -2.34567e9;
   darray[19] = -4.56789e38;

   status = gsd2_copya( 6, 5, BUFSIZE,
      (unsigned char *) darray, (unsigned char *) farray );
   (void) printf( "%2d conversion errors D to R\n", status );

   status = gsd2_copya( 6, 4, BUFSIZE,
      (unsigned char *) darray, (unsigned char *) iarray );
   (void) printf( "%2d conversion errors D to I\n", status );

   status = gsd2_copya( 6, 3, BUFSIZE,
      (unsigned char *) darray, (unsigned char *) warray );
   (void) printf( "%2d conversion errors D to W\n", status );

   status = gsd2_copya( 6, 2, BUFSIZE,
      (unsigned char *) darray, (unsigned char *) larray );
   (void) printf( "%2d conversion errors D to L\n", status );

   status = gsd2_copya( 6, 1, BUFSIZE,
      (unsigned char *) darray, (unsigned char *) barray );
   (void) printf( "%2d conversion errors D to B\n", status );

   for ( i = 0; i < BUFSIZE; i++ )
      (void) printf( "%2d %22.15g %14.8g %11d %6d %1d %4d\n",
         i, darray[i], farray[i], iarray[i], warray[i], larray[i], barray[i] );

   dumpoutput( BUFSIZE, darray, farray,iarray,warray,larray,barray);

   farray[0]  =   0.;
   farray[1]  =  12.3456;
   farray[2]  = -78.9012;
   farray[3]  =  VAL__BADB;
   farray[4]  =  VAL__BADW;
   farray[5]  =  VAL__BADI;
   farray[6]  =  VAL__BADR;
   farray[7]  =  VAL__BADR;
   farray[8]  =  234.567;
   farray[9]  =  567.89;
   farray[10] =  34567.8;
   farray[11] =  67890.1;
   farray[12] =  2.34567e9;
   farray[13] =  VAL__BADR;
   farray[14] = -234.567;
   farray[15] = -567.89;
   farray[16] = -34567.8;
   farray[17] = -67890.1;
   farray[18] = -2.34567e9;
   farray[19] =  VAL__BADR;

   status = gsd2_copya( 5, 6, BUFSIZE,
      (unsigned char *) farray, (unsigned char *) darray );
   (void) printf( "%2d conversion errors R to D\n", status );

   status = gsd2_copya( 5, 4, BUFSIZE,
      (unsigned char *) farray, (unsigned char *) iarray );
   (void) printf( "%2d conversion errors R to I\n", status );

   status = gsd2_copya( 5, 3, BUFSIZE,
      (unsigned char *) farray, (unsigned char *) warray );
   (void) printf( "%2d conversion errors R to W\n", status );

   status = gsd2_copya( 5, 2, BUFSIZE,
      (unsigned char *) farray, (unsigned char *) larray );
   (void) printf( "%2d conversion errors R to L\n", status );

   status = gsd2_copya( 5, 1, BUFSIZE,
      (unsigned char *) farray, (unsigned char *) barray );
   (void) printf( "%2d conversion errors R to B\n", status );

   dumpoutput( BUFSIZE, darray, farray,iarray,warray,larray,barray);

   iarray[0]  =   0.;
   iarray[1]  =  12.3456;
   iarray[2]  = -78.9012;
   iarray[3]  =  VAL__BADB;
   iarray[4]  =  VAL__BADW;
   iarray[5]  =  VAL__BADI;
   iarray[6]  =  VAL__BADI;
   iarray[7]  =  VAL__BADI;
   iarray[8]  =  234.567;
   iarray[9]  =  567.89;
   iarray[10] =  34567.8;
   iarray[11] =  67890.1;
   iarray[12] =  VAL__BADI;
   iarray[13] =  VAL__BADI;
   iarray[14] = -234.567;
   iarray[15] = -567.89;
   iarray[16] = -34567.8;
   iarray[17] = -67890.1;
   iarray[18] =  VAL__BADI;
   iarray[19] =  VAL__BADI;

   status = gsd2_copya( 4, 6, BUFSIZE,
      (unsigned char *) iarray, (unsigned char *) darray );
   (void) printf( "%2d conversion errors I to D\n", status );

   status = gsd2_copya( 4, 5, BUFSIZE,
      (unsigned char *) iarray, (unsigned char *) farray );
   (void) printf( "%2d conversion errors I to R\n", status );

   status = gsd2_copya( 4, 3, BUFSIZE,
      (unsigned char *) iarray, (unsigned char *) warray );
   (void) printf( "%2d conversion errors I to W\n", status );

   status = gsd2_copya( 4, 2, BUFSIZE,
      (unsigned char *) iarray, (unsigned char *) larray );
   (void) printf( "%2d conversion errors I to L\n", status );

   status = gsd2_copya( 4, 1, BUFSIZE,
      (unsigned char *) iarray, (unsigned char *) barray );
   (void) printf( "%2d conversion errors I to B\n", status );

   dumpoutput( BUFSIZE, darray, farray,iarray,warray,larray,barray);

   warray[0]  =   0.;
   warray[1]  =  12.3456;
   warray[2]  = -78.9012;
   warray[3]  =  VAL__BADB;
   warray[4]  =  VAL__BADW;
   warray[5]  =  VAL__BADW;
   warray[6]  =  VAL__BADW;
   warray[7]  =  VAL__BADW;
   warray[8]  =  234.567;
   warray[9]  =  567.89;
   warray[10] =  VAL__BADW;
   warray[11] =  VAL__BADW;
   warray[12] =  VAL__BADW;
   warray[13] =  VAL__BADW;
   warray[14] = -234.567;
   warray[15] = -567.89;
   warray[16] =  VAL__BADW;
   warray[17] =  VAL__BADW;
   warray[18] =  VAL__BADW;
   warray[19] =  VAL__BADW;

   status = gsd2_copya( 3, 6, BUFSIZE,
      (unsigned char *) warray, (unsigned char *) darray );
   (void) printf( "%2d conversion errors W to D\n", status );

   status = gsd2_copya( 3, 5, BUFSIZE,
      (unsigned char *) warray, (unsigned char *) farray );
   (void) printf( "%2d conversion errors W to R\n", status );

   status = gsd2_copya( 3, 4, BUFSIZE,
      (unsigned char *) warray, (unsigned char *) iarray );
   (void) printf( "%2d conversion errors W to I\n", status );

   status = gsd2_copya( 3, 2, BUFSIZE,
      (unsigned char *) warray, (unsigned char *) larray );
   (void) printf( "%2d conversion errors W to L\n", status );

   status = gsd2_copya( 3, 1, BUFSIZE,
      (unsigned char *) warray, (unsigned char *) barray );
   (void) printf( "%2d conversion errors W to B\n", status );

   dumpoutput( BUFSIZE, darray, farray,iarray,warray,larray,barray);

   barray[0]  =   0.;
   barray[1]  =  12.3456;
   barray[2]  = -78.9012;
   barray[3]  =  VAL__BADB;
   barray[4]  =  VAL__BADB;
   barray[5]  =  VAL__BADB;
   barray[6]  =  VAL__BADB;
   barray[7]  =  VAL__BADB;
   barray[8]  =  VAL__BADB;
   barray[9]  =  VAL__BADB;
   barray[10] =  VAL__BADB;
   barray[11] =  VAL__BADB;
   barray[12] =  VAL__BADB;
   barray[13] =  VAL__BADB;
   barray[14] =  VAL__BADB;
   barray[15] =  VAL__BADB;
   barray[16] =  VAL__BADB;
   barray[17] =  VAL__BADB;
   barray[18] =  VAL__BADB;
   barray[19] =  VAL__BADB;

   status = gsd2_copya( 1, 6, BUFSIZE,
      (unsigned char *) barray, (unsigned char *) darray );
   (void) printf( "%2d conversion errors B to D\n", status );

   status = gsd2_copya( 1, 5, BUFSIZE,
      (unsigned char *) barray, (unsigned char *) farray );
   (void) printf( "%2d conversion errors B to R\n", status );

   status = gsd2_copya( 1, 4, BUFSIZE,
      (unsigned char *) barray, (unsigned char *) iarray );
   (void) printf( "%2d conversion errors B to I\n", status );

   status = gsd2_copya( 1, 2, BUFSIZE,
      (unsigned char *) barray, (unsigned char *) larray );
   (void) printf( "%2d conversion errors B to L\n", status );

   status = gsd2_copya( 1, 3, BUFSIZE,
      (unsigned char *) barray, (unsigned char *) warray );
   (void) printf( "%2d conversion errors B to W\n", status );

   dumpoutput( BUFSIZE, darray, farray,iarray,warray,larray,barray);

   larray[0]  =   0.;
   larray[1]  =  12.3456;
   larray[2]  = -78.9012;
   larray[3]  =  0;
   larray[4]  =  0;
   larray[5]  =  0;
   larray[6]  =  0;
   larray[7]  =  0;
   larray[8]  =  1;
   larray[9]  =  1;
   larray[10] =  1;
   larray[11] =  1;
   larray[12] =  1;
   larray[13] =  1;
   larray[14] =  1;
   larray[15] =  1;
   larray[16] =  1;
   larray[17] =  1;
   larray[18] =  1;
   larray[19] =  1;

   status = gsd2_copya( 2, 6, BUFSIZE,
      (unsigned char *) larray, (unsigned char *) darray );
   (void) printf( "%2d conversion errors L to D\n", status );

   status = gsd2_copya( 2, 5, BUFSIZE,
      (unsigned char *) larray, (unsigned char *) farray );
   (void) printf( "%2d conversion errors L to R\n", status );

   status = gsd2_copya( 2, 4, BUFSIZE,
      (unsigned char *) larray, (unsigned char *) iarray );
   (void) printf( "%2d conversion errors L to I\n", status );

   status = gsd2_copya( 2, 3, BUFSIZE,
      (unsigned char *) larray, (unsigned char *) warray );
   (void) printf( "%2d conversion errors L to W\n", status );

   status = gsd2_copya( 2, 1, BUFSIZE,
      (unsigned char *) larray, (unsigned char *) barray );
   (void) printf( "%2d conversion errors L to B\n", status );

   dumpoutput( BUFSIZE, darray, farray,iarray,warray,larray,barray);

   return EXIT_SUCCESS;
}

