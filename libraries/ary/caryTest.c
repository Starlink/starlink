#include <stdio.h>
#include "ary.h"
#include "mers.h"
#include "star/hds.h"
#include "prm_par.h"
#include "sae_par.h"

int main(){
   Ary *ary2;
   Ary *ary3;
   Ary *ary;
   AryPlace *place = NULL;
   HDSLoc *loc = NULL;
   HDSLoc *loc2 = NULL;
   double *dpntr;
   double dsum;
   hdsdim lbnd[ ARY__MXDIM ];
   hdsdim ubnd[ ARY__MXDIM ];
   int *ipntr;
   int status_value = 0;
   int *status = &status_value;
   int bad;
   int can_lock;
   int isect;
   int ival;
   int same;
   size_t el;
   size_t i;
   size_t ngood;

/* Test accessing an existing array.
   ================================ */

   hdsOpen( "./test_array", "Read", &loc, status );
   ival = datLocked( loc, status );
   if( ival == -1 ) {
      can_lock = 0;   /* HDS V4 - cannot lock objects */
   } else if( ival != 3 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 1 (%d != 3 )", status, ival );
   } else {
      can_lock = 1;
   }

   aryFind( loc, "data_array", &ary, status );
   lbnd[ 0 ] = 1000;
   lbnd[ 1 ] = 1;
   lbnd[ 2 ] = 1950;
   ubnd[ 0 ] = 1020;
   ubnd[ 1 ] = 16;
   ubnd[ 2 ] = 2040;
   arySect( ary, 3, lbnd, ubnd, &ary2, status );
   aryMap( ary2, "_DOUBLE", "Read", (void **) &dpntr, &el, status );
   if( el != 30576 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 2 (%ld != 30576)", status, el );
   } else if( *status == SAI__OK ) {
      dsum = 0.0;
      ngood = 0;
      for( i = 0; i < el; i++,dpntr++ ) {
         if( *dpntr != VAL__BADD ) {
            dsum += *dpntr;
            ngood++;
         }
      }
      if( ngood != 13650 ){
         *status = SAI__ERROR;
         errRepf( " ", "Error 3 (%ld != 13650)", status, ngood );
      } else if( dsum != 20666.916872823029 ){
         *status = SAI__ERROR;
         errRepf( " ", "Error 4 (%g != 20666.916872823)", status, dsum );
      }
   }

   aryBad( ary2, 1, &bad, status );
   if( !bad && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4c", status );
   }

   aryBad( ary2, 0, &bad, status );
   if( !bad && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4b", status );
   }

   arySame( ary, ary2, &same, &isect, status );
   if( !same && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4b1", status );
   }
   if( !isect && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4b2", status );
   }

   aryBase( ary2, &ary3, status );
   arySame( ary, ary3, &same, &isect, status );
   if( !same && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4b3", status );
   }

   aryAnnul( &ary3, status );
   aryAnnul( &ary2, status );

   lbnd[ 0 ] = 1023;
   lbnd[ 1 ] = 7;
   lbnd[ 2 ] = 2008;
   ubnd[ 0 ] = 1023;
   ubnd[ 1 ] = 7;
   ubnd[ 2 ] = 2008;
   arySect( ary, 3, lbnd, ubnd, &ary2, status );
   aryMap( ary2, "_DOUBLE", "Read", (void **) &dpntr, &el, status );

   aryBad( ary2, 1, &bad, status );
   if( bad && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4d", status );
   }

   aryBad( ary2, 0, &bad, status );
   if( !bad && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4e", status );
   }

   aryAnnul( &ary2, status );
   ary3 = ary;


/* Test creating a new array.
   ======================== */

   hdsNew( "cary_test", "TEST", "TEST", 0, 0, &loc2, status );
   aryPlace( loc2, "data_array", &place, status );
   lbnd[ 0 ] = -10;
   lbnd[ 1 ] = -30;
   lbnd[ 2 ] = -20;
   lbnd[ 3 ] = -50;
   ubnd[ 0 ] = 0;
   ubnd[ 1 ] = 10;
   ubnd[ 2 ] = 20;
   ubnd[ 3 ] = 30;

   aryNew( "_UWORD", 4, lbnd, ubnd, &place, &ary, status );
   aryMap( ary, "_INTEGER", "Write/ZERO", (void **) &ipntr, &el, status );
   aryUnmap( ary, status );

   arySame( ary, ary3, &same, &isect, status );
   if( same && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4e1", status );
   }

/* NB - THESE TWO CALLS FAIL IF THEY ARE SWAPPED !!! But the same
   happens with the F77 version of ARY, so presumably it's correct
   behaviour. */
   aryAnnul( &ary3, status );
   datAnnul( &loc, status );


   lbnd[ 0 ] = -15;
   lbnd[ 1 ] = -20;
   lbnd[ 2 ] = -20;
   lbnd[ 3 ] = -10;
   ubnd[ 0 ] = 10;
   ubnd[ 1 ] = 0;
   ubnd[ 2 ] = 20;
   ubnd[ 3 ] = 40;
   arySect( ary, 4, lbnd, ubnd, &ary2, status );
   aryMap( ary2, "_DOUBLE", "Update", (void **) &dpntr, &el, status );

   if( el != 1141686 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 5 (%ld != 1141686)", status, el );
   } else if( *status == SAI__OK ) {
      for( i = 0; i < el; i++,dpntr++ ) *dpntr = 1.0;
   }

   aryAnnul( &ary2, status );
   aryAnnul( &ary, status );
   datAnnul( &loc2, status );



   hdsOpen( "cary_test", "Read", &loc, status );
   aryFind( loc, "data_array", &ary, status );
   aryMap( ary, "_DOUBLE", "Read", (void **) &dpntr, &el, status );
   if( el != 1497771 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 6 (%ld != 1497771)", status, el );
   } else if( *status == SAI__OK ) {
      dsum = 0.0;
      ngood = 0;
      for( i = 0; i < el; i++,dpntr++ ) {
         if( *dpntr != VAL__BADD ) {
            dsum += *dpntr;
            ngood++;
         }
      }
      if( ngood != 1497771 ){
         *status = SAI__ERROR;
         errRepf( " ", "Error 7 (%ld != 1497771)", status, ngood );
      } else if( dsum != 388311.0 ){
         *status = SAI__ERROR;
         errRepf( " ", "Error 8 (%g != 388311.0)", status, dsum );
      }
   }
   aryAnnul( &ary, status );
   datAnnul( &loc, status );


/* Test creating a temporary array.
   =============================== */

   aryTemp( &place, status );
   lbnd[ 0 ] = -10;
   lbnd[ 1 ] = -30;
   lbnd[ 2 ] = -20;
   lbnd[ 3 ] = -50;
   ubnd[ 0 ] = 0;
   ubnd[ 1 ] = 10;
   ubnd[ 2 ] = 20;
   ubnd[ 3 ] = 30;

   aryNew( "_UWORD", 4, lbnd, ubnd, &place, &ary, status );
   aryMap( ary, "_INTEGER", "Write/ZERO", (void **) &ipntr, &el, status );
   aryUnmap( ary, status );

   lbnd[ 0 ] = -15;
   lbnd[ 1 ] = -20;
   lbnd[ 2 ] = -20;
   lbnd[ 3 ] = -10;
   ubnd[ 0 ] = 10;
   ubnd[ 1 ] = 0;
   ubnd[ 2 ] = 20;
   ubnd[ 3 ] = 40;
   arySect( ary, 4, lbnd, ubnd, &ary2, status );
   aryMap( ary2, "_DOUBLE", "Update", (void **) &dpntr, &el, status );

   if( el != 1141686 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 9 (%ld != 1141686)", status, el );
   } else if( *status == SAI__OK ) {
      for( i = 0; i < el; i++,dpntr++ ) *dpntr = 1.0;
   }

   aryAnnul( &ary2, status );
   aryAnnul( &ary, status );








   return *status;
}
