#include <stdlib.h>
#include <stdio.h>
#include "ary.h"
#include "ary_err.h"
#include "mers.h"
#include "star/hds.h"
#include "dat_err.h"
#include "prm_par.h"
#include "sae_par.h"
#include <string.h>

int main(){
   int status_value = 0;

   Ary *ary2;
   Ary *ary3;
   Ary *ary4;
   Ary *ary;
   AryPlace *place = NULL;
   HDSLoc *loc = NULL;
   HDSLoc *loc2 = NULL;
   HDSLoc *loc3 = NULL;
   char buf[1000];
   char form[ARY__SZFRM+1];
   char ftype[ARY__SZFTP+1];
   char name2[DAT__SZNAM+1];
   char name3[DAT__SZNAM+1];
   char type[DAT__SZTYP+1];
   char sctype[ARY__SZTYP+1];
   char text[200];
   double *dpntr;
   double *drpntr;
   double *dipntr;
   hdsdim shift[2];
   double dsum;
   double scale;
   double zero;
   float zratio;
   hdsdim dims[ ARY__MXDIM ];
   hdsdim lbnd[ ARY__MXDIM ];
   hdsdim offs[ ARY__MXDIM ];
   hdsdim ubnd[ ARY__MXDIM ];
   int *ipntr;
   int *status = &status_value;
   int axis;
   int bad;
   int base;
   int can_lock;
   int defined;
   int isect;
   int ival;
   int mapped;
   int ndim;
   int ok;
   int oplen;
   int same;
   int temp;
   int there;
   size_t el2;
   size_t el;
   size_t i;
   size_t ngood;

   errMark();

/* Test accessing an existing array.
   ================================ */
   hdsOpen( "./test_array", "Read", &loc, status );

/* This test should only be done when linked with HDS V5
   ival = datLocked( loc, 0, status );
   if( ival != 3 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 1 (%d != 3 )", status, ival );
   }
*/

   aryFind( loc, "data_array", &ary, status );
   aryMsg( "A", ary );

   sprintf( buf, "%s/%s", getenv("PWD"), "./test_array.DATA_ARRAY" );
   msgLoad( " ", "^A", text, sizeof(text), &oplen, status );
   if( *status == SAI__OK && strcmp( text, buf ) ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 1a (%s)", status, text );
   }

   datFind( loc, "data_array", &loc2, status );
   aryImprt( loc2, &ary2, status );
   arySame( ary, ary2, &same, &isect, status );
   if( !same && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 1b", status );
   }

   aryLoc( ary, &loc3, status );
   datName( loc2, name2, status );
   datName( loc3, name3, status );
   datAnnul( &loc3, status );
   if( *status == SAI__OK && strcmp( name2, name3 ) ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 1b1", status );
   }

   aryAnnul( &ary2, status );
   datAnnul( &loc2, status );


   aryFtype( ary, ftype, status );
   if( strcmp( ftype, "_REAL" ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 1c (%s )", status, ftype );
   }

   aryIsacc( ary, "WRITE", &ok, status );
   if( ok && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 1c1 (%s )", status, ftype );
   }

   if( *status == SAI__OK ) {
      aryPtszD( ary, 2.0, -1.0, status );
      if( *status != ARY__ACDEN ) {
         if( *status == SAI__OK ) *status = SAI__ERROR;
         errRepf( " ", "Error 1c1a (%s )", status, ftype );
      } else {
         errAnnul( status );
      }
   }

   aryGtszD( ary, &scale, &zero, status );
   if( ( scale != 1.0 || zero != 0.0 ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 1c2", status );
   }

   aryIsmap( ary, &mapped, status );
   if( mapped && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 1c3", status );
   }

   lbnd[ 0 ] = 1000;
   lbnd[ 1 ] = 1;
   lbnd[ 2 ] = 1950;
   ubnd[ 0 ] = 1020;
   ubnd[ 1 ] = 16;
   ubnd[ 2 ] = 2040;
   arySect( ary, 3, lbnd, ubnd, &ary2, status );

   aryOffs( ary, ary2, 6, offs, status );
   for( i = 0; i < 6; i++ ) {
      if( offs[ i ] != 0 && *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( " ", "Error 1c4", status );
      }
   }

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

   if( *status == SAI__OK ) {
      aryMapz( ary2, "_DOUBLE", "Read", (void **) &drpntr, (void **) &dipntr,
               &el, status );
      if( *status != ARY__ISMAP ) {
         if( *status == SAI__OK ) *status = SAI__ERROR;
         errRepf( " ", "Error 4a0", status );
      } else {
         errAnnul( status );
      }
   }

   aryClone( ary2, &ary3, status );
   aryMapz( ary3, "_DOUBLE", "Read", (void **) &drpntr, (void **) &dipntr,
            &el, status );
   if( el != 30576 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4a1 (%ld != 30576)", status, el );
   } else if( *status == SAI__OK ) {
      dsum = 0.0;
      ngood = 0;
      for( i = 0; i < el; i++,drpntr++ ) {
         if( *drpntr != VAL__BADD ) {
            dsum += *drpntr;
            ngood++;
         }
      }
      if( ngood != 13650 ){
         *status = SAI__ERROR;
         errRepf( " ", "Error 4a2 (%ld != 13650)", status, ngood );
      } else if( dsum != 20666.916872823029 ){
         *status = SAI__ERROR;
         errRepf( " ", "Error 4a3 (%g != 20666.916872823)", status, dsum );
      }

      dsum = 0.0;
      ngood = 0;
      for( i = 0; i < el; i++,dipntr++ ) {
         if( *dipntr != VAL__BADD ) {
            dsum += *dipntr;
            ngood++;
         }
      }
      if( ngood != 30576 ){
         *status = SAI__ERROR;
         errRepf( " ", "Error 4a4 (%ld != 30576)", status, ngood );
      } else if( dsum != 0.0 ){
         *status = SAI__ERROR;
         errRepf( " ", "Error 4a5 (%g != 0.0)", status, dsum );
      }
   }
   aryAnnul( &ary3, status );

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

   aryIsbas( ary, &base, status );
   if( !base && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4b4", status );
   }

   aryIsbas( ary2, &base, status );
   if( base && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4b5", status );
   }

   aryIsmap( ary2, &mapped, status );
   if( !mapped && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4b6", status );
   }

   aryIstmp( ary2, &temp, status );
   if( temp && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4b7", status );
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

   hdsNew( "cary_test2", "TEST", "TEST", 0, 0, &loc2, status );
   aryPlace( loc2, "DATA_ARRAY", &place, status );
   aryCopy( ary2, &place, &ary3, status );
   aryBound( ary3, 3, lbnd, ubnd, &ndim, status );
   if( lbnd[ 0 ] != 1023 ||
       lbnd[ 1 ] != 7 ||
       lbnd[ 2 ] != 2008 ||
       ubnd[ 0 ] != 1023 ||
       ubnd[ 1 ] != 7 ||
       ubnd[ 2 ] != 2008 ||
       ndim != 3 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( " ", "Error 4f", status );
      }
   }

   aryClone( ary3, &ary4, status );
   if( !aryValid( ary4, status ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4f1", status );
   }
   datThere( loc2, "DATA_ARRAY", &there, status );
   if( !there && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4g", status );
   }
   aryDelet( &ary3, status );
   datThere( loc2, "DATA_ARRAY", &there, status );
   if( there && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4h", status );
   }
   if( aryValid( ary4, status ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4h1", status );
   }

   datAnnul( &loc2, status );

   aryTemp( &place, status );
   aryDupe( ary2, &place, &ary3, status );
   aryBound( ary3, 3, lbnd, ubnd, &ndim, status );
   if( lbnd[ 0 ] != 1023 ||
       lbnd[ 1 ] != 7 ||
       lbnd[ 2 ] != 2008 ||
       ubnd[ 0 ] != 1023 ||
       ubnd[ 1 ] != 7 ||
       ubnd[ 2 ] != 2008 ||
       ndim != 3 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( " ", "Error 4h2", status );
      }
   }

   aryState( ary2, &defined, status );
   if( !defined && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4h3", status );
   }

   aryState( ary3, &defined, status );
   if( defined && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4h4", status );
   }

   aryIstmp( ary3, &temp, status );
   if( !temp && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4h5", status );
   }

   aryAnnul( &ary3, status );

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

   aryForm( ary, form, status );
   if( strcmp( form, "SIMPLE") && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4e2", status );
   }

   aryClone( ary, &ary2, status );
   aryIsacc( ary2, "WRITE", &ok, status );
   if( !ok && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4e3 (%s )", status, ftype );
   }

   aryNoacc( "Write", ary2, status );
   aryIsacc( ary2, "WRITE", &ok, status );
   if( ok && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4e4 (%s )", status, ftype );
   }
   aryAnnul( &ary2, status );

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

   aryNdim( ary2, &ndim, status );
   if( ndim != 4 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 5a (%d != 4)", status, ndim );
   }

   aryAnnul( &ary2, status );
   aryAnnul( &ary, status );



   aryPlace( loc2, "image", &place, status );
   ubnd[ 0 ] = 2;
   ubnd[ 1 ] = 10;
   aryNewp( "_BYTE", 2, ubnd, &place, &ary, status );
   aryBound( ary, ARY__MXDIM, lbnd, ubnd, &ndim, status );
   if( ( lbnd[ 0 ] != 1 ) ||
       ( lbnd[ 1 ] != 1 ) ||
       ( ubnd[ 0 ] != 2 ) ||
       ( ubnd[ 1 ] != 10 ) ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( " ", "Error 5b", status );
      }
   }
   aryForm( ary, form, status );
   if( strcmp( form, "PRIMITIVE") && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 5c", status );
   }
   aryMap( ary, "_DOUBLE", "Write", (void **) &dpntr, &el, status );
   if( el != 20 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 5d (%ld != 20)", status, el );
   } else if( *status == SAI__OK ) {
      for( i = 0; i < el; i++,dpntr++ ) *dpntr = 1.0;
   }

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
   arySsect( ary, ary2, &ary3, status );
   aryBound( ary3, ARY__MXDIM, lbnd, ubnd, &ndim, status );
   if( ( lbnd[ 0 ] != -15 ) ||
       ( lbnd[ 1 ] != -20 ) ||
       ( lbnd[ 2 ] != -20 ) ||
       ( lbnd[ 3 ] != -10 ) ||
       ( ubnd[ 0 ] != 10 ) ||
       ( ubnd[ 1 ] != 0 ) ||
       ( ubnd[ 2 ] != 20 ) ||
       ( ubnd[ 3 ] != 40 ) ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( " ", "Error 11", status );
      }
   }
   aryAnnul( &ary3, status );

   aryMap( ary2, "_DOUBLE", "Update", (void **) &dpntr, &el, status );

   if( el != 1141686 && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 9 (%ld != 1141686)", status, el );
   } else if( *status == SAI__OK ) {
      for( i = 0; i < el; i++,dpntr++ ) *dpntr = 1.0;
   }

   aryAnnul( &ary2, status );

   if( *status == SAI__OK ) {
      aryBound( ary, 2, lbnd, ubnd, &ndim, status );
      if( *status != ARY__XSDIM ) {
         int lstat = *status;
         if( *status != SAI__OK ) errAnnul( status );
         *status = SAI__ERROR;
         errRepf( " ", "Error 10 (%d != %d)", status, lstat, ARY__XSDIM );
      } else {
         errAnnul( status );
      }
   }

   aryBound( ary, ARY__MXDIM, lbnd, ubnd, &ndim, status );
   if( ( lbnd[ 0 ] != -10 ) ||
       ( lbnd[ 1 ] != -30 ) ||
       ( lbnd[ 2 ] != -20 ) ||
       ( lbnd[ 3 ] != -50 ) ||
       ( ubnd[ 0 ] != 0 ) ||
       ( ubnd[ 1 ] != 10 ) ||
       ( ubnd[ 2 ] != 20 ) ||
       ( ubnd[ 3 ] != 30 ) ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( " ", "Error 11", status );
      }
   } else if( *status == SAI__OK ){
      for( i = 4; i < ARY__MXDIM; i++ ) {
         if( lbnd[i] != 1 || ubnd[i] != 1 ) {
            *status = SAI__ERROR;
            errRep( " ", "Error 12", status );
         }
      }
   }

   aryClone( ary, &ary2, status );
   arySame( ary, ary2, &same, &isect, status );
   if( !same && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 13", status );
   }

   aryCmplx( ary2, &ival, status );
   if( ival && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 14", status );
   }

   aryAnnul( &ary2, status );

   aryPtszD( ary, 2.0, -1.0, status );
   aryGtszD( ary, &scale, &zero, status );
   if( ( scale != 2.0 || zero != -1.0 ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 4h6", status );
   }
   aryForm( ary, form, status );
   if( strcmp( form, "SCALED") && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4h7", status );
   }
   arySctyp( ary, sctype, status );
   if( strcmp( sctype, "_UWORD") && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 4h8", status );
   }
   if( *status == SAI__OK ) {
      aryVerfy( ary, status );
      if( *status != SAI__OK ) {
         errRepf( " ", "Error 4h9", status );
      }
   }
   aryAnnul( &ary, status );




/* Test delta compression
   ====================== */

   aryTemp( &place, status );
   lbnd[ 0 ] = -10;
   lbnd[ 1 ] = -20;
   lbnd[ 2 ] = 0;
   ubnd[ 0 ] = 0;
   ubnd[ 1 ] = 10;
   ubnd[ 2 ] = 20;

   aryNew( "_INTEGER", 3, lbnd, ubnd, &place, &ary, status );
   aryMap( ary, "_INTEGER", "Write", (void **) &ipntr, &el, status );
   if( *status == SAI__OK ) {
      for( i = 0; i < el; i++ ) ipntr[i] = i;
   }
   aryUnmap( ary, status );

   aryDim( ary, ARY__MXDIM, dims, &ndim, status );
   if( ndim != 3 || dims[0] != 11 || dims[1] != 31 || dims[2] != 21 ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRepf( " ", "Error 14b", status );
      }
   }

   aryTemp( &place, status );
   aryDelta( ary, 0, " ", 0.8, &place, &zratio, &ary2, status );

   aryForm( ary2, form, status );
   if( strcmp( form, "DELTA") && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 14c", status );
   }

   if( ( zratio < 3.09731 || zratio > 3.09733 ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 15", status );
   }

   aryGtdlt( ary2, &axis, type, &zratio, status );
   if( ( zratio < 3.09731 || zratio > 3.09733 ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 16", status );
   }
   if( strcmp( type, "_BYTE" ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 17 (%s)", status, type );
   }
   if( axis != 2 && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 18", status );
   }

   if( *status == SAI__OK ) {
      aryVerfy( ary2, status );
      if( *status != SAI__OK ) {
         errRepf( " ", "Error 18b", status );
      }
   }

   aryMap( ary2, "_INTEGER", "Read", (void **) &ipntr, &el2, status );

   if( *status == SAI__OK ) {
      if( el != el2 ) {
         *status = SAI__ERROR;
         errRepf( " ", "Error 19", status );
      }

      for( i = 0; i < el; i++ ) {
         if( ipntr[i] != i ) {
            *status = SAI__ERROR;
            errRepf( " ", "Error 20 (%d != %zu)", status, ipntr[i], i );
         }
      }
   }
   aryUnmap( ary2, status );

   if( *status == SAI__OK ) {
      aryReset( ary2, status );
      if( *status != ARY__CMPAC ){
         if( *status == SAI__OK ) *status = SAI__ERROR;
         errRepf( " ", "Error 21", status );
      } else{
         errAnnul( status );
      }
   }

   aryBad( ary, 1, &bad, status );
   if( bad && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 22", status );
   }

   arySbad( 1, ary, status );
   aryBad( ary, 0, &bad, status );
   if( !bad && *status == SAI__OK ){
      *status = SAI__ERROR;
      errRepf( " ", "Error 23", status );
   }

   lbnd[ 0 ] = -10;
   lbnd[ 1 ] = -10;
   lbnd[ 2 ] = 0;
   ubnd[ 0 ] = 10;
   ubnd[ 1 ] = 10;
   ubnd[ 2 ] = 20;
   arySbnd( 3, lbnd, ubnd, ary, status );
   aryBound( ary, 3, lbnd, ubnd, &ndim, status );
   if( ( lbnd[ 0 ] != -10 ) ||
       ( lbnd[ 1 ] != -10 ) ||
       ( lbnd[ 2 ] != 0 ) ||
       ( ubnd[ 0 ] != 10 ) ||
       ( ubnd[ 1 ] != 10 ) ||
       ( ubnd[ 2 ] != 20 ) ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( " ", "Error 23a", status );
      }
   }

   aryState( ary, &defined, status );
   if( !defined && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 24", status );
   }
   aryReset( ary, status );
   aryState( ary, &defined, status );
   if( defined && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 25", status );
   }

   shift[ 0 ] = 1;
   shift[ 1 ] = -2;
   aryShift( 2, shift, ary, status );
   aryBound( ary, 3, lbnd, ubnd, &ndim, status );
   if( ( lbnd[ 0 ] != -9 ) ||
       ( lbnd[ 1 ] != -12 ) ||
       ( lbnd[ 2 ] != 0 ) ||
       ( ubnd[ 0 ] != 11 ) ||
       ( ubnd[ 1 ] != 8 ) ||
       ( ubnd[ 2 ] != 20 ) ) {
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;

         printf("%d %d %d %d %d %d\n", lbnd[0], lbnd[1], lbnd[2],
                                       ubnd[0], ubnd[1], ubnd[2] );

         errRep( " ", "Error 26", status );
      }
   }

   arySize( ary, &el, status );
   if( el !=
       ( ubnd[ 0 ] - lbnd[ 0 ] + 1 )*
       ( ubnd[ 1 ] - lbnd[ 1 ] + 1 )*
       ( ubnd[ 2 ] - lbnd[ 2 ] + 1 ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 27", status );
   }

   aryStype( "COMPLEX_REAL", ary, status );
   aryFtype( ary, ftype, status );
   if( strcmp( ftype, "COMPLEX_REAL" ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 28", status );
   }

   aryType( ary, type, status );
   if( strcmp( type, "_REAL" ) && *status == SAI__OK ) {
      *status = SAI__ERROR;
      errRepf( " ", "Error 29", status );
   }

   aryAnnul( &ary2, status );
   aryAnnul( &ary, status );

   errRlse();

   return *status;
}
