/**********************************************************************
   DSX_LUTC.C
-
 Contains:-
-
 DSX_LUTCOL    Set up color Look-up table for image display - FORTRAN interface (Xwindows)
 DSX_LUTROT    Rotate color Look-up table for image display - FORTRAN interface (Xwindows)
 DSX_LUTPAI    Paint a color in the LUT for image display - FORTRAN interface (XWindows)
 DSX_LUTSCA    Scale, shift color Look-up table for image display - FORTRAN interface (Xwindows)
 DSX_LUTBAR    Load/clear display of Look-up Table bar - FORTRAN interface (Xwindows)
 DSX_LUTPUT    Put a LUT into operation for image display - FORTRAN interface (Xwindows)

 DSXC_LUTCOL     Set up color Look-up table for image display (XWindows)
 DSXC_LUTACOL    Set up color Look-up table for image display (XWindows)
 DSXC_LUTROT     Rotate color Look-up table for image display (XWindows)
 DSXC_LUTSCA     Scale, shift color Look-up table for image display (XWindows)
 DSXC_LUTPAI     Paint a color in the LUT for image display (XWindows)
 DSXC_LUTPUT     Put a LUT into operation for image display (XWindows)
 DSXC_LUTBAR     Load/clear display of Look-up Table bar
 DSXC_LUTBARL    Load 'background' of LUT bar
 DSXC_LUTTRAN    Translate bar posn to LUT posn
 DSXC_LUTLOAD    Load LUT into working LUT
 DSXC_LUTVGET    Copy 'LUT.INC' Fortran variables over in this C
 DSXC_LUTVPUT    Copy 'LUT.H' C variables over into Fortran vserionC
*/


#include <stdlib.h>
#include <math.h>

#include <X11/Xlib.h>
#include <X11/Xos.h>

#include "f77.h"
#include "cnf.h"

#include "malloc.h"

#include "ds_gen.h"
#include "ds_panel.h"
#include "ds_lut.h"
#include "dsx_lut.h"
#include "lut.h"

/* Exterbal F77 routines */

extern void F77_EXTERNAL_NAME(ds_lutacol) ( );

/* External C routines */

extern void vtc_tsv ( int, int, int*, int* );
extern void c_printo ( char* );
extern void dsxc_pscur ( float, float );
extern void dsxc_waitbut ( Bool, Bool, int*, int*, int* );
extern void dsxc_getcurpb ( Bool, int*, int*, int[], int* );
extern void dsxc_putim ( char*, int, int, int, int );
extern int imax ( int, int );
extern int imin ( int, int );
extern float fmax ( float, float );
extern float fmin ( float, float );

char   TEXTAA[200];
/*
  (void) sprintf ( TEXTAA, " F1XXX %d %d %d %d %d ",
                          PDSOPEN, DSNXS, DSNXE, DSNYS, DSNYE );
   (void) c_printo ( TEXTAA );
*/

/***********************************************************************
  DSX_LUTCOL -- Set up color Look-up table for image display - FORTRAN interface (Xwindows)

    alan penny                ral              1993 Aug
*/

F77_SUBROUTINE(dsx_lutcol) (void)

/* C-- */
{
/* Cbegin */

       (void) dsxc_lutcol ( );

}


/***********************************************************************
  DSX_LUTROT -- Rotate color Look-up table for image display - FORTRAN interface (Xwindows)

    alan penny                ral              1993 Aug
*/

F77_SUBROUTINE(dsx_lutrot) (void)

/* C-- */
{
/* Cbegin */

       (void) dsxc_lutrot ( );

}


/***********************************************************************
  DSX_LUTPAI -- Paint a color in the LUT for image display (FORTRAN interface) (XWindows)

    alan penny                ral              1993 Aug
*/

F77_SUBROUTINE(dsx_lutpai) (void)

/* C-- */
{
/* Cbegin */

       (void) dsxc_lutpai ( );

}


/***********************************************************************
  DSX_LUTSCA -- Scale, shift color Look-up table for image display - FORTRAN interface (Xwindows)

    alan penny                ral              1993 Aug
*/

F77_SUBROUTINE(dsx_lutsca) (void)

/* C-- */
{
/* Cbegin */

       (void) dsxc_lutsca ( );

}


/***********************************************************************
  DSX_LUTBAR -- Load/clear display of Look-up Table bar - FORTRAN interface (Xwindows)

    alan penny                ral              1993 Aug
*/

F77_SUBROUTINE(dsx_lutbar) ( INTEGER(kopt) )

    GENPTR_INTEGER(kopt)         /* i: Option Flag 1=load; 0= clear */
/* C-- */
{
      int ka;
/* Cbegin */

      ka = *kopt;
      (void) dsxc_lutbar ( ka );

}


/***********************************************************************
  DSX_LUTPUT -- Put a LUT into operation for image display - FORTRAN interface (Xwindows)

    alan penny                ral              1993 Aug
*/

F77_SUBROUTINE(dsx_lutput) (void)

/* C-- */
{
/* Cbegin */

       (void) dsxc_lutput ( );

}



/***************************************************************************
 DSXC_LUTCOL -- Set up color Look-up table for image display (XWindows)

   alan penny                  ral                        1990-02-03
   pat morris                  leeds                      1992 feb
*/

dsxc_lutcol (void)

/* C-- */
{
      int   k, ka, kdiv;
      Status istat;
      unsigned short int c;
      char flags;
/* Cbegin */


      DSOPEN = F77_NAMED_COMMON(ds_genb).dsopen;
      PDSOPEN = F77_NAMED_COMMON(ds_panelb).pdsopen;
      NUMDDCOL = F77_NAMED_COMMON(lutacom).numddcol;

      flags = DoRed | DoGreen | DoBlue;

      if ( DSOPEN || PDSOPEN ) {

         (void) dsxc_lutvget();

         if ( OWNCOL ) {

            kdiv = LVX/NUMDDCOL;
            for ( k = 15; k<15+LVX; k++ ) {
               ka = 15 + ((k-15)/kdiv);
               COLOUR[ka].pixel = PC_ID[ka];
               COLOUR[ka].flags = flags;
               c = LUTC_VAL[(k-15)][0]*65535.0;
               COLOUR[ka].red = c;
               c = LUTC_VAL[(k-15)][1]*65535.0;
               COLOUR[ka].green = c;
               c = LUTC_VAL[(k-15)][2]*65535.0;
               COLOUR[ka].blue = c;
            }

            XStoreColors ( VD_ID, CM_ID, COLOUR, 15+NUMDDCOL );
            XInstallColormap ( VD_ID, CM_ID );
            XFlush ( VD_ID );

         }
         else {

            for (k = 15; k<15+LVX; k++ ) {
               COLOUR[k].flags = flags;
               c = LUTC_VAL[(k-15)][0]*65535.0;
               COLOUR[k].red = c;
               c = LUTC_VAL[(k-15)][1]*65535.0;
               COLOUR[k].green = c;
               c = LUTC_VAL[(k-15)][2]*65535.0;
               COLOUR[k].blue = c;
               istat = XAllocColor ( VD_ID, CM_ID, &COLOUR[k] );
               if ( istat==0 ) {
                  (void) c_printo ( "ERROR,x_lut: Colour pixel not allocated" );
                  PC_ID[k] = 0;
               }
               else
                  PC_ID[k] = COLOUR[k].pixel;
            }
            XFlush ( VD_ID );

         }

      }

}



/***************************************************************************
 DSXC_LUTACOL -- Set up color Look-up table for image display (XWindows)

   alan penny                  ral                        1990-02-03
   pat morris                  leeds                      1992 feb
*/

dsxc_lutacol (void)

/* C-- */
{
/* Cbegin */

      (void) F77_CALL(ds_lutacol) ( );

}

/*****************************************************************
 DSXC_LUTROT -- Rotate color Look-up table for image display (XWindows)

   alan penny                  ral                        1990-02-03
   pat morris                  leeds                      1992 feb
*/

dsxc_lutrot (void)

/* C-- */
{
      float rot, ta[LVX], rv;
      int j, k, kd, istat, kb[3], kbut, ja, kpx, kpy, kpxa, kpya, kdpx;
      Bool loop;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;

      rv = (float) (DSSNX)/2.0;
      (void) dsxc_pscur ( rv, 10.0 );					/*Set cursor at bar*/

      (void) dsxc_lutvget();

      (void) dsxc_waitbut ( True, False, &kbut, &kpxa, &kpya );		/*Buttons up?*/

      loop = True;
      while ( loop ) {
         (void) dsxc_waitbut ( True, True, &kbut, &kpxa, &kpya );		/*Button pressed?*/
         if ( kpxa>=0 && kpxa<DSSNX && kpya>=0 && kpya<DSSNY ) loop = False;
      }

      loop = True;
      while ( loop ) {

         (void) dsxc_mswait ( 50 );
         (void) dsxc_getcurpb ( True, &kpx, &kpy, kb, &istat );

         kbut = 0;
         if ( istat==0 ) {
            if ( (kb[0]==1) || (kb[1]==1) || (kb[2]==1) ) {
               if ( kb[0]==1 ) kbut = 1;
               if ( kb[1]==1 ) kbut = 2;
               if ( kb[2]==1 ) kbut = 3;
            }
         }

         kdpx = kpx - kpxa ;
         if ( kbut==3 )
            loop = False;
         else if ( kbut==0 )
            kpxa = kpx;
         else if ( kpx>=0 && kpx<DSSNX && kdpx!=0 ) {
            kpxa = kpx;
            rot = (float) (kdpx)/( LUTC_SC * (float) DSSNX );
            kd = rot * (float) LVX;
            for ( k = 0; k<=2 ;k++ ) {
               for ( j=0 ; j<LVX; j++ ) {
                  ja = j - kd;
                  if ( ja<0 ) ja += LVX;
                  if ( ja>=LVX ) ja = ja % (LVX-1);
                  ta[j] = LUTC_VAL[ja][k];
               }
               for ( j=0 ; j<LVX ; j++ )
                  LUTC_VAL[j][k] = ta[j] ;
            }
            (void) dsxc_lutvput();
            (void) dsxc_lutput ();
         }

      }


      DSCURPOSX = kpx;						/*Store cursor screen posn*/
      DSCURPOSY = kpy;
      DSCURSET = True;

      F77_NAMED_COMMON(ds_gen).dscurposx = kpx;
      F77_NAMED_COMMON(ds_gen).dscurposy = kpy;
      F77_NAMED_COMMON(ds_genb).dscurset = F77_TRUE;


}

/***************************************************************************
 DSXC_LUTSCA -- Scale, shift color Look-up table for image display (XWindows)

   alan penny                  ral                        1990-02-03
   pat morris                  leeds                      1992 feb
*/

dsxc_lutsca (void)

/* C-- */
{
      float rv;
      int istat, kb[3], kbut, kpx, kpy, kpxa, kpya, kdpx, kdpy;
      Bool loop;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;
      (void) dsxc_lutvget();

      rv = DSSNX/2;
      (void) dsxc_pscur ( rv, 10.0 );						/*Set cursor on bar*/

      (void) dsxc_waitbut ( True, False, &kbut, &kpxa, &kpya );			/*Buttons up?*/

      loop = True;
      while ( loop ) {
         (void) dsxc_waitbut ( True, True, &kbut, &kpxa, &kpya );		/*Button pressed?*/
         if ( kpxa>=0 && kpxa<DSSNX && kpya>=0 && kpya<DSSNY ) loop = False;
      }
      loop = True;
      while ( loop ) {

         (void) dsxc_mswait ( 50 );
         (void) dsxc_getcurpb ( True, &kpx, &kpy, kb, &istat );

         kbut = 0;
         if ( istat==0 ) {
            if ( (kb[0]==1) || (kb[1]==1) || (kb[2]==1) ) {
               if ( kb[0]==1 ) kbut = 1;
               if ( kb[1]==1 ) kbut = 2;
               if ( kb[2]==1 ) kbut = 3;
            }
         }

         kdpx = kpx - kpxa;
         kdpy = kpy - kpya;

         if ( kbut==3 )
            loop = False;
         else if ( kbut==0 ) {
            kpxa = kpx;
            kpya = kpy;
         } else {
            if ( kpx>=0 && kpx<DSSNX && kpy>=0 && kpy<DSSNY ) {
               if ( kdpx!=0 ) {
                  kpxa = kpx;
                  LUTC_ZE = LUTC_ZE + (float) (kdpx)/ (float) (DSSNX);
               }
               if ( abs(kdpy)>=1 ) {
                  kpya = kpy;
                  rv = LUTC_SC * (1.0+( (float) (kdpy)/ (float) (DSSNY)));
                  LUTC_ZE = LUTC_ZE + (LUTC_SC - rv) * ((float) (kpx)/(float) (DSSNX));
                  LUTC_SC = rv;
               }
               if ( abs(kdpx)>=1 || abs(kdpy)>=1 ) {
                  F77_NAMED_COMMON(lutacom).lut_sc = LUTC_SC;
                  F77_NAMED_COMMON(lutacom).lut_ze = LUTC_ZE;
                  (void) dsxc_lutput ();
              }
            }
         }

      }

      DSCURSET = True;					/*Store cursor screen posn*/
      DSCURPOSX = kpx;
      DSCURPOSY = kpy;

      F77_NAMED_COMMON(ds_gen).dscurposx = kpx;
      F77_NAMED_COMMON(ds_gen).dscurposy = kpy;
      F77_NAMED_COMMON(ds_genb).dscurset = F77_TRUE;


}

/************************************************************************
  DSXC_LUTPAI -- Paint a color in the LUT for image display (XWindows)

   alan penny                  ral                        1990-02-03
   pat morris                  leeds                      1992 feb
*/

dsxc_lutpai (void)

/* C-- */
{
      float  col[3], acol[3], rv;
      int    j, istat, kb[3], kpx, kpy, kbut, js, je,
             kxss, kxse, kpxa;
      Bool   loop, loopmain;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;

      kxss = 0;
      kxse = DSSNX;
      if ( DSSNX>260 ) {
         kxss = (DSSNX-260)/2;
         kxse = kxss + 260 - 1;
      }

      rv = DSSNX / 2.0;
      (void) dsxc_pscur ( rv, 10.0 );					/* Set cursor at bar centre */

      (void) dsxc_waitbut ( True, False, &kbut, &kpx, &kpy );		/* Buttons up in area? */

      loopmain = True;						/* Loop repeating sequence */
      while ( loopmain ) {

         (void) dsxc_waitbut ( True, True, &kbut, &kpx, &kpy );

         if ( kbut==3 )					/* Exit if button 3 */
            loopmain = False;
         else if ( (kpx<kxss) || (kpx>kxse) )
            (void) c_printo ( "Must be within LUT bar" );
         else {
            rv = (float) (kpx-kxss)/(float) (kxse-kxss+1);
            (void) dsxc_luttran ( rv, col, &je );

            (void) dsxc_waitbut ( True, False, &kbut, &kpx, &kpy );	/* Buttons up in area? */

            (void) dsxc_waitbut ( True, True, &kbut, &kpx, &kpy );	/* Buttons pressed in area? */
            kpx = imin(kxse,imax(kxss,kpx));
            rv = (float) (kpx-kxss)/(float) (kxse-kxss+1);
            kpxa = kpx;
            (void) dsxc_luttran ( rv, acol, &js );

            loop = True;
            while ( loop ) {
               (void) dsxc_getcurpb ( True, &kpx, &kpy, kb, &istat );
               kpx = imin(kxse,imax(kxss,kpx));
               if ( istat==0 ) {
                  if ( (kb[0]==0) && (kb[1]==0) && (kb[2]==0) )
                     loop = False;
               }
               if ( loop && (istat==0) && (kpx>kpxa) ) {
                  rv = (float) (kpx-kxss)/(float) (kxse-kxss+1);
                  (void) dsxc_luttran ( rv, acol, &je );
                  if ( je<js ) {
                     for (j = js-1; j<LVX; j++ ) {
                        LUTC_VAL[j][0] = col[0];
                        LUTC_VAL[j][1] = col[1];
                        LUTC_VAL[j][2] = col[2];
                     }
                     for (j = 0; j<je; j++ ) {
                        LUTC_VAL[j][0] = col[0];
                        LUTC_VAL[j][1] = col[1];
                        LUTC_VAL[j][2] = col[2];
                     }
                  }
                  else {
                     for (j = js-1; j<je; j++ ) {
                        LUTC_VAL[j][0] = col[0];
                        LUTC_VAL[j][1] = col[1];
                        LUTC_VAL[j][2] = col[2];
                     }
                  }
                  (void) dsxc_lutvput();
                  (void) dsxc_lutput ();
               }
            }

         }

      }

      (void) dsxc_lutvput();

      DSCURPOSX = kpx;						/*Store cursor screen posn*/
      DSCURPOSY = kpy;
      DSCURSET = True;

      F77_NAMED_COMMON(ds_gen).dscurposx = kpx;
      F77_NAMED_COMMON(ds_gen).dscurposy = kpy;
      F77_NAMED_COMMON(ds_genb).dscurset = F77_TRUE;


}


/******************************************************************************
 DSXC_LUTPUT -- Put a LUT into operation for image display (XWindows)

   alan penny                  ral                        1990-02-03
   pat morris                  leeds                      1992 feb
*/

dsxc_lutput (void)

/* C-- */
{
      float tcol[LVX*3];
      int   k, istat, ka, kdiv;

      char  flags;
      unsigned short int j;
/* Cbegin */


      DSOPEN = F77_NAMED_COMMON(ds_genb).dsopen;
      PDSOPEN = F77_NAMED_COMMON(ds_panelb).pdsopen;
      NUMDDCOL = F77_NAMED_COMMON(lutacom).numddcol;

      (void) dsxc_lutload ( tcol );	 			/* Load LUT into working LUT */

      flags = DoRed | DoGreen | DoBlue;

      if ( DSOPEN || PDSOPEN ) {

         if ( OWNCOL ) {

            kdiv = LVX/NUMDDCOL;
            for ( k = 15; k<15+LVX; k++ ) {
               ka = 15 + ((k-15)/kdiv);
               COLOUR[ka].pixel = PC_ID[ka];
               COLOUR[ka].flags = flags;
               j = tcol[3*(k-15)+0]*65535.0;
               COLOUR[ka].red = j;
               j = tcol[3*(k-15)+1]*65535.0;
               COLOUR[ka].green = j;
               j = tcol[3*(k-15)+2]*65535.0;
               COLOUR[ka].blue = j;
            }

            XStoreColors ( VD_ID, CM_ID, COLOUR, 15+NUMDDCOL );
            XInstallColormap ( VD_ID, CM_ID );
            XFlush ( VD_ID );
         }
         else {

            for ( k = 15; k < 15+LVX; k++ ) {
               COLOUR[k].flags = flags;
               j = tcol[3*(k-15)+0]*65535.0;
               COLOUR[k].red = j;
               j = tcol[3*(k-15)+1]*65535.0;
               COLOUR[k].green = j;
               j = tcol[3*(k-15)+2]*65535.0;
               COLOUR[k].blue = j;
               istat = XAllocColor ( VD_ID, CM_ID, &COLOUR[k] );
               if ( istat==0 ) {
                  (void) c_printo ( "ERROR: Colour pixel not allocated" );
                  PC_ID[k] = 0 ;
               }
               else
                  PC_ID[k] = COLOUR[k].pixel;
            }
            XFlush ( VD_ID );

         }

      }


}

/*********************************************************************
  DSXC_LUTBAR -- Load/clear display of Look-up Table bar (XWindows)

   alan penny                        ral              1990-01-31
   pat morris                        leeds            1992 feb
*/

dsxc_lutbar ( kopt )

   int kopt;		/* i: Option Flag 1=load; 0= clear */

/* C-- */
{
      int     kxss, kxse, kyss, kyse, kxvs, kxve, kyvs, kyve,
               i, j, kxcs, kxce, kycs, kyce, ncx, ncy,
               kx, ky, kxos, kyos, kyoe, iv;
      float   rva;
      char    byteim[2048], xdu, xdd, *ip, *tip;
/* Cbegin */


      DSSNX = F77_NAMED_COMMON(ds_gen).dssnx;
      DSSNY = F77_NAMED_COMMON(ds_gen).dssny;

      xdu = BY_PC_ID[14];
      xdd = BY_PC_ID[10];

      kxss = 0;
      kxse = DSSNX-1;
      if ( DSSNX>260 ) {
         kxss = (DSSNX-260)/2;
         kxse = kxss + 260 - 1;
      }
      kyss = 0;
      kyse = 17;
      kx = kxse - kxss + 1;
      ky = kyse - kyss + 1;
      kxcs = kxss + 2;
      kxce = kxse - 2;
      kycs = kyss + 2;
      kyce = kyse - 2;
      ncx = kxce - kxcs + 1;
      ncy = kyce - kycs + 1;

      if ( kopt==1 ) {
         ip = calloc ( kx*ky+1, sizeof(char) );
         (void) dsxc_lutbarl ( ip, kx, ky, xdu, xdd );
         if ( (ncx>1) && (ncy>0) ) {
            for ( j = 0; j<ncx; j++ ) {
               rva = (float) j*(float) (LVX-1)/((float) (ncx-1));
               rva = fmax(-1.0e6,rva);
               rva = fmin(1.0e6,rva);
               iv = 15 + (int) rva;
               byteim[j] = BY_PC_ID[iv];
            }
            kxos = kxcs - kxss + 1;
            kyos = kycs - kyss + 1;
            kyoe = kyos + ncy - 1;
            for ( j = kyos; j<=kyoe; j++ ) {
               for ( i = 0; i<ncx; i++ ) {
                  ip[kxos+i+j*kx] = byteim[i];
               }
            }
         }
         (void) dsxc_putim ( ip, kx, ky, kxss, kyss );
         free ( ip );
      }

      if ( kopt==0 ) {
         kyse = kyse + 3;
         vtc_tsv ( kxss, kyss, &kxvs, &kyvs );
         vtc_tsv ( kxse, kyse, &kxve, &kyve );
         kxvs = imax(0,imin(DSSNX,(kxvs-1)));
         kxve = imax(0,imin(DSSNX,(kxve+1)));
         kyvs = imax(0,imin(DSSNY,(kyvs-1)));
         kyve = imax(0,imin(DSSNY,(kyve+1)));
         (void) dsxc_vtim ( kxvs, kxve, kyvs, kyve, 0 );
      }


}


/****************************************************************
  DSXC_LUTBARL -- Load 'background' of LUT bar

    alan penny           ral                       1990-02-01
*/

dsxc_lutbarl ( im, kx, ky, xdu, xdd )

      char       im[];		/* o: Colour bar */
      int        kx;		/* i: X size of bar */
      int        ky;		/* i: Y size of bar */
      char       xdu;		/* i: Up colour code */
      char       xdd;		/* i: Down colour code */

/* C-- */
{
      int  j, ja, jb, k, ka, kb, lx, ly;
/* Cbegin */


      lx = imax(2,(kx/10));
      ly = imax(2,(ky/3));

      for ( k=0; k<=kx*ky-1; k++ ){
         im[k] = xdd;
      }

      for ( k=1; k<=ky; k=k+2*ly ){
         for ( ka=1; ka<=ly; ka++ ){
            kb = k + ka - 1;
            if ( kb<=ky ) {
               for ( j=1; j<=kx; j=j+2*lx ){
                  for ( ja=1; ja<=lx; ja++ ){
                     jb = j + ja - 1;
                     if ( jb<=kx ) im[(jb-1)+(kb-1)*kx] = xdu;
                  }
               }
               if ( jb<kx ) {
                  for ( j=jb; j<=kx; j++ ){
                     im[(j-1)+(kb-1)*kx] = xdu;
                  }
               }
            }
         }
      }

      if ( kb<ky ) {
         for ( k=kb; k<=ky; k++ ) {
            for ( j=1; j<=kx; j=j+2*lx ) {
               for ( ja=1; j<=lx; ja++ ) {
                  jb = j + ja - 1;
                  if ( jb<=kx ) im[(jb-1)+(k-1)*kx] = xdu ;
               }
            }
         }
      }


}


/******************************************************************
 DSXC_LUTTRAN -- Translate bar posn to LUT posn

   alan penny                  ral                        1990-02-03
*/

dsxc_luttran ( rv, col, jm )

      float    rv;		/* i: Posn in bar (0.0-0.999) */
      float    col[];		/* o: Colour in LUT */
      int      *jm;		/* o: Posn in LUT */
/* C-- */
{
      int k;
      float st[3], en[3];
/* Cbegin */


      (void) dsxc_lutvget();

      if ( LUTC_ENDS==1 ) {
         if ( LUTC_FLIPPED ) {
            st[0] = 1.0; st[1] = 1.0; st[2] = 1.0;
            en[0] = 0.0; en[1] = 0.0; en[2] = 0.0;
         }else{
            st[0] = 0.0; st[1] = 0.0; st[2] = 0.0;
            en[0] = 1.0; en[1] = 1.0; en[2] = 1.0;
         }
      }else if ( LUTC_ENDS==2 ) {
         if ( LUTC_FLIPPED ) {
            st[0] = 0.0; st[1] = 0.0; st[2] = 0.0;
            en[0] = 1.0; en[1] = 1.0; en[2] = 1.0;
         }else{
            st[0] = 1.0; st[1] = 1.0; st[2] = 1.0;
            en[0] = 0.0; en[1] = 0.0; en[2] = 0.0;
         }
      }else if ( LUTC_ENDS==3 ) {
         st[0] = LUTC_VAL[0][0];
         st[1] = LUTC_VAL[0][1];
         st[2] = LUTC_VAL[0][2];
         en[0] = LUTC_VAL[LVX-1][0];
         en[1] = LUTC_VAL[LVX-1][1];
         en[2] = LUTC_VAL[LVX-1][2];
      }

      rv = (rv-LUTC_ZE)/LUTC_SC;
      *jm = 1 + (int) ((float) (LVX-1)*rv);

      if ( *jm<1 ) {
         if ( LUTC_ENDS==4 ) {
            while ( *jm<1 ) {
               *jm = *jm + LVX;
            }
            for ( k=0; k<=2; k++ ) {
               col[k] = LUTC_VAL[*jm-1][k];
            }
         }else{
            *jm = 1;
            col[0] = st[0]; col[1] = st[1]; col[2] = st[2];
         }
      }else if ( *jm>LVX ) {
         if ( LUTC_ENDS==4 ) {
            while ( *jm>LVX ) {
               *jm = *jm - LVX ;
            }
            for ( k=0; k<=2; k++ ) {
               col[k] = LUTC_VAL[*jm-1][k];
            }
         }else{
            *jm = LVX;
            col[0] = en[0]; col[1] = en[1]; col[2] = en[2];
         }
      }else{
          col[0] = LUTC_VAL[*jm-1][0];
          col[1] = LUTC_VAL[*jm-1][1];
          col[2] = LUTC_VAL[*jm-1][2];
      }


}


/******************************************************************
 DSXC_LUTLOAD -- Load LUT into working LUT

   alan penny                  ral                        1990-02-03
*/

dsxc_lutload ( tcol )

      float    tcol[];		/* o: Working LUT */
/* C-- */
{
      float st[3], en[3], rv;
      int jb, k, ja;

/* Cbegin */


      (void) dsxc_lutvget();

      if ( LUTC_ENDS==1 ) {
         if ( LUTC_FLIPPED ) {
            st[0]=1.0; st[1]=1.0; st[2]=1.0;
            en[0]=0.0; en[1]=0.0; en[2]=0.0;
         }else{
            st[0]=0.0; st[1]=0.0; st[2]=0.0;
            en[0]=1.0; en[1]=1.0; en[2]=1.0;
         }
      }else if ( LUTC_ENDS==2 ) {
         if ( LUTC_FLIPPED ) {
            st[0]=0.0; st[1]=0.0; st[2]=0.0;
            en[0]=1.0; en[1]=1.0; en[2]=1.0;
         }else{
            st[0]=1.0; st[1]=1.0; st[2]=1.0;
            en[0]=0.0; en[1]=0.0; en[2]=0.0;
         }
      }else if ( LUTC_ENDS==3 ) {
         st[0] = LUTC_VAL[0][0];
         st[1] = LUTC_VAL[0][1];
         st[2] = LUTC_VAL[0][2];
         en[0] = LUTC_VAL[LVX-1][0];
         en[1] = LUTC_VAL[LVX-1][1];
         en[2] = LUTC_VAL[LVX-1][2];
      }

      if ( LUTC_ENDS==4 ) {
         for ( jb=1; jb<=LVX; jb++ ) {
            rv = (float) (jb-1)/(float) (LVX-1);
            ja = 1.0 + ((float)(LVX-1))*(rv-LUTC_ZE)/LUTC_SC;
            while ( ja<1 ) {
               ja = ja + LVX;
            }
            while ( ja>LVX ) {
               ja = ja - LVX;
            }
            for ( k=0; k<=2; k++ ) {
               tcol[3*(jb-1)+k] = LUTC_VAL[ja-1][k];
            }
         }
      }else{
         for ( jb=1; jb<=LVX; jb++ ) {
            rv = (float) (jb-1)/(float) (LVX-1);
            ja = 1.0 + ((float)(LVX-1))*(rv-LUTC_ZE)/LUTC_SC;
            if ( ja<1 ) {
               for ( k=0; k<=2; k++ ) {
                  tcol[3*(jb-1)+k] = st[k];
               }
            }else if ( ja>LVX ) {
               for ( k=0; k<=2; k++ ) {
                  tcol[3*(jb-1)+k] = en[k];
               }
            }else{
               for ( k=0; k<=2; k++ ) {
                  tcol[3*(jb-1)+k] = LUTC_VAL[ja-1][k];
               }
            }
         }
      }


}
/***************************************************************************
 DSXC_LUTVGET -- Copy 'LUT.INC' Fortran variables over in this C

   alan penny                  ral                        1993 Aug
*/

dsxc_lutvget (void)

/* C-- */
{
      int   j, k;
/* Cbegin */


      LUTC_STORED  = F77_NAMED_COMMON(lutacom).lut_stored;
      LUTC_ENDS    = F77_NAMED_COMMON(lutacom).lut_ends;
      LUTC_NUM     = F77_NAMED_COMMON(lutacom).lut_num;
      LUTC_SC      = F77_NAMED_COMMON(lutacom).lut_sc;
      LUTC_ZE      = F77_NAMED_COMMON(lutacom).lut_ze;
      LUTC_FLIPPED = F77_NAMED_COMMON(lutbcom).lut_flipped;


      for ( k=0; k<=2; k++ ) {
         for ( j=0; j<=LVX-1; j++ ) {
            LUTC_VAL[j][k] = F77_NAMED_COMMON(lutacom).lut_val[k][j];
         }
      }

}


/***************************************************************************
 DSXC_LUTVPUT -- Copy 'LUT.H' C variables over into Fortran version

   alan penny                  ral                        1993 Aug
*/

dsxc_lutvput (void)

/* C-- */
{
      int   j, k;
/* Cbegin */


      F77_NAMED_COMMON(lutacom).lut_stored  = LUTC_STORED;
      F77_NAMED_COMMON(lutacom).lut_ends    = LUTC_ENDS;
      F77_NAMED_COMMON(lutacom).lut_num     = LUTC_NUM;
      F77_NAMED_COMMON(lutacom).lut_sc      = LUTC_SC;
      F77_NAMED_COMMON(lutacom).lut_ze      = LUTC_ZE;
      F77_NAMED_COMMON(lutbcom).lut_flipped = LUTC_FLIPPED;


      for ( k=0; k<=2; k++ ) {
         for ( j=0; j<=LVX-1; j++ ) {
            F77_NAMED_COMMON(lutacom).lut_val[k][j] = LUTC_VAL[j][k];
         }
      }

}

