/*============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995,1996 Mark Calabretta
*
*   This program is free software; you can redistribute it and/or modify it
*   under the terms of the GNU General Public License as published by the
*   Free Software Foundation; either version 2 of the License, or (at your
*   option) any later version.
*
*   This program is distributed in the hope that it will be useful, but
*   WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*   General Public License for more details.
*
*   You should have received a copy of the GNU General Public License along
*   with this library; if not, write to the Free Software Foundation, Inc.,
*   675 Mass Ave, Cambridge, MA 02139, USA.
*
*   Correspondence concerning WCSLIB may be directed to:
*      Internet email: mcalabre@atnf.csiro.au
*      Postal address: Dr. Mark Calabretta,
*                      Australia Telescope National Facility,
*                      P.O. Box 76,
*                      Epping, NSW, 2121,
*                      AUSTRALIA
*
*=============================================================================
*
*   twcs2 tests wcsmix() for closure on the 1 degree celestial grid for a
*   number of selected projections.  Points with good solutions are marked
*   with a white dot on a graphical display of the projection while bad
*   solutions are flagged with a red circle.
*
*   $Id: twcs2.c,v 2.4 1996/05/07 20:32:29 mcalabre Exp $
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include "wcs.h"

#ifndef __STDC__
#ifndef const
#define const
#endif
#endif

/* Set to 1 to skip mixex(), primarily for debugging purposes. */
int skip_mixex = 0;

char ctype[4][9] = {"FREQ    ", "XLAT-xxx", "TIME    ", "XLON-xxx"};
double crpix[4] =  { 0.0,  0.0,  0.0,  0.0};
double pc[4][4] = {{ 1.1,  0.0,  0.0,  0.0},
                   { 0.0,  1.0,  0.0,  0.1},
                   { 0.0,  0.0,  1.0,  0.0},
                   { 0.0,  0.2,  0.0,  1.0}};
double cdelt[4] =  { 1.0,  1.0,  1.0, -1.0};
double crval[4] =  {408e6, 0.0, -2e3,  0.0};

main()

{
   void grdplt(), mixex();
   char   text[80];
   register int j;
   const double tol = 1.0e-10;
   double latc;
   struct celprm cel0, celc, celp, ntv0, ntvc, ntvp;
   struct prjprm prj;
   struct linprm lin;

   /* Uncomment the following two lines to raise SIGFPE on floating point
    * exceptions for the Sun FORTRAN compiler.  This signal can be caught
    * within 'dbx' by issuing the command "catch FPE".
    */
/* #include <floatingpoint.h> */
/* call ieee_handler ("set", "common", SIGFPE_ABORT); */


   printf("\nTesting WCSLIB wcsmix routine\n");
   printf("-----------------------------\n");

   /* List error messages. */
   printf("\nList of wcsset error codes:\n");
   printf("   %d: %s.\n", 1, wcsset_errmsg[1]);
 
   printf("\nList of wcsfwd error codes:\n");
   for (j = 1; j <=4 ; j++) {
      printf("   %d: %s.\n", j, wcsfwd_errmsg[j]);
   }
 
   printf("\nList of wcsrev error codes:\n");
   for (j = 1; j <=4 ; j++) {
      printf("   %d: %s.\n", j, wcsrev_errmsg[j]);
   }
 
   printf("\nList of wcsmix error codes:\n");
   for (j = 1; j <=5 ; j++) {
      printf("   %d: %s.\n", j, wcsmix_errmsg[j]);
   }


   /* PGPLOT initialization. */
   strcpy(text, "/xwindow");
   pgbeg(0, text, 1, 1);

   /* Define pen colours. */
   pgscr(0, 0.00, 0.00, 0.00);
   pgscr(1, 1.00, 1.00, 0.00);
   pgscr(2, 1.00, 1.00, 1.00);
   pgscr(3, 0.50, 0.50, 0.80);
   pgscr(4, 0.80, 0.50, 0.50);
   pgscr(5, 0.80, 0.80, 0.80);
   pgscr(6, 0.50, 0.50, 0.80);
   pgscr(7, 0.80, 0.50, 0.50);
   pgscr(8, 0.30, 0.50, 0.30);
   pgscr(9, 1.00, 0.75, 0.00);


   lin.flag  = 0;
   lin.naxis = 4;
   lin.crpix = crpix;
   lin.pc    = (double*)pc;
   lin.cdelt = cdelt;

   for (j = 0; j < 10; prj.p[j++] = 0.0);
   for (j = 0; j < 10; prj.w[j++] = 0.0);
   prj.r0 = 0.0;

   /* Latitude midway between the standard parallels for the conics. */
   latc = 60.0;

   /* Set reference angles for native grids; polar projections... */
   ntvp.ref[0]  =   0.0;
   ntvp.ref[1]  =  90.0;
   ntvp.ref[2]  = 999.0;
   ntvp.ref[3]  = 999.0;

   /*  ...conic projections... */
   ntvc.ref[0]  =   0.0;
   ntvc.ref[1]  =  latc;
   ntvc.ref[2]  = 999.0;
   ntvc.ref[3]  = 999.0;

   /* ...cylindrical and conventional projections. */
   ntv0.ref[0]  =   0.0;
   ntv0.ref[1]  =   0.0;
   ntv0.ref[2]  = 999.0;
   ntv0.ref[3]  = 999.0;

   /* Set reference angles for the celestial grids; polar projections... */
   celp.ref[0] = 150.0;
   celp.ref[1] = -30.0;
   celp.ref[2] = 150.0;
   celp.ref[3] = 999.0;

   /* Force celp to be initialized since we want to use it now. */
   celp.flag = 0;
   (void) celset("ARC", &celp, &prj);

   /* Compute reference angles for the cylindrical and conic projections */
   /* so that they all use the same oblique celestial grid regardless of */
   /* the reference point; conic projections... */
   sphrev (0.0, latc, celp.euler, &celc.ref[0], &celc.ref[1]);
   sphfwd (0.0, 90.0, celp.euler, &celc.ref[2], &celc.ref[3]);

   /* ...cylindrical and conventional projections. */
   sphrev (0.0,  0.0, celp.euler, &cel0.ref[0], &cel0.ref[1]);
   sphfwd (0.0, 90.0, celp.euler, &cel0.ref[2], &cel0.ref[3]);

   /* Note that we have 6 contexts (NTVP, NTVC, NTV0, CELP, CELC, and CEL0). */
   /* Routines GRDPLT and MIXEX will each force these to be initialized on */
   /* every invokation since PCODE will differ each time. */

   printf("\n\nNote: Expect to see a few occurrences of wcsmix error 5 (no ");
   printf("solution) at\n");
   printf("points corresponding to the native poles for those projections ");
   printf("where the\n");
   printf("pole is represented as a finite interval.\n");

   /* ARC: zenithal/azimuthal equidistant. */
   strncpy(&ctype[1][5], "ARC", 3);
   strncpy(&ctype[3][5], "ARC", 3);
   grdplt(1, -190.0, 190.0, -190.0, 190.0, ctype, crval, &ntvp, &celp,
          &prj, &lin);
   mixex(tol, ctype, crval, &celp, &prj, &lin);

   /* ZEA: zenithal/azimuthal equal area. */
   strncpy(&ctype[1][5], "ZEA", 3);
   strncpy(&ctype[3][5], "ZEA", 3);
   grdplt(1, -120.0, 120.0, -120.0, 120.0, ctype, crval, &ntvp, &celp,
          &prj, &lin);
   mixex(tol, ctype, crval, &celp, &prj, &lin);

   /* CYP: cylindrical perspective. */
   strncpy(&ctype[1][5], "CYP", 3);
   strncpy(&ctype[3][5], "CYP", 3);
   prj.p[1] = 3.0;
   prj.p[2] = 0.8;
   grdplt(2, -170.0, 170.0, -170.0, 170.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   /* CAR: Cartesian. */
   strncpy(&ctype[1][5], "CAR", 3);
   strncpy(&ctype[3][5], "CAR", 3);
   grdplt(2, -210.0, 210.0, -210.0, 210.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   /* CEA: cylindrical equal area. */
   strncpy(&ctype[1][5], "CEA", 3);
   strncpy(&ctype[3][5], "CEA", 3);
   prj.p[1] = 0.75;
   grdplt(2, -200.0, 200.0, -200.0, 200.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   /* COD: conic equidistant. */
   strncpy(&ctype[1][5], "COD", 3);
   strncpy(&ctype[3][5], "COD", 3);
   prj.p[1] = latc;
   prj.p[2] = 15.0;
   grdplt(3, -200.0, 200.0, -190.0, 210.0, ctype, crval, &ntvc, &celc,
          &prj, &lin);
   mixex(tol, ctype, crval, &celc, &prj, &lin);

   /* COE: conic equal area. */
   strncpy(&ctype[1][5], "COE", 3);
   strncpy(&ctype[3][5], "COE", 3);
   prj.p[1] = latc;
   prj.p[2] = 15.0;
   grdplt(3, -140.0, 140.0, -130.0, 150.0, ctype, crval, &ntvc, &celc,
          &prj, &lin);
   mixex(tol, ctype, crval, &celc, &prj, &lin);

   /* BON: Bonne's projection. */
   strncpy(&ctype[1][5], "BON", 3);
   strncpy(&ctype[3][5], "BON", 3);
   prj.p[1] = 30.0;
   grdplt(4, -160.0, 160.0, -160.0, 160.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   /* PCO: polyconic. */
   strncpy(&ctype[1][5], "PCO", 3);
   strncpy(&ctype[3][5], "PCO", 3);
   grdplt(4, -190.0, 190.0, -190.0, 190.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   /* GLS: Sanson-Flamsteed (global sinusoid). */
   strncpy(&ctype[1][5], "GLS", 3);
   strncpy(&ctype[3][5], "GLS", 3);
   grdplt(4, -190.0, 190.0, -190.0, 190.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   /* PAR: parabolic. */
   strncpy(&ctype[1][5], "PAR", 3);
   strncpy(&ctype[3][5], "PAR", 3);
   grdplt(4, -190.0, 190.0, -190.0, 190.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   /* AIT: Hammer-Aitoff. */
   strncpy(&ctype[1][5], "AIT", 3);
   strncpy(&ctype[3][5], "AIT", 3);
   grdplt(4, -170.0, 170.0, -170.0, 170.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   /* MOL: Mollweide's projection. */
   strncpy(&ctype[1][5], "MOL", 3);
   strncpy(&ctype[3][5], "MOL", 3);
   grdplt(4, -170.0, 170.0, -170.0, 170.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   /* QSC: quadrilateralized spherical cube. */
   strncpy(&ctype[1][5], "QSC", 3);
   strncpy(&ctype[3][5], "QSC", 3);
   grdplt(5, -340.0, 90.0, -210.0, 210.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   /* TSC: tangential spherical cube. */
   strncpy(&ctype[1][5], "TSC", 3);
   strncpy(&ctype[3][5], "TSC", 3);
   grdplt(5, -340.0, 80.0, -210.0, 210.0, ctype, crval, &ntv0, &cel0,
          &prj, &lin);
   mixex(tol, ctype, crval, &cel0, &prj, &lin);

   pgend();

   return 0;
}


void grdplt(type, imin, imax, jmin, jmax, ctype, crval, native, celest, prj,
   lin)

int    type;
double imax, imin, jmax, jmin;
char   ctype[][9];
double crval[];
struct celprm *native, *celest;
struct prjprm *prj;
struct linprm *lin;

{
   char   text[80];
   int    ci, ilat, ilng, k;
   float  fimax, fimin, fjmax, fjmin, ir[512], jr[512];
   double lat, lng, phi, theta, img[4], pix[4], world[4];
   double step;
   struct wcsprm wcs;

   wcs.flag = 0;
   native->flag = 0;
   celest->flag = 0;
   prj->flag = 0;
   prj->r0 = 0.0;

   /* Define PGPLOT viewport. */
   fimax = (float)imax;
   fimin = (float)imin;
   fjmax = (float)jmax;
   fjmin = (float)jmin;
   pgenv(fimin, fimax, fjmin, fjmax, 1, -2);

   /* Issue a dummy call to initialize data structures. */
   world[0] = crval[0];
   world[1] = crval[1];
   world[2] = crval[2];
   world[3] = crval[3];
   (void) wcsfwd(ctype, &wcs, world, crval, celest, &phi, &theta, prj, img,
                 lin, pix);

   if (type == 5) {
      /* Some sort of quad-cube projection. */
      pgsci(8);

      /* Draw the map boundary. */
      img[0] = 0.0;
      img[1] = 0.0;
      img[2] = 0.0;
      img[3] = 0.0;

      img[wcs.lng] = -prj->w[0];
      img[wcs.lat] =  prj->w[0];
      linfwd(img, lin, pix);
      ir[0] = pix[wcs.lng];
      jr[0] = pix[wcs.lat];

      img[wcs.lng] = -prj->w[0];
      img[wcs.lat] =  prj->w[0]*3.0;
      linfwd(img, lin, pix);
      ir[1] = pix[wcs.lng];
      jr[1] = pix[wcs.lat];

      img[wcs.lng] =  prj->w[0];
      img[wcs.lat] =  prj->w[0]*3.0;
      linfwd(img, lin, pix);
      ir[2] = pix[wcs.lng];
      jr[2] = pix[wcs.lat];

      img[wcs.lng] =  prj->w[0];
      img[wcs.lat] = -prj->w[0]*3.0;
      linfwd(img, lin, pix);
      ir[3] = pix[wcs.lng];
      jr[3] = pix[wcs.lat];

      img[wcs.lng] = -prj->w[0];
      img[wcs.lat] = -prj->w[0]*3.0;
      linfwd(img, lin, pix);
      ir[4] = pix[wcs.lng];
      jr[4] = pix[wcs.lat];

      img[wcs.lng] = -prj->w[0];
      img[wcs.lat] =  prj->w[0];
      linfwd(img, lin, pix);
      ir[5] = pix[wcs.lng];
      jr[5] = pix[wcs.lat];

      img[wcs.lng] =  prj->w[0]*7.0;
      img[wcs.lat] =  prj->w[0];
      linfwd(img, lin, pix);
      ir[6] = pix[wcs.lng];
      jr[6] = pix[wcs.lat];

      img[wcs.lng] =  prj->w[0]*7.0;
      img[wcs.lat] = -prj->w[0];
      linfwd(img, lin, pix);
      ir[7] = pix[wcs.lng];
      jr[7] = pix[wcs.lat];

      img[wcs.lng] = -prj->w[0];
      img[wcs.lat] = -prj->w[0];
      linfwd(img, lin, pix);
      ir[8] = pix[wcs.lng];
      jr[8] = pix[wcs.lat];

      pgline(9, ir, jr);
   }

   /* Write a descriptive title. */
   pgsci(1);
   sprintf(text, "%s projection - 15 degree graticule", wcs.pcode);
   printf("\n%s\n", text);
   pgtext(imin, jmin-10.0, text);

   sprintf(text, "centered on celestial coordinates (%6.2lf,%6.2lf)",
      celest->ref[0], celest->ref[1]);
   printf("%s\n", text);
   pgtext(imin, jmin-20.0, text);

   sprintf(text, "with celestial pole at native coordinates (%7.2lf,%7.2lf)",
      celest->ref[2], celest->ref[3]);
   printf("%s\n", text);
   pgtext(imin, jmin-30.0, text);


   /* Draw the native coordinate grid faintly in the background. */
   pgsci(8);

   if (type == 4) {
      step = 10.0;
   } else {
      step = 15.0;
   }

   /* Draw native meridians of longitude. */
   for (ilng = -180; ilng <= 180; ilng += 15) {
      lng = (double)ilng;
      if (ilng == -180) lng = -179.99;
      if (ilng ==  180) lng =  179.99;

      k = 0;
      for (ilat = -90; ilat <= 90; ilat++) {
         lat = (double)ilat;

         world[wcs.lng] = lng;
         world[wcs.lat] = lat;
         if (wcsfwd(ctype, &wcs, world, crval, native, &phi, &theta, prj, img,
                    lin, pix)) {
            continue;
         }

         if (type == 5 && k > 0) {
            if (fabs(pix[wcs.lng]-ir[k-1]) > 2.0 ||
                fabs(pix[wcs.lat]-jr[k-1]) > 5.0) {
               if (k > 1) pgline(k, ir, jr);
               k = 0;
            }
         }

         ir[k] = pix[wcs.lng];
         jr[k] = pix[wcs.lat];
         k++;
      }

      pgline(k, ir, jr);
   }

   /* Draw native parallels of latitude. */
   for (ilat = -90; ilat <= 90; ilat += 15) {
      lat = (double)ilat;

      k = 0;
      for (ilng = -180; ilng <= 180; ilng++) {
         lng = (double)ilng;
         if (ilng == -180) lng = -179.99;
         if (ilng ==  180) lng =  179.99;

         world[wcs.lng] = lng;
         world[wcs.lat] = lat;
         if (wcsfwd(ctype, &wcs, world, crval, native, &phi, &theta, prj, img,
                    lin, pix)) {
            continue;
         }

         if (type == 5 && k > 0) {
            if (fabs(pix[wcs.lng]-ir[k-1]) > 2.0 ||
                fabs(pix[wcs.lat]-jr[k-1]) > 5.0) {
               if (k > 1) pgline(k, ir, jr);
               k = 0;
            }
         }

         ir[k] = pix[wcs.lng];
         jr[k] = pix[wcs.lat];
         k++;
      }

      pgline(k, ir, jr);
   }


   /* Draw a colour-coded celestial coordinate grid. */
   ci = 1;

   /* Draw celestial meridians of longitude. */
   for (ilng = -180; ilng <= 180; ilng += 15) {
      lng = (double)ilng;

      if (++ci > 7) ci = 2;
      pgsci(ilng?ci:1);

      k = 0;
      for (ilat = -90; ilat <= 90; ilat++) {
         lat = (double)ilat;

         world[wcs.lng] = lng;
         world[wcs.lat] = lat;
         if (wcsfwd(ctype, &wcs, world, crval, celest, &phi, &theta, prj, img,
                    lin, pix)) {
            continue;
         }

         /* Test for discontinuities. */
         if (k > 0) {
            if (fabs(pix[wcs.lng]-ir[k-1]) > step ||
                fabs(pix[wcs.lat]-jr[k-1]) > step) {
               if (k > 1) pgline(k, ir, jr);
               k = 0;
            }
         }

         ir[k] = pix[wcs.lng];
         jr[k] = pix[wcs.lat];
         k++;
      }

      pgline(k, ir, jr);
   }

   /* Draw celestial parallels of latitude. */
   ci = 1;
   for (ilat = -90; ilat <= 90; ilat += 15) {
      lat = (double)ilat;

      if (++ci > 7) ci = 2;
      pgsci(ilat?ci:1);

      k = 0;
      for (ilng = -180; ilng <= 180; ilng++) {
         lng = (double)ilng;

         world[wcs.lng] = lng;
         world[wcs.lat] = lat;
         if (wcsfwd(ctype, &wcs, world, crval, celest, &phi, &theta, prj, img,
                    lin, pix)) {
            continue;
         }

         /* Test for discontinuities. */
         if (k > 0) {
            if (fabs(pix[wcs.lng]-ir[k-1]) > step ||
                fabs(pix[wcs.lat]-jr[k-1]) > step) {
               if (k > 1) pgline(k, ir, jr);
               k = 0;
            }
         }

         ir[k] = pix[wcs.lng];
         jr[k] = pix[wcs.lat];
         k++;
      }

      pgline(k, ir, jr);
   }

   pgsci(2);

   return;
}


/*----------------------------------------------------------------------------
*   mixex() tests wcsmix().
*
*   Given:
*      tol      double   Closure tolerance, degrees.
*      ctype   char[][9] Coordinate axis types.
*      crval    double[] Coordinate reference values.
*
*   Given and returned:
*      cel      celprm*  Coordinate transformation parameters.
*      prj      prjprm*  Projection parameters.
*      lin      linprm*  Linear transformation parameters.
*---------------------------------------------------------------------------*/

void mixex(tol, ctype, crval, cel, prj, lin)

double tol;
char   ctype[][9];
double crval[];
struct celprm *cel;
struct prjprm *prj;
struct linprm *lin;

{
   void   id();
   int    err, doid, lat, lng;
   register int k;
   float  ipt[1], jpt[1];
   double lng1, lat1, phi, theta;
   double latspan[2], lngspan[2];
   double img[4], pix1[4], pix2[4], pix3[4], world[4];
   double pixlng, pixlat;
   struct wcsprm wcs;

   if (skip_mixex) return;

   wcs.flag = 0;
   cel->flag = 0;
   prj->flag = 0;
   prj->r0 = 0.0;

   /* Find the projection code. */
   (void) wcsset(lin->naxis, ctype, &wcs);

   printf("Testing %s; closure tolerance %8.1g deg.\n", wcs.pcode, tol);

   world[0] = 0.0;
   world[1] = 0.0;
   world[2] = 0.0;
   world[3] = 0.0;

   for (lat = 90; lat >= -90; lat--) {
      lat1 = (double)lat;

      for (lng = -180; lng <= 180; lng++) {
         lng1 = (double)lng;

         world[wcs.lng] = lng1;
         world[wcs.lat] = lat1;
         if (err = wcsfwd(ctype, &wcs, world, crval, cel, &phi, &theta, prj,
                          img, lin, pix1)) {
            printf("%3s: lng1 =%20.15lf  lat1 =%20.15lf  error %3d\n",
               wcs.pcode, lng1, lat1, err);
            continue;
         }

         pixlng = pix1[wcs.lng];
         pixlat = pix1[wcs.lat];

         ipt[0] = pixlng;
         jpt[0] = pixlat;
         pgpt(1, ipt, jpt, -1);

         lngspan[0] = lng1 - 9.3;
         if (lngspan[0] < -180.0) lngspan[0] = -180.0;
         lngspan[1] = lng1 + 4.1;
         if (lngspan[1] >  180.0) lngspan[1] =  180.0;
         latspan[0] = lat1 - 3.7;
         if (latspan[0] <  -90.0) latspan[0] =  -90.0;
         latspan[1] = lat1 + 7.2;
         if (latspan[1] >   90.0) latspan[1] =   90.0;

         doid = 1;

         pix2[wcs.lng] = pixlng;
         if (err = wcsmix(ctype, &wcs, wcs.lng, 1, latspan, 1.0, 2, world,
               crval, cel, &phi, &theta, prj, img, lin, pix2)) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  1: wcsmix error %d, %s.\n", err, wcsmix_errmsg[err]);
         } else if (err = wcsfwd(ctype, &wcs, world, crval, cel,
               &phi, &theta, prj, img, lin, pix3)) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  1: wcsfwd error %d, %s.\n", err, wcsfwd_errmsg[err]);
         } else if (fabs(pix3[wcs.lng]-pixlng) > tol) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  1: lat2 =%20.15lf    j2 =%20.15lf\n", world[wcs.lat],
               pix2[wcs.lat]);
         }


         pix2[wcs.lat] = pixlat;
         if (err = wcsmix(ctype, &wcs, wcs.lat, 1, latspan, 1.0, 2, world,
               crval, cel, &phi, &theta, prj, img, lin, pix2)) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  2: wcsmix error %d, %s.\n", err, wcsmix_errmsg[err]);
         } else if (err = wcsfwd(ctype, &wcs, world, crval, cel, &phi, &theta,
               prj, img, lin, pix3)) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  2: wcsfwd error %d, %s.\n", err, wcsfwd_errmsg[err]);
         } else if (fabs(pix3[wcs.lat]-pixlat) > tol) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  2: lat2 =%20.15lf    i2 =%20.15lf\n", world[wcs.lat],
               pix2[wcs.lng]);
         }

         world[wcs.lat] = lat1;

         pix2[wcs.lng] = pixlng;
         if (err = wcsmix(ctype, &wcs, wcs.lng, 2, lngspan, 1.0, 2, world,
               crval, cel, &phi, &theta, prj, img, lin, pix2)) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  3: wcsmix error %d, %s.\n", err, wcsmix_errmsg[err]);
         } else if (err = wcsfwd(ctype, &wcs, world, crval, cel, &phi, &theta,
               prj, img, lin, pix3)) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  3: wcsfwd error %d, %s.\n", err, wcsfwd_errmsg[err]);
         } else if (fabs(pix3[wcs.lng]-pixlng) > tol) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  3: lng2 =%20.15lf    j2 =%20.15lf\n", world[wcs.lng],
               pix2[wcs.lat]);
         }


         pix2[wcs.lat] = pixlat;
         if (err = wcsmix(ctype, &wcs, wcs.lat, 2, lngspan, 1.0, 2, world,
               crval, cel, &phi, &theta, prj, img, lin, pix2)) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  4: wcsmix error %d, %s.\n", err, wcsmix_errmsg[err]);
         } else if (err = wcsfwd(ctype, &wcs, world, crval, cel, &phi, &theta,
               prj, img, lin, pix3)) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  4: wcsfwd error %d, %s.\n", err, wcsfwd_errmsg[err]);
         } else if (fabs(pix3[wcs.lat]-pixlat) > tol) {
            id(&doid, wcs.pcode, cel, lng1, lat1, pixlng, pixlat);
            printf("  4: lng2 =%20.15lf    i2 =%20.15lf\n", world[wcs.lng],
               pix2[wcs.lng]);
         }

      }
   }

   for (k = 0; k < 10; prj->p[k++] = 0.0);

   return;
}


void id(doid, pcode, cel, lng1, lat1, pixlng, pixlat)

int    *doid;
char   pcode[4];
struct celprm *cel;
double lng1, lat1, pixlng, pixlat;

{
   float  ipt[1], jpt[1];
   double phi, theta;

   if (*doid) {
      /* Compute native coordinates. */
      sphfwd(lng1, lat1, cel->euler, &phi, &theta);

      printf("%3s: lng1 =%20.15lf  lat1 =%20.15lf\n", pcode, lng1, lat1);
      printf("      phi =%20.15lf theta =%20.15lf\n", phi, theta);
      printf("       i1 =%20.15lf    j1 =%20.15lf\n", pixlng, pixlat);
      *doid = 0;

      pgsci(9);
      ipt[0] = pixlng;
      jpt[0] = pixlat;
      pgpt(1, ipt, jpt, 21);
      pgsci(2);
   }

   return;
}
