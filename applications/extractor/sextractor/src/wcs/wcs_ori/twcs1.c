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
*   twcs1 tests wcsfwd() and wcsrev() for closure on the 1 degree celestial
*   grid for a number of selected projections.
*
*   $Id: twcs1.c,v 2.2 1996/09/10 06:35:34 mcalabre Exp $
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include "wcs.h"

#ifndef __STDC__
#ifndef const
#define const
#endif
#endif


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
   void wcsex();
   char   text[80];
   register int j;
   const double tol = 1.0e-10;
   double latc;
   struct celprm cel0, celc, celp;
   struct prjprm prj;
   struct linprm lin;

   /* Uncomment the following two lines to raise SIGFPE on floating point
    * exceptions for the Sun FORTRAN compiler.  This signal can be caught
    * within 'dbx' by issuing the command "catch FPE".
    */
/* #include <floatingpoint.h> */
/* call ieee_handler ("set", "common", SIGFPE_ABORT); */


   printf("\nTesting closure of WCSLIB world coordinate transformation routines\n");
   printf("------------------------------------------------------------------\n");

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

   /* Note that we have 3 contexts (CELP, CELC, and CEL0).  wcsex() will force
      these to be initialized on every invokation since PCODE will differ each
      time. */

   /* ARC: zenithal/azimuthal equidistant. */
   strncpy(&ctype[1][5], "ARC", 3);
   strncpy(&ctype[3][5], "ARC", 3);
   wcsex(tol, ctype, crval, &celp, &prj, &lin);

   /* ZEA: zenithal/azimuthal equal area. */
   strncpy(&ctype[1][5], "ZEA", 3);
   strncpy(&ctype[3][5], "ZEA", 3);
   wcsex(tol, ctype, crval, &celp, &prj, &lin);

   /* CYP: cylindrical perspective. */
   strncpy(&ctype[1][5], "CYP", 3);
   strncpy(&ctype[3][5], "CYP", 3);
   prj.p[1] = 3.0;
   prj.p[2] = 0.8;
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   /* CAR: Cartesian. */
   strncpy(&ctype[1][5], "CAR", 3);
   strncpy(&ctype[3][5], "CAR", 3);
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   /* CEA: cylindrical equal area. */
   strncpy(&ctype[1][5], "CEA", 3);
   strncpy(&ctype[3][5], "CEA", 3);
   prj.p[1] = 0.75;
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   /* COD: conic equidistant. */
   strncpy(&ctype[1][5], "COD", 3);
   strncpy(&ctype[3][5], "COD", 3);
   prj.p[1] = latc;
   prj.p[2] = 15.0;
   wcsex(tol, ctype, crval, &celc, &prj, &lin);

   /* COE: conic equal area. */
   strncpy(&ctype[1][5], "COE", 3);
   strncpy(&ctype[3][5], "COE", 3);
   prj.p[1] = latc;
   prj.p[2] = 15.0;
   wcsex(tol, ctype, crval, &celc, &prj, &lin);

   /* BON: Bonne's projection. */
   strncpy(&ctype[1][5], "BON", 3);
   strncpy(&ctype[3][5], "BON", 3);
   prj.p[1] = 30.0;
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   /* PCO: polyconic. */
   strncpy(&ctype[1][5], "PCO", 3);
   strncpy(&ctype[3][5], "PCO", 3);
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   /* GLS: Sanson-Flamsteed (global sinusoid). */
   strncpy(&ctype[1][5], "GLS", 3);
   strncpy(&ctype[3][5], "GLS", 3);
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   /* PAR: parabolic. */
   strncpy(&ctype[1][5], "PAR", 3);
   strncpy(&ctype[3][5], "PAR", 3);
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   /* AIT: Hammer-Aitoff. */
   strncpy(&ctype[1][5], "AIT", 3);
   strncpy(&ctype[3][5], "AIT", 3);
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   /* MOL: Mollweide's projection. */
   strncpy(&ctype[1][5], "MOL", 3);
   strncpy(&ctype[3][5], "MOL", 3);
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   /* QSC: quadrilateralized spherical cube. */
   strncpy(&ctype[1][5], "QSC", 3);
   strncpy(&ctype[3][5], "QSC", 3);
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   /* TSC: tangential spherical cube. */
   strcpy(&ctype[0][0], "CUBEFACE");
   strncpy(&ctype[1][5], "TSC", 3);
   strncpy(&ctype[3][5], "TSC", 3);
   wcsex(tol, ctype, crval, &cel0, &prj, &lin);

   return 0;
}


/*----------------------------------------------------------------------------
*   wcsex() tests closure of wcsfwd() and wcsrev().
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
 
void wcsex(tol, ctype, crval, cel, prj, lin)
 
double tol;
char   ctype[][9];
double crval[];
struct celprm *cel;
struct prjprm *prj;
struct linprm *lin;
 
{
   int err, lat, lng;
   double dlat, dlatmx, dlng, dlngmx, lat1, lng1, phi, theta;
   double img[4], pix[4], world[4];
   struct wcsprm wcs;
 
   wcs.flag = 0;
   cel->flag = 0;
   prj->flag = 0;
   prj->r0 = 0.0;

   dlngmx = 0.0;
   dlatmx = 0.0;

   /* Find the projection code. */
   (void) wcsset(lin->naxis, ctype, &wcs);

   printf("\nTesting %s; closure tolerance %8.1g deg.\n", wcs.pcode, tol);

   for (lat = 90; lat >= -90; lat--) {
      lat1 = (double)lat;
 
      for (lng = -180; lng <= 180; lng++) {
         lng1 = (double)lng;
 
         world[0] = 0.0;
         world[1] = 0.0;
         world[2] = 0.0;
         world[3] = 0.0;
         world[wcs.lng] = lng1;
         world[wcs.lat] = lat1;
 
         if (err = wcsfwd(ctype, &wcs, world, crval, cel, &phi, &theta, prj,
                          img, lin, pix)) {
            printf("        %3s: lng1 =%20.15lf  lat1 =%20.15lf  error %3d\n",
               wcs.pcode, lng1, lat1, err);
            continue;
         }
 
         if (err = wcsrev(ctype, &wcs, pix, lin, img, prj, &phi, &theta, crval,
                          cel, world)) {
            printf("        %3s: lng1 =%20.15lf  lat1 =%20.15lf  error%3d\n",
               wcs.pcode, lng1, lat1, err);
            continue;
         }
 
         dlng = fabs(world[wcs.lng]-lng1);
         if (dlng > 180.0) dlng = fabs(dlng-360.0);
         if (abs(lat) != 90 && dlng > dlngmx) dlngmx = dlng;
         dlat = fabs(world[wcs.lat]-lat1);
         if (dlat > dlatmx) dlatmx = dlat;
 
         if (dlat > tol) {
            printf("        %3s: lng1 =%20.15lf  lat1 =%20.15lf\n",
               wcs.pcode, lng1, lat1);
            printf("             lng2 =%20.15lf  lat2 =%20.15lf\n",
               world[wcs.lng], world[wcs.lat]);
         } else if (abs(lat) != 90) {
            if (dlng > tol) {
               printf("        %3s: lng1 =%20.15lf  lat1 =%20.15lf\n",
                  wcs.pcode, lng1, lat1);
               printf("             lng2 =%20.15lf  lat2 =%20.15lf\n",
                  world[wcs.lng], world[wcs.lat]);
            }
         }
      }
   }
 
   printf("     Maximum closure residual: lng%10.3le   lat%10.3le\n",
      dlngmx, dlatmx);

}
