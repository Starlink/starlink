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
*   tproj2 tests projection routines by plotting test grids using PGPLOT.
*
*   $Id: tproj2.c,v 2.2 1996/05/07 20:30:14 mcalabre Exp $
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include "proj.h"

main()

{
   void prjplt();
   int    j;
   char text[80], text1[80], text2[80];
   struct prjprm prj;
 
 
   printf("\nTesting WCSLIB spherical projection routines\n");
   printf("--------------------------------------------\n");

   /* List error messages. */
   printf("\nList of prjset error codes:\n");
   printf("   %d: %s.\n", 1, prjset_errmsg[1]);
 
   printf("\nList of prjfwd error codes:\n");
   for (j = 1; j <=2 ; j++) {
      printf("   %d: %s.\n", j, prjfwd_errmsg[j]);
   }
 
   printf("\nList of prjrev error codes:\n");
   for (j = 1; j <=2 ; j++) {
      printf("   %d: %s.\n", j, prjrev_errmsg[j]);
   }
 
   printf("\n");


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

   strcpy(text1, "\n%s projection\n");
   strcpy(text2, "\n%s projection\nParameters:");

   for (j = 0; j < 10; prj.p[j++] = 0.0);

   /* AZP: zenithal/azimuthal perspective. */
   prj.p[1] = 2.0;
   printf(text2, "Zenithal/azimuthal perspective");
   printf("%12.5lf\n", prj.p[1]);
   prjplt("AZP", azpfwd, &prj, 90, -30, 1);

   /* TAN: gnomonic. */
   printf(text1, "Gnomonic");
   prjplt("TAN", tanfwd, &prj, 90, 5, 1);

   /* SIN: orthographic. */
   prj.p[1] = 0.3;
   prj.p[2] = 1.5;
   printf(text2, "Orthographic/synthesis");
   printf("%12.5lf%12.5lf\n", prj.p[1], prj.p[2]);
   prjplt("SIN", sinfwd, &prj, 90, 60, 1);

   /* STG: stereographic. */
   printf(text1, "Stereographic");
   prjplt("STG", stgfwd, &prj, 90, -85, 1);

   /* ARC: zenithal/azimuthal equidistant. */
   printf(text1, "Zenithal/azimuthal equidistant");
   prjplt("ARC", arcfwd, &prj, 90, -90, 1);

   /* ZPN: zenithal/azimuthal polynomial. */
   prj.p[0] =  0.05000;
   prj.p[1] =  0.95000;
   prj.p[2] = -0.02500;
   prj.p[3] = -0.15833;
   prj.p[4] =  0.00208;
   prj.p[5] =  0.00792;
   prj.p[6] = -0.00007;
   prj.p[7] = -0.00019;
   prj.p[8] =  0.00000;
   prj.p[9] =  0.00000;
   printf(text2, "Zenithal/azimuthal polynomial");
   printf("%12.5lf%12.5lf%12.5lf%12.5lf%12.5lf\n",
      prj.p[0], prj.p[1], prj.p[2], prj.p[3], prj.p[4]);
   printf("           %12.5lf%12.5lf%12.5lf%12.5lf%12.5lf\n",
      prj.p[5], prj.p[6], prj.p[7], prj.p[8], prj.p[9]);
   prjplt("ZPN", zpnfwd, &prj, 90, 10, 1);

   /* ZEA: zenithal/azimuthal equal area. */
   printf(text1, "Zenithal/azimuthal equal area");
   prjplt("ZEA", zeafwd, &prj, 90, -90, 1);

   /* AIR: Airy's zenithal projection. */
   prj.p[1] = 45.0;
   printf(text2, "Airy's zenithal");
   printf("%12.5lf\n", prj.p[1]);
   prjplt("AIR", airfwd, &prj, 90, -85, 1);

   /* CYP: cylindrical perspective. */
   prj.p[1] = 3.0;
   prj.p[2] = 0.8;
   printf(text2, "Cylindrical perspective");
   printf("%12.5lf%12.5lf\n", prj.p[1], prj.p[2]);
   prjplt("CYP", cypfwd, &prj, 90, -90, 2);

   /* CAR: Cartesian. */
   printf(text1, "Cartesian");
   prjplt("CAR", carfwd, &prj, 90, -90, 2);

   /* MER: Mercator's. */
   printf(text1, "Mercator's");
   prjplt("MER", merfwd, &prj, 85, -85, 2);

   /* CEA: cylindrical equal area. */
   prj.p[1] = 0.75;
   printf(text2, "Cylindrical equal area");
   printf("%12.5lf\n", prj.p[1]);
   prjplt("CEA", ceafwd, &prj, 90, -90, 2);

   /* COP: conic perspective. */
   prj.p[1] =  60.0;
   prj.p[2] =  15.0;
   printf(text2, "Conic perspective");
   printf("%12.5lf%12.5lf\n", prj.p[1], prj.p[2]);
   prjplt("COP", copfwd, &prj, 90, -25, 3);

   /* COD: conic equidistant. */
   prj.p[1] = -60.0;
   prj.p[2] =  15.0;
   printf(text2, "Conic equidistant");
   printf("%12.5lf%12.5lf\n", prj.p[1], prj.p[2]);
   prjplt("COD", codfwd, &prj, 90, -90, 3);

   /* COE: conic equal area. */
   prj.p[1] =  60.0;
   prj.p[2] = -15.0;
   printf(text2, "Conic equal area");
   printf("%12.5lf%12.5lf\n", prj.p[1], prj.p[2]);
   prjplt("COE", coefwd, &prj, 90, -90, 3);

   /* COO: conic orthomorphic. */
   prj.p[1] = -60.0;
   prj.p[2] = -15.0;
   printf(text2, "Conic orthomorphic");
   printf("%12.5lf%12.5lf\n", prj.p[1], prj.p[2]);
   prjplt("COO", coofwd, &prj, 85, -90, 3);

   /* BON: Bonne's projection. */
   prj.p[1] = 30.0;
   printf(text2, "Bonne's");
   printf("%12.5lf\n", prj.p[1]);
   prjplt("BON", bonfwd, &prj, 90, -90, 4);

   /* PCO: polyconic. */
   printf(text1, "Polyconic");
   prjplt("PCO", pcofwd, &prj, 90, -90, 4);

   /* GLS: Sanson-Flamsteed (global sinusoid). */
   printf(text1, "Sanson-Flamsteed (global sinusoid)");
   prjplt("GLS", glsfwd, &prj, 90, -90, 4);

   /* PAR: parabolic. */
   printf(text1, "Parabolic");
   prjplt("PAR", parfwd, &prj, 90, -90, 4);

   /* AIT: Hammer-Aitoff. */
   printf(text1, "Hammer-Aitoff");
   prjplt("AIT", aitfwd, &prj, 90, -90, 4);

   /* MOL: Mollweide's projection. */
   printf(text1, "Mollweide's");
   prjplt("MOL", molfwd, &prj, 90, -90, 4);

   /* CSC: COBE quadrilateralized spherical cube. */
   printf(text1, "COBE quadrilateralized spherical cube");
   prjplt("CSC", cscfwd, &prj, 90, -90, 5);

   /* QSC: quadrilateralized spherical cube. */
   printf(text1, "Quadrilateralized spherical cube");
   prjplt("QSC", qscfwd, &prj, 90, -90, 5);

   /* TSC: tangential spherical cube. */
   printf(text1, "Tangential spherical cube");
   prjplt("TSC", tscfwd, &prj, 90, -90, 5);

   pgend();

   return 0;
}


/*----------------------------------------------------------------------------
*   PRJPLT draws a 15 degree coordinate grid.
*
*   Given:
*      pcode[4]  char     Projection mnemonic.
*      *prjfwd() int      Forward projection routine.
*      north     int      Northern cutoff latitude, degrees.
*      south     int      Southern cutoff latitude, degrees.
*      type      int      Projection classification:
*                           1: zenithal/azimuthal
*                           2: cylindrical
*                           3: conic
*                           4: conventional
*                           5: quad cube
*
*   Given and returned:
*      prj       prjprm*  Projection parameters.
*---------------------------------------------------------------------------*/

void prjplt(pcode, prjfwd, prj, north, south, type)

char   pcode[4];
int    north, south, type;
int    (*prjfwd)();
struct prjprm *prj;

{
   char   text[80];
   int    ci, ilat, ilng;
   register int j;
   float  xr[512], yr[512];
   double lat, lng, x, y;

   printf("Plotting %s; latitudes%3d to%4d.\n", pcode, north, south);

   prj->flag = 0;
   prj->r0 = 0.0;

   if (type == 5) {
      pgenv(-335.0, 65.0, -200.0, 200.0, 1, -2);
      pgsci(2);
      sprintf(text,"%s - 15 degree graticule", pcode);
      pgtext(-340.0, -220.0, text);

      pgsci(8);
      (void) prjfwd(0.0, 0.0, prj, &x, &y);

      xr[0] =  prj->w[0];
      yr[0] =  prj->w[0];
      xr[1] =  prj->w[0];
      yr[1] =  prj->w[0]*3.0;
      xr[2] = -prj->w[0];
      yr[2] =  prj->w[0]*3.0;
      xr[3] = -prj->w[0];
      yr[3] = -prj->w[0]*3.0;
      xr[4] =  prj->w[0];
      yr[4] = -prj->w[0]*3.0;
      xr[5] =  prj->w[0];
      yr[5] =  prj->w[0];
      xr[6] = -prj->w[0]*7.0;
      yr[6] =  prj->w[0];
      xr[7] = -prj->w[0]*7.0;
      yr[7] = -prj->w[0];
      xr[8] =  prj->w[0];
      yr[8] = -prj->w[0];
      pgline(9, xr, yr);
   } else {
      pgenv(-200.0, 200.0, -200.0, 200.0, 1, -2);
      pgsci(2);
      sprintf(text,"%s - 15 degree graticule", pcode);
      pgtext(-240.0, -220.0, text);
   }


   ci = 1;
   for (ilng = -180; ilng <= 180; ilng+=15) {
      if (++ci > 7) ci = 2;

      lng = (double)ilng;

      pgsci(ilng?ci:1);

      j = 0;
      for (ilat = north; ilat >= south; ilat--) {
         lat = (double)ilat;

         if (prjfwd(lng, lat, prj, &x, &y)) continue;

         if (type == 5 && j > 0) {
            if (fabs(x+xr[j-1]) > 2.0 || fabs(y-yr[j-1]) > 5.0) {
               if (j > 1) pgline(j, xr, yr);
               j = 0;
            }
         }

         xr[j] = -x;
         yr[j] =  y;
         j++;
      }

      pgline(j, xr, yr);
   }

   ci = 1;
   for (ilat = -90; ilat <= 90; ilat += 15) {
      if (++ci > 7) ci = 2;

      if (ilat > north) continue;
      if (ilat < south) continue;

      lat = (double)ilat;

      pgsci(ilat?ci:1);

      j = 0;
      for (ilng = -180; ilng <= 180; ilng++) {
         lng = (double)ilng;

         if (prjfwd(lng, lat, prj, &x, &y)) continue;

         if (type == 5 && j > 0) {
            if (fabs(x+xr[j-1]) > 2.0 || fabs(y-yr[j-1]) > 5.0) {
               if (j > 1) pgline(j, xr, yr);
               j = 0;
            }
         }

         xr[j] = -x;
         yr[j] =  y;
         j++;
      }

      pgline(j, xr, yr);
   }

   pgsci(1);
   xr[0] = 0.0f;
   yr[0] = 0.0f;
   pgpt(1, xr, yr, 21);


   for (j = 0; j < 10; prj->p[j++] = 0.0);

   return;
}
