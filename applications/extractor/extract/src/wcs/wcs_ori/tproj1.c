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
*   tproj1 tests forward and reverse spherical projections for closure.
*
*   $Id: tproj1.c,v 2.2 1996/05/07 20:29:35 mcalabre Exp $
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include "proj.h"

#ifndef __STDC__
#ifndef const
#define const
#endif
#endif


main()

{
   void projex();
   register int j;
   const double tol = 1.0e-10;
   struct prjprm prj;

   /* Uncomment the following two lines to raise SIGFPE on floating point
    * exceptions for the Sun FORTRAN compiler.  This signal can be caught
    * within 'dbx' by issuing the command "catch FPE".
    */
/* #include <floatingpoint.h> */
/* call ieee_handler ("set", "common", SIGFPE_ABORT); */

 
   printf("\nTesting closure of WCSLIB spherical projection routines\n");
   printf("-------------------------------------------------------\n");

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

   printf("\n\n");


   for (j = 0; j < 10; prj.p[j++] = 0.0);

   /* AZP: zenithal/azimuthal perspective. */
   prj.p[1] = 2.0;
   projex("AZP", azpfwd, azprev, &prj, 90, -30, tol);

   /* TAN: gnomonic. */
   projex("TAN", tanfwd, tanrev, &prj, 90, 5, tol);

   /* SIN: orthographic/synthesis. */
   prj.p[1] = 0.3;
   prj.p[2] = 1.5;
   projex("SIN", sinfwd, sinrev, &prj, 90, 60, tol);

   /* STG: stereographic. */
   projex("STG", stgfwd, stgrev, &prj, 90, -85, tol);

   /* ARC: zenithal/azimuthal equidistant. */
   projex("ARC", arcfwd, arcrev, &prj, 90, -90, tol);

   /* ZPN: zenithal/azimuthal polynomial. */
   prj.p[0] =  0.00000;
   prj.p[1] =  0.95000;
   prj.p[2] = -0.02500;
   prj.p[3] = -0.15833;
   prj.p[4] =  0.00208;
   prj.p[5] =  0.00792;
   prj.p[6] = -0.00007;
   prj.p[7] = -0.00019;
   prj.p[8] =  0.00000;
   prj.p[9] =  0.00000;
   projex("ZPN", zpnfwd, zpnrev, &prj, 90, 10, tol);

   /* ZEA: zenithal/azimuthal equal area. */
   projex("ZEA", zeafwd, zearev, &prj, 90, -90, tol);

   /* AIR: Airy's zenithal projection. */
   prj.p[1] = 45.0;
   projex("AIR", airfwd, airrev, &prj, 90, -85, tol);

   /* CYP: cylindrical perspective. */
   prj.p[1] = 3.0;
   prj.p[2] = 0.8;
   projex("CYP", cypfwd, cyprev, &prj, 90, -90, tol);

   /* CAR: Cartesian. */
   projex("CAR", carfwd, carrev, &prj, 90, -90, tol);

   /* MER: Mercator's. */
   projex("MER", merfwd, merrev, &prj, 85, -85, tol);

   /* CEA: cylindrical equal area. */
   prj.p[1] = 0.75;
   projex("CEA", ceafwd, cearev, &prj, 90, -90, tol);

   /* COP: conic perspective. */
   prj.p[1] =  60.0;
   prj.p[2] =  15.0;
   projex("COP", copfwd, coprev, &prj, 90, -25, tol);

   /* COD: conic equidistant. */
   prj.p[1] = -60.0;
   prj.p[2] =  15.0;
   projex("COD", codfwd, codrev, &prj, 90, -90, tol);

   /* COE: conic equal area. */
   prj.p[1] =  60.0;
   prj.p[2] = -15.0;
   projex("COE", coefwd, coerev, &prj, 90, -90, tol);

   /* COO: conic orthomorphic. */
   prj.p[1] = -60.0;
   prj.p[2] = -15.0;
   projex("COO", coofwd, coorev, &prj, 85, -90, tol);

   /* BON: Bonne's projection. */
   prj.p[1] = 30.0;
   projex("BON", bonfwd, bonrev, &prj, 90, -90, tol);

   /* PCO: polyconic. */
   projex("PCO", pcofwd, pcorev, &prj, 90, -90, tol);

   /* GLS: Sanson-Flamsteed (global sinusoid). */
   projex("GLS", glsfwd, glsrev, &prj, 90, -90, tol);

   /* PAR: parabolic. */
   projex("PAR", parfwd, parrev, &prj, 90, -90, tol);

   /* AIT: Hammer-Aitoff. */
   projex("AIT", aitfwd, aitrev, &prj, 90, -90, tol);

   /* MOL: Mollweide's projection. */
   projex("MOL", molfwd, molrev, &prj, 90, -90, tol);

   /* CSC: COBE quadrilateralized spherical cube. */
   projex("CSC", cscfwd, cscrev, &prj, 90, -90, 4.0e-2);

   /* QSC: quadrilateralized spherical cube. */
   projex("QSC", qscfwd, qscrev, &prj, 90, -90, tol);

   /* TSC: tangential spherical cube. */
   projex("TSC", tscfwd, tscrev, &prj, 90, -90, tol);

   return 0;
}


/*----------------------------------------------------------------------------
*   PROJEX exercises the spherical projection routines.
*
*   Given:
*      pcode[4]  char     Projection code.
*      *prjfwd() int      Forward projection routine.
*      *prjrev() int      Reverse projection routine.
*      north     int      Northern cutoff latitude, degrees.
*      south     int      Southern cutoff latitude, degrees.
*
*   Given and returned:
*      prj       prjprm*  Projection parameters.
*      tol       double   Closure tolerance, degrees.
*---------------------------------------------------------------------------*/

void projex(pcode, prjfwd, prjrev, prj, north, south, tol)

char   pcode[4];
int    (*prjfwd)(), (*prjrev)(), north, south;
double tol;
struct prjprm *prj;

{
   int    err, lat, lng;
   register int j;
   double dlat, dlatmx, dlng, dlngmx, dr, drmax, lat1, lat2, lng1, lng2;
   double r, theta, x, x1, x2, y, y1, y2;

   printf("Testing %s; latitudes %3d to %4d, closure tolerance %8.1g deg.\n",
      pcode, north, south, tol);

   prj->flag = 0;
   prj->r0 = 0.0;

   dlngmx = 0.0;
   dlatmx = 0.0;

   /* Uncomment the next line to test alternative initializations of */
   /* projection parameters.                                         */
   /* prj->r0 = R2D; */

   for (lat = north; lat >= south; lat--) {
      lat1 = (double)lat;

      for (lng = -180; lng <= 180; lng++) {
         lng1 = (double)lng;

         if (err = prjfwd(lng1, lat1, prj, &x, &y)) {
            printf("        %3s: lng1 =%20.15lf  lat1 =%20.15lf  error %3d\n",
               pcode, lng1, lat1, err);
            continue;
         }

         if (err = prjrev(x, y, prj, &lng2, &lat2)) {
            printf("        %3s: lng1 =%20.15lf  lat1 =%20.15lf\n",
               pcode, lng1, lat1);
            printf("                x =%20.15lf     y =%20.15lf  error%3d\n",
               x, y, err);
            continue;
         }

         dlng = fabs(lng2-lng1);
         if (dlng > 180.0) dlng = fabs(dlng-360.0);
         if (abs(lat) != 90 && dlng > dlngmx) dlngmx = dlng;
         dlat = fabs(lat2-lat1);
         if (dlat > dlatmx) dlatmx = dlat;

         if (dlat > tol) {
            printf("        %3s: lng1 =%20.15lf  lat1 =%20.15lf\n",
               pcode, lng1, lat1);
            printf("                x =%20.15lf     y =%20.15lf\n", x, y);
            printf("             lng2 =%20.15lf  lat2 =%20.15lf\n",
               lng2, lat2);
         } else if (abs(lat) != 90) {
            if (dlng > tol) {
               printf("        %3s: lng1 =%20.15lf  lat1 =%20.15lf\n",
                  pcode, lng1, lat1);
               printf("                x =%20.15lf     y =%20.15lf\n", x, y);
               printf("             lng2 =%20.15lf  lat2 =%20.15lf\n",
                  lng2, lat2);
            }
         }
      }
   }

   printf("             Maximum residual (sky): lng%10.3le   lat%10.3le\n",
      dlngmx, dlatmx);


   /* Test closure at a point close to the reference point. */
   r = 1.0;
   theta = -180.0;

   drmax = 0.0;

   for (j = 1; j <= 12; j++) {
      x1 = r*cosd(theta);
      y1 = r*sind(theta);

      if (err = prjrev(x1, y1, prj, &lng1, &lat1)) {
         printf("        %3s:   x1 =%20.15lf    y1 =%20.15lf  error%3d\n",
            pcode, x1, y1, err);

      } else if (err = prjfwd(lng1, lat1, prj, &x2, &y2)) {
         printf("        %3s:   x1 =%20.15lf    y1 =%20.15lf\n",
            pcode, x1, y1);
         printf("              lng =%20.15lf   lat =%20.15lf  error%3d\n",
            lng1, lat1, err);

      } else {
         dr = sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1));
         if (dr > drmax) drmax = dr;
         if (dr > tol) {
            printf("        %3s:   x1 =%20.15lf    y1 =%20.15lf\n",
               pcode, x1, y1);
            printf("              lng =%20.15lf   lat =%20.15lf\n",
               lng1, lat1);
            printf("               x2 =%20.15lf    y2 =%20.15lf\n",
               x2, y2);
         }
      }

      r /= 10.0;
      theta += 15.0;
   }

   printf("             Maximum residual (map):  dR%10.3le\n", drmax);

   for (j = 0; j < 10; prj->p[j++] = 0.0);

   return;
}
