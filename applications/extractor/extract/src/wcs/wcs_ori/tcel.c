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
*   tcel tests the spherical projection driver routines supplied with WCSLIB
*   by drawing native and celestial coordinate grids for Bonne's projection.
*
*   $Id: tcel.c,v 2.1 1996/05/07 20:29:03 mcalabre Exp $
*---------------------------------------------------------------------------*/

#include <stdio.h>
#include "cel.h"

main()

{
   char   pcode[4], text[80];
   int    ci, ilat, ilng, j;
   int    crval1, crval2, latpole, longpole;
   float  xr[512], yr[512];
   double lat, lng, phi, theta, x, y;
   struct celprm native, celestial;
   struct prjprm prj;


   printf("\nTesting WCSLIB celestial coordinate transformation routines\n");
   printf("-----------------------------------------------------------\n");
 
   /* List error messages. */
   printf("\nList of celset error codes:\n");
   for (j = 1; j <=2 ; j++) {
      printf("   %d: %s.\n", j, celset_errmsg[j]);
   }
 
   printf("\nList of celfwd error codes:\n");
   for (j = 1; j <=3 ; j++) {
      printf("   %d: %s.\n", j, celfwd_errmsg[j]);
   }
 
   printf("\nList of celrev error codes:\n");
   for (j = 1; j <=3 ; j++) {
      printf("   %d: %s.\n", j, celrev_errmsg[j]);
   }

   printf("\n");


   /* Set up Bonne's projection with conformal latitude at +35. */
   strcpy(pcode, "BON");

   /* Initialize projection parameters. */
   prj.flag = 0;
   prj.r0 = 0.0;
   for (j = 0; j < 10; prj.p[j++] = 0.0);
   prj.p[1] = 35.0;

   /* Set reference angles for the native grid. */
   native.ref[0] =   0.0;
   native.ref[1] =   0.0;
   native.ref[2] = 999.0;
   native.ref[3] = 999.0;
   native.flag   = 0;

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

   /* Define PGPLOT viewport. */
   pgenv(-180.0, 180.0, -90.0, 140.0, 1, -2);

   /* Loop over CRVAL1, CRVAL2, LONGPOLE, and LATPOLE. */
   crval1 = -180;
   for (crval2 =  -90; crval2 <=  90; crval2 += 45) {
      for (longpole = -180; longpole <= 180; longpole += 45) {
         for (latpole = -1; latpole <= 1; latpole += 2) {
            /* For the celestial grid, set the celestial coordinates of the
             * reference point of the projection (which for Bonne's projection
             * is at the intersection of the native equator and prime
             * meridian), the native longitude of the celestial pole, and
             * extra information needed to determine the native latitude of
             * the celestial pole.  These correspond to FITS keywords CRVAL1,
             * CRVAL2, LONGPOLE, and LATPOLE.
             */
            celestial.ref[0] = (double)crval1;
            celestial.ref[1] = (double)crval2;
            celestial.ref[2] = (double)longpole;
            celestial.ref[3] = (double)latpole;
            celestial.flag   = 0;

            /* Skip invalid values of LONGPOLE. */
            if (celset(pcode, &celestial, &prj)) {
               continue;
            }

            /* Skip redundant values of LATPOLE. */
            if (latpole == 1 && fabs(celestial.ref[3]) < 0.1) {
               continue;
            }

            /* Write a descriptive title. */
            sprintf(text, "%s projection - 15 degree graticule", pcode);
            printf("\n%s\n", text);
            pgtext(-180.0, -100.0, text);

            sprintf(text,
               "centered on celestial coordinates (%7.2lf,%6.2lf)",
               celestial.ref[0], celestial.ref[1]);
            printf("%s\n", text);
            pgtext (-180.0, -110.0, text);

            sprintf(text,
               "with north celestial pole at native coordinates (%7.2lf,%7.2lf)",
               celestial.ref[2], celestial.ref[3]);
            printf("%s\n", text);
            pgtext(-180.0, -120.0, text);


            /* Draw the native coordinate grid faintly in the background. */
            pgsci(8);

            /* Draw native meridians of longitude. */
            for (ilng = -180; ilng <= 180; ilng += 15) {
               lng = (double)ilng;
               if (ilng == -180) lng = -179.99;
               if (ilng ==  180) lng =  179.99;

               /* Dash the longitude of the celestial pole. */
               if ((ilng-longpole)%360 == 0) {
                  pgsls(2);
                  pgslw(5);
               }

               j = 0;
               for (ilat = -90; ilat <= 90; ilat++) {
                  lat = (double)ilat;

                  if (celfwd(pcode, lng, lat, &native, &phi, &theta, &prj,
                             &x, &y)) {
                     continue;
                  }

                  xr[j] = -x;
                  yr[j] =  y;
                  j++;
               }

               pgline(j, xr, yr);
               pgsls(1);
               pgslw(1);
            }

            /* Draw native parallels of latitude. */
            for (ilat = -90; ilat <= 90; ilat += 15) {
               lat = (double)ilat;

               j = 0;
               for (ilng = -180; ilng <= 180; ilng++) {
                  lng = (double)ilng;
                  if (ilng == -180) lng = -179.99;
                  if (ilng ==  180) lng =  179.99;

                  if (celfwd(pcode, lng, lat, &native, &phi, &theta, &prj,
                             &x, &y)) {
                     continue;
                  }

                  xr[j] = -x;
                  yr[j] =  y;
                  j++;
               }

               pgline(j, xr, yr);
            }


            /* Draw a colour-coded celestial coordinate grid. */
            ci = 1;

            /* Draw celestial meridians of longitude. */
            for (ilng = -180; ilng <= 180; ilng += 15) {
               lng = (double)ilng;

               if (++ci > 7) ci = 2;
               pgsci(ilng?ci:1);

               /* Dash the reference longitude. */
               if ((ilng-crval1)%360 == 0) {
                  pgsls(2);
                  pgslw(5);
               }

               j = 0;
               for (ilat = -90; ilat <= 90; ilat++) {
                  lat = (double)ilat;

                  if (celfwd(pcode, lng, lat, &celestial, &phi, &theta, &prj,
                             &x, &y)) {
                     continue;
                  }

                  /* Test for discontinuities. */
                  if (j > 0) {
                     if (fabs(x+xr[j-1]) > 4.0 || fabs(y-yr[j-1]) > 4.0) {
                        if (j > 1) pgline(j, xr, yr);
                        j = 0;
                     }
                  }

                  xr[j] = -x;
                  yr[j] =  y;
                  j++;
               }

               pgline(j, xr, yr);
               pgsls(1);
               pgslw(1);
            }

            /* Draw celestial parallels of latitude. */
            ci = 1;
            for (ilat = -90; ilat <= 90; ilat += 15) {
               lat = (double)ilat;

               if (++ci > 7) ci = 2;
               pgsci(ilat?ci:1);

               /* Dash the reference latitude. */
               if (ilat == crval2) {
                  pgsls(2);
                  pgslw(5);
               }

               j = 0;
               for (ilng = -180; ilng <= 180; ilng++) {
                  lng = (double)ilng;

                  if (celfwd(pcode, lng, lat, &celestial, &phi, &theta, &prj,
                             &x, &y)) {
                     continue;
                  }

                  /* Test for discontinuities. */
                  if (j > 0) {
                     if (fabs(x+xr[j-1]) > 4.0 || fabs(y-yr[j-1]) > 4.0) {
                        if (j > 1) pgline(j, xr, yr);
                        j = 0;
                     }
                  }

                  xr[j] = -x;
                  yr[j] =  y;
                  j++;
               }

               pgline(j, xr, yr);
               pgsls(1);
               pgslw(1);
            }

            /* New page. */
            pgpage();

            /* Cycle through celestial longitudes. */
            if ((crval1 += 15) > 180) crval1 = -180;

            /* Skip boring celestial latitudes. */
            if (crval2 == 0) break;
         }

         if (crval2 == 0) break;
      }
   }

   pgend();

   return 0;
}
