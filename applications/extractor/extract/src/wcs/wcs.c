/*=============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995,1996 Mark Calabretta
*
*   This library is free software; you can redistribute it and/or modify it
*   under the terms of the GNU Library General Public License as published
*   by the Free Software Foundation; either version 2 of the License, or (at
*   your option) any later version.
*
*   This library is distributed in the hope that it will be useful, but
*   WITHOUT ANY WARRANTY; without even the implied warranty of
*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library
*   General Public License for more details.
*
*   You should have received a copy of the GNU Library General Public License
*   along with this library; if not, write to the Free Software Foundation,
*   Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
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
*   C routines which implement the FITS World Coordinate System (WCS)
*   convention.
*
*   Summary of routines
*   -------------------
*   wcsfwd() and wcsrev() are high level driver routines for the WCS linear
*   transformation, spherical coordinate transformation, and spherical
*   projection routines.
*
*   Given either the celestial longitude or latitude plus an element of the
*   pixel coordinate a hybrid routine, wcsmix(), iteratively solves for the
*   unknown elements.
*
*   An initialization routine, wcsset(), computes indices from the ctype
*   array but need not be called explicitly - see the explanation of
*   wcs.flag below.
*
*
*   Initialization routine; wcsset()
*   --------------------------------
*   Initializes elements of a wcsprm data structure which holds indices into
*   the coordinate arrays.  Note that this routine need not be called directly;
*   it will be invoked by wcsfwd() and wcsrev() if the "flag" structure member
*   is anything other than a predefined magic value.
*
*   Given:
*      naxis    const int
*                        Number of image axes.
*      ctype[][9]
*               const char
*                        Coordinate axis types corresponding to the FITS
*                        CTYPEn header cards.
*
*   Returned:
*      wcs      wcsprm*  Indices for the celestial coordinates obtained
*                        by parsing the ctype[] array (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Inconsistent or unrecognized coordinate axis
*                              types.
*
*
*   Forward transformation; wcsfwd()
*   --------------------------------
*   Compute the pixel coordinate for given world coordinates.
*
*   Given:
*      ctype[][9]
*               const char
*                        Coordinate axis types corresponding to the FITS
*                        CTYPEn header cards.
*
*   Given or returned:
*      wcs      wcsprm*  Indices for the celestial coordinates obtained
*                        by parsing the ctype[] array (see below).
*
*   Given:
*      world    const double[]
*                        World coordinates.  world[wcs->lng] and
*                        world[wcs->lat] are the celestial longitude and
*                        latitude, in degrees.
*
*   Given:
*      crval    const double[]
*                        Coordinate reference values corresponding to the FITS
*                        CRVALn header cards.
*
*   Given and returned:
*      cel      celprm*  Spherical coordinate transformation parameters (usage
*                        is described in the prologue to "cel.c").
*
*   Returned:
*      phi,     double*  Longitude and latitude in the native coordinate
*      theta             system of the projection, in degrees.
*
*   Given and returned:
*      prj      prjprm*  Projection parameters (usage is described in the
*                        prologue to "proj.c").
*
*   Returned:
*      imgcrd   double[] Image coordinate.  imgcrd[wcs->lng] and
*                        imgcrd[wcs->lat] are the projected x-, and
*                        y-coordinates, in "degrees".  For quadcube
*                        projections with a CUBEFACE axis the face number is
*                        also returned in imgcrd[wcs->cubeface].
*
*   Given and returned:
*      lin      linprm*  Linear transformation parameters (usage is described
*                        in the prologue to "lin.c").
*
*   Returned:
*      pixcrd   double[] Pixel coordinate.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Invalid coordinate transformation parameters.
*                           2: Invalid projection parameters.
*                           3: Invalid world coordinate.
*                           4: Invalid linear transformation parameters.
*
*
*   Reverse transformation; wcsrev()
*   --------------------------------
*   Compute world coordinates for a given pixel coordinate.
*
*   Given:
*      ctype[][9]
*               const char
*                        Coordinate axis types corresponding to the FITS
*                        CTYPEn header cards.
*
*   Given or returned:
*      wcs      wcsprm*  Indices for the celestial coordinates obtained
*                        by parsing the ctype[] array (see below).
*
*   Given:
*      pixcrd   const double[]
*                        Pixel coordinate.
*
*   Given and returned:
*      lin      linprm*  Linear transformation parameters (usage is described
*                        in the prologue to "lin.c").
*
*   Returned:
*      imgcrd   double[] Image coordinate.  imgcrd[wcs->lng] and
*                        imgcrd[wcs->lat] are the projected x-, and
*                        y-coordinates, in "degrees".
*
*   Given and returned:
*      prj      prjprm*  Projection parameters (usage is described in the
*                        prologue to "proj.c").
*
*   Returned:
*      phi,     double*  Longitude and latitude in the native coordinate
*      theta             system of the projection, in degrees.
*
*   Given:
*      crval    const double[]
*                        Coordinate reference values corresponding to the FITS
*                        CRVALn header cards.
*
*   Given and returned:
*      cel      celprm*  Spherical coordinate transformation parameters
*                        (usage is described in the prologue to "cel.c").
*
*   Returned:
*      world    double[] World coordinates.  world[wcs->lng] and
*                        world[wcs->lat] are the celestial longitude and
*                        latitude, in degrees.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Invalid coordinate transformation parameters.
*                           2: Invalid projection parameters.
*                           3: Invalid pixel coordinate.
*                           4: Invalid linear transformation parameters.
*
*
*   Hybrid transformation; wcsmix()
*   -------------------------------
*   Given either the celestial longitude or latitude plus an element of the
*   pixel coordinate solve for the remaining elements by iterating on the
*   unknown celestial coordinate element using wcsfwd().
*
*   Given:
*      ctype[][9]
*               const char
*                        Coordinate axis types corresponding to the FITS
*                        CTYPEn header cards.
*
*   Given or returned:
*      wcs      wcsprm*  Indices for the celestial coordinates obtained
*                        by parsing the ctype[] array (see below).
*
*   Given:
*      mixpix   const int
*                        Which element of the pixel coordinate is given.
*      mixcel   const int
*                        Which element of the celestial coordinate is
*                        given:
*                           1: Celestial longitude is given in
*                              world[wcs->lng], latitude returned in
*                              world[wcs->lat].
*                           2: Celestial latitude is given in
*                              world[wcs->lng], longitude returned in
*                              world[wcs->lat].
*      vspan[2] const double
*                        Solution interval for the celestial coordinate, in
*                        degrees.
*      vstep    const double
*                        Step size for solution search, in degrees.  If zero,
*                        a sensible, although perhaps non-optimal default will
*                        be used.
*      viter    int
*                        If a solution is not found then the step size will be
*                        halved and the search recommenced.  viter controls
*                        how many times the step size is halved.  The allowed
*                        range is 0 - 5.
*
*   Given and returned:
*      world    double[] World coordinates.  world[wcs->lng] and
*                        world[wcs->lat] are the celestial longitude and
*                        latitude, in degrees.  Which is given and which
*                        returned depends on the value of mixcel.  All other
*                        elements are given.
*
*   Given:
*      crval    const double[]
*                        Coordinate reference values corresponding to the FITS
*                        CRVALn header cards.
*
*   Given and returned:
*      cel      celprm*  Spherical coordinate transformation parameters
*                        (usage is described in the prologue to "cel.c").
*
*   Returned:
*      phi,     double*  Longitude and latitude in the native coordinate
*      theta             system of the projection, in degrees.
*
*   Given and returned:
*      prj      prjprm*  Projection parameters (usage is described in the
*                        prologue to "proj.c").
*
*   Returned:
*      imgcrd   double[] Image coordinate.  imgcrd[wcs->lng] and
*                        imgcrd[wcs->lat] are the projected x-, and
*                        y-coordinates, in "degrees".
*
*   Given and returned:
*      lin      linprm*  Linear transformation parameters (usage is described
*                        in the prologue to "lin.c").
*
*   Given and returned:
*      pixcrd   double[] Pixel coordinate.  The element indicated by mixpix is
*                        given and the remaining elements are returned.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Invalid coordinate transformation parameters.
*                           2: Invalid projection parameters.
*                           3: Coordinate transformation error.
*                           4: Invalid linear transformation parameters.
*                           5: No solution found in the specified interval.
*
*
*   Notes
*   -----
*    1) The CTYPEn must in be upper case and there must be 0 or 1 pair of
*       matched celestial axis types.  The ctype[][9] should be padded with
*       blanks on the right and null-terminated.
*
*    2) Elements of the crval[] array which correspond to celestial axes are
*       ignored, the reference coordinate values in cel->ref[0] and
*       cel->ref[1] are the ones used.
*
*
*   WCS indexing parameters
*   -----------------------
*   The wcsprm struct consists of the following:
*
*      int flag
*         The wcsprm struct contains indexes and other information derived
*         from the CTYPEn.  Whenever any of the ctype[] are set or changed
*         this flag must be set to zero to signal the initialization routine,
*         wcsset() to redetermine the indices.  The flag is set to 999 if
*         there is no celestial axis pair in the CTYPEn.
*
*      char pcode[4]
*         The WCS projection code.
*
*      char lngtyp[5], lattyp[5]
*         WCS celestial axis types.
*
*      int lng,lat
*         Indices into the imgcrd[], and world[] arrays as described above.
*         These may also serve as indices for the celestial longitude and
*         latitude axes in the pixcrd[] array provided that the PC matrix
*         does not transpose axes.
*
*      int cubeface
*         Index into the pixcrd[] array for the CUBEFACE axis.  This is
*         optionally used for the quadcube projections where each cube face is
*         stored on a separate axis.
*
*
*   wcsmix() algorithm
*   ------------------
*      Initially the specified solution interval is checked to see if it's a
*      "crossing" interval.  If it isn't, a search is made for a crossing
*      solution by iterating on the unknown celestial coordinate starting at
*      the upper limit of the solution interval and decrementing by the
*      specified step size.  A crossing is indicated if the trial value of the
*      pixel coordinate steps through the value specified.  If a crossing
*      interval is found then the solution is determined by a modified form of
*      "regula falsi" division of the crossing interval.  If no crossing
*      interval was found within the specified solution interval then a search
*      is made for a "non-crossing" solution as may arise from a point of
*      tangency.  The process is complicated by having to make allowance for
*      the discontinuities that occur in all map projections.
*
*      Once one solution has been determined others may be found by subsequent
*      invokations of wcsmix() with suitably restricted solution intervals.
*
*      Note that there is a circumstance where the problem posed to wcsmix()
*      is ill-conditioned and it may fail to find a valid solution where one
*      does exist.  This may arise when the solution point lies at a native
*      pole of a projection in which the pole is represented as a finite
*      interval.
*
*      Because of its generality wcsmix() is very compute-intensive.  For
*      compute-limited applications more efficient special-case solvers could
*      be written for simple projections, for example non-oblique cylindrical
*      projections.
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   $Id: wcs.c,v 2.5 1996/09/10 06:37:10 mcalabre Exp $
*===========================================================================*/

#include "wcs.h"

/* Map error number to error message for each function. */
const char *wcsset_errmsg[] = {
   0,
   "Inconsistent or unrecognized coordinate axis types"};

const char *wcsfwd_errmsg[] = {
   0,
   "Invalid coordinate transformation parameters",
   "Invalid projection parameters",
   "Invalid world coordinate",
   "Invalid linear transformation parameters"};

const char *wcsrev_errmsg[] = {
   0,
   "Invalid coordinate transformation parameters",
   "Invalid projection parameters",
   "Invalid pixel coordinate",
   "Invalid linear transformation parameters"};

const char *wcsmix_errmsg[] = {
   0,
   "Invalid coordinate transformation parameters",
   "Invalid projection parameters",
   "Coordinate transformation error",
   "Invalid linear transformation parameters",
   "No solution found in the specified interval"};


#ifdef SIGNBIT
#define wcs_signbit(X) ((X) < 0.0 ? 0 : 1)
#endif


int wcsset (naxis, ctype, wcs)

const int naxis;
const char ctype[][9];
struct wcsprm *wcs;

{
   int j, k, *ndx;
   char requir[9];

   strcpy(wcs->pcode, "");
   strcpy(requir, "");
   wcs->lng = -1;
   wcs->lat = -1;
   wcs->cubeface = -1;

   for (j = 0; j < naxis; j++) {
      if (ctype[j][4] != '-') {
         if (strcmp(ctype[j], "CUBEFACE") == 0) {
            if (wcs->cubeface == -1) {
               wcs->cubeface = j;
            } else {
               /* Multiple CUBEFACE axes! */
               return 1;
            }
         }
         continue;
      }

      /* Got an axis qualifier, is it a recognized WCS projection? */
      for (k = 0; k < npcode; k++) {
         if (strncmp(&ctype[j][5], pcodes[k], 3) == 0) break;
      }

      if (k == npcode) continue;

      /* Parse the celestial axis type. */
      if (strcmp(wcs->pcode, "") == 0) {
         sprintf(wcs->pcode, "%.3s", &ctype[j][5]);

         if (strncmp(ctype[j], "RA--", 4) == 0) {
            wcs->lng = j;
            strcpy(wcs->lngtyp, "RA");
            strcpy(wcs->lattyp, "DEC");
            ndx = &wcs->lat;
            sprintf(requir, "DEC--%s", wcs->pcode);
         } else if (strncmp(ctype[j], "DEC-", 4) == 0) {
            wcs->lat = j;
            strcpy(wcs->lngtyp, "RA");
            strcpy(wcs->lattyp, "DEC");
            ndx = &wcs->lng;
            sprintf(requir, "RA---%s", wcs->pcode);
         } else if (strncmp(&ctype[j][1], "LON", 3) == 0) {
            wcs->lng = j;
            sprintf(wcs->lngtyp, "%cLON", ctype[j][0]);
            sprintf(wcs->lattyp, "%cLAT", ctype[j][0]);
            ndx = &wcs->lat;
            sprintf(requir, "%s-%s", wcs->lattyp, wcs->pcode);
         } else if (strncmp(&ctype[j][1], "LAT", 3) == 0) {
            wcs->lat = j;
            sprintf(wcs->lngtyp, "%cLON", ctype[j][0]);
            sprintf(wcs->lattyp, "%cLAT", ctype[j][0]);
            ndx = &wcs->lng;
            sprintf(requir, "%s-%s", wcs->lngtyp, wcs->pcode);
         } else {
            /* Unrecognized celestial type. */
            return 1;
         }
      } else {
         if (strncmp(ctype[j], requir, 8) != 0) {
            /* Inconsistent projection types. */
            return 1;
         }

         *ndx = j;
         strcpy(requir, "");
      }
   }

   if (strcmp(requir, "")) {
      /* Unmatched celestial axis. */
      return 1;
   }

   if (strcmp(wcs->pcode, "")) {
      wcs->flag = WCSSET;
   } else {
      /* Signal for no celestial axis pair. */
      wcs->flag = 999;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int wcsfwd(ctype, wcs, world, crval, cel, phi, theta, prj, imgcrd, lin,
    pixcrd)

const char ctype[][9];
struct wcsprm* wcs;
const double world[];
const double crval[];
struct celprm *cel;
double *phi, *theta;
struct prjprm *prj;
double imgcrd[];
struct linprm *lin;
double pixcrd[];

{
   int    err, j;
   double offset;

   /* Initialize if required. */
   if (wcs->flag != WCSSET) {
      if (wcsset(lin->naxis, ctype, wcs)) return 1;
   }

   /* Convert to relative physical coordinates. */
   for (j = 0; j < lin->naxis; j++) {
      if (j == wcs->lng) continue;
      if (j == wcs->lat) continue;
      imgcrd[j] = world[j] - crval[j];
   }

   if (wcs->flag != 999) {
      /* Compute projected coordinates. */
      if (err = celfwd(wcs->pcode, world[wcs->lng], world[wcs->lat], cel,
                   phi, theta, prj, &imgcrd[wcs->lng], &imgcrd[wcs->lat])) {
         return err;
      }

      /* Do we have a CUBEFACE axis? */
      if (wcs->cubeface != -1) {
         /* Separation between faces. */
         if (prj->r0 == 0.0) {
            offset = 45.0;
         } else {
            offset = prj->r0*PI/4.0;
         }
 
         /* Stack faces in a cube. */
         if (imgcrd[wcs->lat] < offset) {
            imgcrd[wcs->lat] += offset;
            imgcrd[wcs->cubeface] = 5.0;
         } else if (imgcrd[wcs->lng] < offset*3) {
            imgcrd[wcs->lng] += offset*3;
            imgcrd[wcs->cubeface] = 4.0;
         } else if (imgcrd[wcs->lng] < offset*2) {
            imgcrd[wcs->lng] += offset*2;
            imgcrd[wcs->cubeface] = 3.0;
         } else if (imgcrd[wcs->lng] < offset) {
            imgcrd[wcs->lng] += offset;
            imgcrd[wcs->cubeface] = 2.0;
         } else if (imgcrd[wcs->lat] > offset) {
            imgcrd[wcs->lat] -= offset;
            imgcrd[wcs->cubeface] = 0.0;
         } else {
            imgcrd[wcs->cubeface] = 1.0;
         }
      }
   }

   /* Apply forward linear transformation. */
   if (linfwd(imgcrd, lin, pixcrd)) {
      return 4;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int wcsrev(ctype, wcs, pixcrd, lin, imgcrd, prj, phi, theta, crval, cel,
    world)

const char ctype[][9];
struct wcsprm *wcs;
const double pixcrd[];
struct linprm *lin;
double imgcrd[];
struct prjprm *prj;
double *phi, *theta;
const double crval[];
struct celprm *cel;
double world[];

{
   int    err, face, j;
   double offset;

   /* Initialize if required. */
   if (wcs->flag != WCSSET) {
      if (wcsset(lin->naxis, ctype, wcs)) return 1;
   }

   /* Apply reverse linear transformation. */
   if (linrev(pixcrd, lin, imgcrd)) {
      return 4;
   }

   /* Convert to world coordinates. */
   for (j = 0; j < lin->naxis; j++) {
      if (j == wcs->lng) continue;
      if (j == wcs->lat) continue;
      world[j] = imgcrd[j] + crval[j];
   }


   if (wcs->flag != 999) {
      /* Do we have a CUBEFACE axis? */
      if (wcs->cubeface != -1) {
         face = (int)(imgcrd[wcs->cubeface] + 0.5);
         if (fabs(imgcrd[wcs->cubeface]-face) > 1e-10) {
            return 3;
         }

         /* Separation between faces. */
         if (prj->r0 == 0.0) {
            offset = 45.0;
         } else {
            offset = prj->r0*PI/4.0;
         }

         /* Lay out faces in a plane. */
         switch (face) {
         case 0:
            imgcrd[wcs->lat] += offset;
            break;
         case 1:
            break;
         case 2:
            imgcrd[wcs->lng] -= offset;
            break;
         case 3:
            imgcrd[wcs->lng] -= offset*2;
            break;
         case 4:
            imgcrd[wcs->lng] -= offset*3;
            break;
         case 5:
            imgcrd[wcs->lat] -= offset;
            break;
         default:
            return 3;
         }
      }

      /* Compute celestial coordinates. */
      if (err = celrev(wcs->pcode, imgcrd[wcs->lng], imgcrd[wcs->lat], prj,
                   phi, theta, cel, &world[wcs->lng], &world[wcs->lat])) {
         return err;
      }
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int wcsmix(ctype, wcs, mixpix, mixcel, vspan, vstep, viter, world, crval, cel,
           phi, theta, prj, imgcrd, lin, pixcrd)

const char ctype[][9];
struct wcsprm *wcs;
const int mixpix, mixcel;
const double vspan[2], vstep;
int viter;
double world[];
const double crval[];
struct celprm *cel;
double *phi, *theta;
struct prjprm *prj;
double imgcrd[];
struct linprm *lin;
double pixcrd[];

{
   const int niter = 60;
   int    crossed, err, istep, iter, retry;
   const double tol = 1.0e-10;
   double lambda, span[2], step;
   double pixmix;
   double lng, lng0, lng0m, lng1, lng1m;
   double lat, lat0, lat0m, lat1, lat1m;
   double d, d0, d0m, d1, d1m, dx;
   double dabs, dmin, lmin;

   /* Check vspan. */
   if (vspan[0] <= vspan[1]) {
      span[0] = vspan[0];
      span[1] = vspan[1];
   } else {
      /* Swap them. */
      span[0] = vspan[1];
      span[1] = vspan[0];
   }

   /* Check vstep. */
   step = fabs(vstep);
   if (step == 0.0) {
      step = (span[1] - span[0])/10.0;
      if (step > 1.0 || step == 0.0) step = 1.0;
   }

   /* Check viter. */
   if (viter < 0) {
      viter = 0;
   } else if (viter > 5) {
      viter = 5;
   }

   /* Given pixel element. */
   pixmix = pixcrd[mixpix];

   /* Iterate on the step size. */
   for (istep = 0; istep <= viter; istep++) {
      if (istep) step /= 2.0;

      /* Iterate on the sky coordinate between the specified range. */
      if (mixcel == 1) {
         /* Celestial longitude is given. */

         /* Check whether the solution interval is a crossing interval. */
         lat0 = span[0];
         world[wcs->lat] = lat0;
         if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta, prj,
                          imgcrd, lin, pixcrd)) {
            return err;
         }
         d0 = pixcrd[mixpix] - pixmix;

         dabs = fabs(d0);
         if (dabs < tol) return 0;

         lat1 = span[1];
         world[wcs->lat] = lat1;
         if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta, prj,
                          imgcrd, lin, pixcrd)) {
            return err;
         }
         d1 = pixcrd[mixpix] - pixmix;

         dabs = fabs(d1);
         if (dabs < tol) return 0;

         lmin = lat1;
         dmin = dabs;

         /* Check for a crossing point. */
         if (wcs_signbit(d0) != wcs_signbit(d1)) {
            crossed = 1;
            dx = d1;
         } else {
            crossed = 0;
            lat0 = span[1];
         }

         for (retry = 0; retry < 4; retry++) {
            /* Refine the solution interval. */
            while (lat0 > span[0]) {
               lat0 -= step;
               if (lat0 < span[0]) lat0 = span[0];
               world[wcs->lat] = lat0;
               if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                prj, imgcrd, lin, pixcrd)) {
                  return err;
               }
               d0 = pixcrd[mixpix] - pixmix;

               /* Check for a solution. */
               dabs = fabs(d0);
               if (dabs < tol) return 0;

               /* Record the point of closest approach. */
               if (dabs < dmin) {
                  lmin = lat0;
                  dmin = dabs;
               }

               /* Check for a crossing point. */
               if (wcs_signbit(d0) != wcs_signbit(d1)) {
                  crossed = 2;
                  dx = d0;
                  break;
               }

               /* Advance to the next subinterval. */
               lat1 = lat0;
               d1 = d0;
            }

            if (crossed) {
               /* A crossing point was found. */
               for (iter = 0; iter < niter; iter++) {
                  /* Use regula falsi division of the interval. */
                  lambda = d0/(d0-d1);
                  if (lambda < 0.1) {
                     lambda = 0.1;
                  } else if (lambda > 0.9) {
                     lambda = 0.9;
                  }

                  lat = lat0 + lambda*(lat1 - lat0);
                  world[wcs->lat] = lat;
                  if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                   prj, imgcrd, lin, pixcrd)) {
                     return err;
                  }
                  d = pixcrd[mixpix] - pixmix;

                  /* Check for a solution. */
                  dabs = fabs(d);
                  if (dabs < tol) return 0;

                  /* Record the point of closest approach. */
                  if (dabs < dmin) {
                     lmin = lat;
                     dmin = dabs;
                  }

                  if (wcs_signbit(d0) == wcs_signbit(d)) {
                     lat0 = lat;
                     d0 = d;
                  } else {
                     lat1 = lat;
                     d1 = d;
                  }
               }

               /* No convergence, must have been a discontinuity. */
               if (crossed == 1) lat0 = span[1];
               lat1 = lat0;
               d1 = dx;
               crossed = 0;

            } else {
               /* No crossing point; look for a tangent point. */
               if (lmin == span[0]) break;
               if (lmin == span[1]) break;

               lat = lmin;
               lat0 = lat - step;
               if (lat0 < span[0]) lat0 = span[0];
               lat1 = lat + step;
               if (lat1 > span[1]) lat1 = span[1];

               world[wcs->lat] = lat0;
               if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                prj, imgcrd, lin, pixcrd)) {
                  return err;
               }
               d0 = fabs(pixcrd[mixpix] - pixmix);

               d  = dmin;

               world[wcs->lat] = lat1;
               if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                prj, imgcrd, lin, pixcrd)) {
                  return err;
               }
               d1 = fabs(pixcrd[mixpix] - pixmix);

               for (iter = 0; iter < niter; iter++) {
                  lat0m = (lat0 + lat)/2.0;
                  world[wcs->lat] = lat0m;
                  if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                   prj, imgcrd, lin, pixcrd)) {
                     return err;
                  }
                  d0m = fabs(pixcrd[mixpix] - pixmix);

                  if (d0m < tol) return 0;

                  lat1m = (lat1 + lat)/2.0;
                  world[wcs->lat] = lat1m;
                  if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                   prj, imgcrd, lin, pixcrd)) {
                     return err;
                  }
                  d1m = fabs(pixcrd[mixpix] - pixmix);

                  if (d1m < tol) return 0;

                  if (d0m < d && d0m <= d1m) {
                     lat1 = lat;
                     d1   = d;
                     lat  = lat0m;
                     d    = d0m;
                  } else if (d1m < d) {
                     lat0 = lat;
                     d0   = d;
                     lat  = lat1m;
                     d    = d1m;
                  } else {
                     lat0 = lat0m;
                     d0   = d0m;
                     lat1 = lat1m;
                     d1   = d1m;
                  }
               }
            }
         }

      } else {
         /* Celestial latitude is given. */

         /* Check whether the solution interval is a crossing interval. */
         lng0 = span[0];
         world[wcs->lng] = lng0;
         if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta, prj,
                          imgcrd, lin, pixcrd)) {
            return err;
         }
         d0 = pixcrd[mixpix] - pixmix;

         dabs = fabs(d0);
         if (dabs < tol) return 0;

         lng1 = span[1];
         world[wcs->lng] = lng1;
         if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta, prj,
                          imgcrd, lin, pixcrd)) {
            return err;
         }
         d1 = pixcrd[mixpix] - pixmix;

         dabs = fabs(d1);
         if (dabs < tol) return 0;
         lmin = lng1;
         dmin = dabs;

         /* Check for a crossing point. */
         if (wcs_signbit(d0) != wcs_signbit(d1)) {
            crossed = 1;
            dx = d1;
         } else {
            crossed = 0;
            lng0 = span[1];
         }

         for (retry = 0; retry < 4; retry++) {
            /* Refine the solution interval. */
            while (lng0 > span[0]) {
               lng0 -= step;
               if (lng0 < span[0]) lng0 = span[0];
               world[wcs->lng] = lng0;
               if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                          prj, imgcrd, lin, pixcrd)) {
                  return err;
               }
               d0 = pixcrd[mixpix] - pixmix;

               /* Check for a solution. */
               dabs = fabs(d0);
               if (dabs < tol) return 0;

               /* Record the point of closest approach. */
               if (dabs < dmin) {
                  lmin = lng0;
                  dmin = dabs;
               }

               /* Check for a crossing point. */
               if (wcs_signbit(d0) != wcs_signbit(d1)) {
                  crossed = 2;
                  dx = d0;
                  break;
               }

               /* Advance to the next subinterval. */
               lng1 = lng0;
               d1 = d0;
            }

            if (crossed) {
               /* A crossing point was found. */
               for (iter = 0; iter < niter; iter++) {
                  /* Use regula falsi division of the interval. */
                  lambda = d0/(d0-d1);
                  if (lambda < 0.1) {
                     lambda = 0.1;
                  } else if (lambda > 0.9) {
                     lambda = 0.9;
                  }

                  lng = lng0 + lambda*(lng1 - lng0);
                  world[wcs->lng] = lng;
                  if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                   prj, imgcrd, lin, pixcrd)) {
                     return err;
                  }
                  d = pixcrd[mixpix] - pixmix;

                  /* Check for a solution. */
                  dabs = fabs(d);
                  if (dabs < tol) return 0;

                  /* Record the point of closest approach. */
                  if (dabs < dmin) {
                     lmin = lng;
                     dmin = dabs;
                  }

                  if (wcs_signbit(d0) == wcs_signbit(d)) {
                     lng0 = lng;
                     d0 = d;
                  } else {
                     lng1 = lng;
                     d1 = d;
                  }
               }

               /* No convergence, must have been a discontinuity. */
               if (crossed == 1) lng0 = span[1];
               lng1 = lng0;
               d1 = dx;
               crossed = 0;

            } else {
               /* No crossing point; look for a tangent point. */
               if (lmin == span[0]) break;
               if (lmin == span[1]) break;

               lng = lmin;
               lng0 = lng - step;
               if (lng0 < span[0]) lng0 = span[0];
               lng1 = lng + step;
               if (lng1 > span[1]) lng1 = span[1];

               world[wcs->lng] = lng0;
               if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                prj, imgcrd, lin, pixcrd)) {
                  return err;
               }
               d0 = fabs(pixcrd[mixpix] - pixmix);

               d  = dmin;

               world[wcs->lng] = lng1;
               if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                prj, imgcrd, lin, pixcrd)) {
                  return err;
               }
               d1 = fabs(pixcrd[mixpix] - pixmix);

               for (iter = 0; iter < niter; iter++) {
                  lng0m = (lng0 + lng)/2.0;
                  world[wcs->lng] = lng0m;
                  if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                   prj, imgcrd, lin, pixcrd)) {
                     return err;
                  }
                  d0m = fabs(pixcrd[mixpix] - pixmix);

                  if (d0m < tol) return 0;

                  lng1m = (lng1 + lng)/2.0;
                  world[wcs->lng] = lng1m;
                  if (err = wcsfwd(ctype, wcs, world, crval, cel, phi, theta,
                                   prj, imgcrd, lin, pixcrd)) {
                     return err;
                  }
                  d1m = fabs(pixcrd[mixpix] - pixmix);

                  if (d1m < tol) return 0;

                  if (d0m < d && d0m <= d1m) {
                     lng1 = lng;
                     d1   = d;
                     lng  = lng0m;
                     d    = d0m;
                  } else if (d1m < d) {
                     lng0 = lng;
                     d0   = d;
                     lng  = lng1m;
                     d    = d1m;
                  } else {
                     lng0 = lng0m;
                     d0   = d0m;
                     lng1 = lng1m;
                     d1   = d1m;
                  }
               }
            }
         }
      }
   }

   /* No solution. */
   return 5;

}
