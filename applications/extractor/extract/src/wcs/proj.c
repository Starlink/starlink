/*============================================================================
*
*   WCSLIB - an implementation of the FITS WCS proposal.
*   Copyright (C) 1995-1999, Mark Calabretta
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
*   C implementation of the spherical map projections recognized by the FITS
*   "World Coordinate System" (WCS) convention.
*
*   Summary of routines
*   -------------------
*   Each projection is implemented via separate functions for the forward,
*   *fwd(), and reverse, *rev(), transformation.
*
*   Initialization routines, *set(), compute intermediate values from the
*   projection parameters but need not be called explicitly - see the
*   explanation of prj.flag below.
*
*      azpset azpfwd azprev   AZP: zenithal/azimuthal perspective
*      tanset tanfwd tanrev   TAN: gnomonic
*      sinset sinfwd sinrev   SIN: orthographic/synthesis
*      stgset stgfwd stgrev   STG: stereographic
*      arcset arcfwd arcrev   ARC: zenithal/azimuthal equidistant
*      zpnset zpnfwd zpnrev   ZPN: zenithal/azimuthal polynomial
*      zeaset zeafwd zearev   ZEA: zenithal/azimuthal equal area
*      airset airfwd airrev   AIR: Airy
*      cypset cypfwd cyprev   CYP: cylindrical perspective
*      carset carfwd carrev   CAR: Cartesian
*      merset merfwd merrev   MER: Mercator
*      ceaset ceafwd cearev   CEA: cylindrical equal area
*      copset copfwd coprev   COP: conic perspective
*      codset codfwd codrev   COD: conic equidistant
*      coeset coefwd coerev   COE: conic equal area
*      cooset coofwd coorev   COO: conic orthomorphic
*      bonset bonfwd bonrev   BON: Bonne
*      pcoset pcofwd pcorev   PCO: polyconic
*      glsset glsfwd glsrev   GLS: Sanson-Flamsteed (global sinusoidal)
*      parset parfwd parrev   PAR: parabolic
*      aitset aitfwd aitrev   AIT: Hammer-Aitoff
*      molset molfwd molrev   MOL: Mollweide
*      cscset cscfwd cscrev   CSC: COBE quadrilateralized spherical cube
*      qscset qscfwd qscrev   QSC: quadrilateralized spherical cube
*      tscset tscfwd tscrev   TSC: tangential spherical cube
*      tnxset tnxfwd tnxrev   TNX: IRAF's gnomonic
*
*
*   Initialization routine; *set()
*   ------------------------------
*   Initializes members of a prjprm data structure which hold intermediate
*   values.  Note that this routine need not be called directly; it will be
*   invoked by prjfwd() and prjrev() if the "flag" structure member is
*   anything other than a predefined magic value.
*
*   Given and/or returned:
*      prj      prjprm*  Projection parameters (see below).
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Invalid projection parameters.
*
*   Forward transformation; *fwd()
*   -----------------------------
*   Compute (x,y) coordinates in the plane of projection from native spherical
*   coordinates (phi,theta).
*
*   Given:
*      phi,     const double
*      theta             Longitude and latitude of the projected point in
*                        native spherical coordinates, in degrees.
*
*   Given and returned:
*      prj      prjprm*  Projection parameters (see below).
*
*   Returned:
*      x,y      double*  Projected coordinates.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Invalid projection parameters.
*                           2: Invalid value of (phi,theta).
*
*   Reverse transformation; *rev()
*   -----------------------------
*   Compute native spherical coordinates (phi,theta) from (x,y) coordinates in
*   the plane of projection.
*
*   Given:
*      x,y      const double
*                        Projected coordinates.
*
*   Given and returned:
*      prj      prjprm*  Projection parameters (see below).
*
*   Returned:
*      phi,     double*  Longitude and latitude of the projected point in
*      theta             native spherical coordinates, in degrees.
*
*   Function return value:
*               int      Error status
*                           0: Success.
*                           1: Invalid projection parameters.
*                           2: Invalid value of (x,y).
*
*   Projection parameters
*   ---------------------
*   The prjprm struct consists of the following:
*
*      int flag
*         This flag must be set to zero whenever any of p[10] or r0 are set
*         or changed.  This signals the initialization routine to recompute
*         intermediaries.  flag may also be set to -1 to disable strict bounds
*         checking for the AZP, TAN, SIN, ZPN, and COP projections.
*      double r0
*         r0; The radius of the generating sphere for the projection, a linear
*         scaling parameter.  If this is zero, it will be reset to the default
*         value of 180/pi (the value for FITS WCS).
*      double p[10]
*         The first 10 elements contain projection parameters which correspond
*         to the PROJPn keywords in FITS, so p[0] is PROJP0, and p[9] is
*         PROJP9.  Many projections use p[1] (PROJP1) and some also use p[2]
*         (PROJP2).  ZPN is the only projection which uses any of the others.
*
*   The remaining members of the prjprm struct are maintained by the
*   initialization routines and should not be modified.  This is done for the
*   sake of efficiency and to allow an arbitrary number of contexts to be
*   maintained simultaneously.
*
*      int n
*      double w[10]
*         Intermediate values derived from the projection parameters.
*
*   Usage of the p[] array as it applies to each projection is described in
*   the prologue to each trio of projection routines.
*
*   Argument checking
*   -----------------
*   Forward routines:
*
*      The values of phi and theta (the native longitude and latitude)
*      normally lie in the range [-180,180] for phi, and [-90,90] for theta.
*      However, all forward projections will accept any value of phi and will
*      not normalize it.
*
*      The forward projection routines do not explicitly check that theta lies
*      within the range [-90,90].  They do check for any value of theta which
*      produces an invalid argument to the projection equations (e.g. leading
*      to division by zero).  The forward routines for AZP, TAN, SIN, ZPN, and
*      COP also return error 2 if (phi,theta) corresponds to the overlapped
*      (far) side of the projection but also return the corresponding value of
*      (x,y).  This strict bounds checking may be relaxed by setting prj->flag
*      to -1 (rather than 0) when these projections are initialized.
*
*   Reverse routines:
*
*      Error checking on the projected coordinates (x,y) is limited to that
*      required to ascertain whether a solution exists.  Where a solution does
*      exist no check is made that the value of phi and theta obtained lie
*      within the ranges [-180,180] for phi, and [-90,90] for theta.
*
*   Accuracy
*   --------
*   Closure to a precision of at least 1E-10 degree of longitude and latitude
*   has been verified for typical projection parameters on the 1 degree grid
*   of native longitude and latitude (to within 5 degrees of any latitude
*   where the projection may diverge).
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*   IRAF's TNX added by E.Bertin 2000/08/23
*   $Id: proj.c,v 1.1.1.1 2002/03/15 16:33:26 bertin Exp $
*===========================================================================*/

#ifdef HAVE_CONFIG_H
#include	"config.h"
#endif

#include <math.h>
#include <stdio.h>
#include "poly.h"
#include "proj.h"
#include "tnx.h"
#include "wcsmath.h"
#include "wcstrig.h"

/* Map error number to error message for each function. */
const char *prjset_errmsg[] = {
   0,
   "Invalid projection parameters"};

const char *prjfwd_errmsg[] = {
   0,
   "Invalid projection parameters",
   "Invalid value of (phi,theta)"};

const char *prjrev_errmsg[] = {
   0,
   "Invalid projection parameters",
   "Invalid value of (x,y)"};


#ifdef COPYSIGN
#define wcs_copysign(X, Y) ((Y) < 0.0 ? -fabs(X) : fabs(X))
#endif


/*============================================================================
*   AZP: zenithal/azimuthal perspective projection.
*
*   Given:
*      prj->p[1]   AZP distance parameter, mu in units of r0.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(mu+1)
*      prj->w[1]   1/prj->w[0]
*      prj->w[2]   Boundary parameter, -mu    for |mu| <= 1,
*                                      -1/mu  for |mu| >= 1.
*===========================================================================*/

int azpset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) prj->r0 = R2D;

   prj->w[0] = prj->r0*(prj->p[1] + 1.0);
   if (prj->w[0] == 0.0) {
      return 1;
   }

   prj->w[1] = 1.0/prj->w[0];
   if (fabs(prj->p[1]) <= 1.0) {
      prj->w[2] = -prj->p[1];
   } else {
      prj->w[2] = -1.0/prj->p[1];
   }

   if (prj->flag == -1) {
      prj->flag = -PRJSET;
   } else {
      prj->flag = PRJSET;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int azpfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double r, s, sthe;

   if (abs(prj->flag) != PRJSET) {
      if (azpset(prj)) return 1;
   }

   sthe = wcs_sind(theta);

   s = prj->p[1] + sthe;
   if (s == 0.0) {
      return 2;
   }

   r =  prj->w[0]*wcs_cosd(theta)/s;
   *x =  r*wcs_sind(phi);
   *y = -r*wcs_cosd(phi);

   if (prj->flag == PRJSET && sthe < prj->w[2]) {
      return 2;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int azprev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double r, rho, s;
   const double tol = 1.0e-13;

   if (abs(prj->flag) != PRJSET) {
      if (azpset(prj)) return 1;
   }

   r = sqrt(x*x + y*y);
   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = wcs_atan2d(x, -y);
   }

   rho = r*prj->w[1];
   s = rho*prj->p[1]/sqrt(rho*rho+1.0);
   if (fabs(s) > 1.0) {
      if (fabs(s) > 1.0+tol) {
         return 2;
      }
      *theta = wcs_atan2d(1.0,rho) - wcs_copysign(90.0,s);
   } else {
      *theta = wcs_atan2d(1.0,rho) - wcs_asind(s);
   }

   return 0;
}

/*============================================================================
*   TAN: gnomonic projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*===========================================================================*/

int tanset(prj)

struct prjprm *prj;

{
   int	k;

   if (prj->r0 == 0.0) prj->r0 = R2D;

   if (prj->flag == -1) {
      prj->flag = -PRJSET;
   } else {
      prj->flag = PRJSET;
   } 

   for (k = 99; k >= 0 && prj->p[k] == 0.0 && prj->p[k+100] == 0.0; k--);
   if (k < 0)
     {
     k = 2;
     prj->p[1] = prj->p[101] = 1.0;
     }

   prj->n = k;

   return 0;
}

/*--------------------------------------------------------------------------*/

int tanfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double r, s, xp[2];

   if (abs(prj->flag) != PRJSET) {
      if(tanset(prj)) return 1;
   }

   s = wcs_sind(theta);
   if (s == 0.0) return 2;

   r =  prj->r0*wcs_cosd(theta)/s;
   xp[0] =  r*wcs_sind(phi);
   xp[1] = -r*wcs_cosd(phi);
   *x = prj->inv_x? poly_func(prj->inv_x, xp) : xp[0];
   *y = prj->inv_y? poly_func(prj->inv_y, xp) : xp[1];

   if (prj->flag == PRJSET && s < 0.0) {
      return 2;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int tanrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double	xp,yp, rp;

   if (abs(prj->flag) != PRJSET) {
      if (tanset(prj)) return 1;
   }

   raw_to_pv(prj, x,y, &xp, &yp);
   rp = sqrt(xp*xp+yp*yp);
   if (rp == 0.0) {
      *phi = 0.0;
   } else {
      *phi = wcs_atan2d(xp, -yp);
   }
   *theta = wcs_atan2d(prj->r0, rp);

   return 0;
}

/*============================================================================
*   SIN: orthographic/synthesis projection.
*
*   Given:
*      prj->p[1:2] SIN obliqueness parameters, alpha and beta.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   1/r0
*      prj->w[1]   alpha**2 + beta**2
*      prj->w[2]   2*(alpha**2 + beta**2)
*      prj->w[3]   2*(alpha**2 + beta**2 + 1)
*      prj->w[4]   alpha**2 + beta**2 - 1
*===========================================================================*/

int sinset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) prj->r0 = R2D;

   prj->w[0] = 1.0/prj->r0;
   prj->w[1] = prj->p[1]*prj->p[1] + prj->p[2]*prj->p[2];
   prj->w[2] = 2.0*prj->w[1];
   prj->w[3] = prj->w[2] + 2.0;
   prj->w[4] = prj->w[1] - 1.0;

   if (prj->flag == -1) {
      prj->flag = -PRJSET;
   } else {
      prj->flag = PRJSET;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int sinfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double cphi, cthe, sphi, t, z;

   if (abs(prj->flag) != PRJSET) {
      if (sinset(prj)) return 1;
   }

   t = (90.0 - fabs(theta))*D2R;
   if (t < 1.0e-5) {
      if (theta > 0.0) {
         z = -t*t/2.0;
      } else {
         z = -2.0 + t*t/2.0;
      }
      cthe = t;
   } else {
      z =  wcs_sind(theta) - 1.0;
      cthe = wcs_cosd(theta);
   }

   cphi = wcs_cosd(phi);
   sphi = wcs_sind(phi);
   *x =  prj->r0*(cthe*sphi + prj->p[1]*z);
   *y = -prj->r0*(cthe*cphi + prj->p[2]*z);
   /* Validate this solution. */
   if (prj->flag == PRJSET) {
      if (prj->w[1] == 0.0) {
         /* Orthographic projection. */
         if (theta < 0.0) {
            return 2;
         }
      } else {
         /* "Synthesis" projection. */
         t = wcs_atand(prj->p[1]*sphi + prj->p[2]*cphi);
         if (theta < t) {
            return 2;
         }
      }
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int sinrev (x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   const double tol = 1.0e-13;
   double a, b, c, d, r2, sth, sth1, sth2, sxy, x0, xp, y0, yp, z;

   if (abs(prj->flag) != PRJSET) {
      if (sinset(prj)) return 1;
   }

   /* Compute intermediaries. */
   x0 = x*prj->w[0];
   y0 = y*prj->w[0];
   r2 = x0*x0 + y0*y0;

   if (prj->w[1] == 0.0) {
      /* Orthographic projection. */
      if (r2 != 0.0) {
         *phi = wcs_atan2d(x0, -y0);
      } else {
         *phi = 0.0;
      }

      if (r2 < 0.5) {
         *theta = wcs_acosd(sqrt(r2));
      } else if (r2 <= 1.0) {
         *theta = wcs_asind(sqrt(1.0 - r2));
      } else {
         return 2;
      }

   } else {
      /* "Synthesis" projection. */
      if (r2 < 1.0e-10) {
         /* Use small angle formula. */
         z = -r2/2.0;
         *theta = 90.0 - R2D*sqrt(r2/(1.0 - x0*prj->p[1] + y0*prj->p[2]));

      } else {
         sxy = 2.0*(prj->p[1]*x0 - prj->p[2]*y0);

         a = prj->w[3];
         b = -(sxy + prj->w[2]);
         c = r2 + sxy + prj->w[4];
         d = b*b - 2.0*a*c;

         /* Check for a solution. */
         if (d < 0.0) {
            return 2;
         }
         d = sqrt(d);

         /* Choose solution closest to pole. */
         sth1 = (-b + d)/a;
         sth2 = (-b - d)/a;
         sth = (sth1>sth2) ? sth1 : sth2;
         if (sth > 1.0) {
            if (sth-1.0 < tol) {
               sth = 1.0;
            } else {
               sth = (sth1<sth2) ? sth1 : sth2;
            }
         }
         if (sth > 1.0 || sth < -1.0) {
            return 2;
         }

         *theta = wcs_asind(sth);
         z = sth - 1.0;
      }

      xp = -y0 - prj->p[2]*z;
      yp =  x0 - prj->p[1]*z;
      if (xp == 0.0 && yp == 0.0) {
         *phi = 0.0;
      } else {
         *phi = wcs_atan2d(yp,xp);
      }
   }

   return 0;
}

/*============================================================================
*   STG: stereographic projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   2*r0
*      prj->w[1]   1/(2*r0)
*===========================================================================*/

int stgset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 360.0/PI;
      prj->w[1] = PI/360.0;
   } else {
      prj->w[0] = 2.0*prj->r0;
      prj->w[1] = 1.0/prj->w[0];
   }

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int stgfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double r, s;

   if (prj->flag != PRJSET) {
      if (stgset(prj)) return 1;
   }

   s = 1.0 + wcs_sind(theta);
   if (s == 0.0) {
      return 2;
   }

   r =  prj->w[0]*wcs_cosd(theta)/s;
   *x =  r*wcs_sind(phi);
   *y = -r*wcs_cosd(phi);

   return 0;
}

/*--------------------------------------------------------------------------*/

int stgrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double r;

   if (prj->flag != PRJSET) {
      if (stgset(prj)) return 1;
   }

   r = sqrt(x*x + y*y);
   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = wcs_atan2d(x, -y);
   }
   *theta = 90.0 - 2.0*wcs_atand(r*prj->w[1]);

   return 0;
}

/*============================================================================
*   ARC: zenithal/azimuthal equidistant projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/180)
*      prj->w[1]   (180/pi)/r0
*===========================================================================*/

int arcset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 1.0;
      prj->w[1] = 1.0;
   } else {
      prj->w[0] = prj->r0*D2R;
      prj->w[1] = 1.0/prj->w[0];
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int arcfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double r;

   if (prj->flag != PRJSET) {
      if (arcset(prj)) return 1;
   }

   r =  prj->w[0]*(90.0 - theta);
   *x =  r*wcs_sind(phi);
   *y = -r*wcs_cosd(phi);

   return 0;
}

/*--------------------------------------------------------------------------*/

int arcrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double r;

   if (prj->flag != PRJSET) {
      if (arcset(prj)) return 1;
   }

   r = sqrt(x*x + y*y);
   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = wcs_atan2d(x, -y);
   }
   *theta = 90.0 - r*prj->w[1];

   return 0;
}

/*============================================================================
*   ZPN: zenithal/azimuthal polynomial projection.
*
*   Given:
*      prj->p[0:99] Polynomial coefficients.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->n      Degree of the polynomial, N.
*      prj->w[0]   Co-latitude of the first point of inflection (N > 2).
*      prj->w[1]   Radius of the first point of inflection (N > 2).
*===========================================================================*/

int zpnset(prj)

struct prjprm *prj;

{
   int   i, j, k;
   double d, d1, d2, r, zd, zd1, zd2;
   const double tol = 1.0e-13;

   if (prj->r0 == 0.0) prj->r0 = R2D;

   /* Find the highest non-zero coefficient. */
   for (k = 99; k >= 0 && prj->p[k] == 0.0; k--);
   if (k < 0) return 1;

   prj->n = k;

   if (k >= 3) {
      /* Find the point of inflection closest to the pole. */
      zd1 = 0.0;
      d1  = prj->p[1];
      if (d1 <= 0.0) {
         return 1;
      }

      /* Find the point where the derivative first goes negative. */
      for (i = 0; i < 180; i++) {
         zd2 = i*D2R;
         d2  = 0.0;
         for (j = k; j > 0; j--) {
            d2 = d2*zd2 + j*prj->p[j];
         }

         if (d2 <= 0.0) break;
         zd1 = zd2;
         d1  = d2;
      }

      if (i == 180) {
         /* No negative derivative -> no point of inflection. */
         zd = PI;
      } else {
         /* Find where the derivative is zero. */
         for (i = 1; i <= 10; i++) {
            zd = zd1 - d1*(zd2-zd1)/(d2-d1);

            d = 0.0;
            for (j = k; j > 0; j--) {
               d = d*zd + j*prj->p[j];
            }

            if (fabs(d) < tol) break;

            if (d < 0.0) {
               zd2 = zd;
               d2  = d;
            } else {
               zd1 = zd;
               d1  = d;
            }
         }
      }

      r = 0.0;
      for (j = k; j >= 0; j--) {
         r = r*zd + prj->p[j];
      }
      prj->w[0] = zd;
      prj->w[1] = r;
   }

   if (prj->flag == -1) {
      prj->flag = -PRJSET;
   } else {
      prj->flag = PRJSET;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int zpnfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   int   j;
   double r, s;

   if (abs(prj->flag) != PRJSET) {
      if (zpnset(prj)) return 1;
   }

   s = (90.0 - theta)*D2R;
   r = 0.0;
   for (j = prj->n; j >= 0; j--) {
      r = r*s + prj->p[j];
   }
   r = prj->r0*r;

   *x =  r*wcs_sind(phi);
   *y = -r*wcs_cosd(phi);

   if (prj->flag == PRJSET && s > prj->w[0]) {
      return 2;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int zpnrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   int   i, j, k;
   double a, b, c, d, lambda, r, r1, r2, rt, zd, zd1, zd2;
   const double tol = 1.0e-13;

   if (abs(prj->flag) != PRJSET) {
      if (zpnset(prj)) return 1;
   }

   k = prj->n;

   r = sqrt(x*x + y*y)/prj->r0;

   if (k < 1) {
      /* Constant - no solution. */
      return 1;
   } else if (k == 1) {
      /* Linear. */
      zd = (r - prj->p[0])/prj->p[1];
   } else if (k == 2) {
      /* Quadratic. */
      a = prj->p[2];
      b = prj->p[1];
      c = prj->p[0] - r;

      d = b*b - 4.0*a*c;
      if (d < 0.0) {
         return 2;
      }
      d = sqrt(d);

      /* Choose solution closest to pole. */
      zd1 = (-b + d)/(2.0*a);
      zd2 = (-b - d)/(2.0*a);
      zd  = (zd1<zd2) ? zd1 : zd2;
      if (zd < -tol) zd = (zd1>zd2) ? zd1 : zd2;
      if (zd < 0.0) {
         if (zd < -tol) {
            return 2;
         }
         zd = 0.0;
      } else if (zd > PI) {
         if (zd > PI+tol) {
            return 2;
         }
         zd = PI;
      }
   } else {
      /* Higher order - solve iteratively. */
      zd1 = 0.0;
      r1  = prj->p[0];
      zd2 = prj->w[0];
      r2  = prj->w[1];

      if (r < r1) {
         if (r < r1-tol) {
            return 2;
         }
         zd = zd1;
      } else if (r > r2) {
         if (r > r2+tol) {
            return 2;
         }
         zd = zd2;
      } else {
         /* Disect the interval. */
         for (j = 0; j < 100; j++) {
            lambda = (r2 - r)/(r2 - r1);
            if (lambda < 0.1) {
               lambda = 0.1;
            } else if (lambda > 0.9) {
               lambda = 0.9;
            }

            zd = zd2 - lambda*(zd2 - zd1);

            rt = 0.0;
            for (i = k; i >= 0; i--) {
                rt = (rt * zd) + prj->p[i];
            }

            if (rt < r) {
                if (r-rt < tol) break;
                r1 = rt;
                zd1 = zd;
            } else {
                if (rt-r < tol) break;
                r2 = rt;
                zd2 = zd;
            }

            if (fabs(zd2-zd1) < tol) break;
         }
      }
   }

   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = wcs_atan2d(x, -y);
   }
   *theta = 90.0 - zd*R2D;

   return 0;
}

/*============================================================================
*   ZEA: zenithal/azimuthal equal area projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   2*r0
*      prj->w[1]   1/(2*r0)
*===========================================================================*/

int zeaset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 360.0/PI;
      prj->w[1] = PI/360.0;
   } else {
      prj->w[0] = 2.0*prj->r0;
      prj->w[1] = 1.0/prj->w[0];
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int zeafwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double r;

   if (prj->flag != PRJSET) {
      if (zeaset(prj)) return 1;
   }

   r =  prj->w[0]*wcs_sind((90.0 - theta)/2.0);
   *x =  r*wcs_sind(phi);
   *y = -r*wcs_cosd(phi);

   return 0;
}

/*--------------------------------------------------------------------------*/

int zearev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double r, s;
   const double tol = 1.0e-12;

   if (prj->flag != PRJSET) {
      if (zeaset(prj)) return 1;
   }

   r = sqrt(x*x + y*y);
   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = wcs_atan2d(x, -y);
   }

   s = r*prj->w[1];
   if (fabs(s) > 1.0) {
      if (fabs(r - prj->w[0]) < tol) {
         *theta = -90.0;
      } else {
         return 2;
      }
   } else {
      *theta = 90.0 - 2.0*wcs_asind(s);
   }

   return 0;
}

/*============================================================================
*   AIR: Airy's projection.
*
*   Given:
*      prj->p[1]   Latitude theta_b within which the error is minimized,
*                  in degrees.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   2*r0
*      prj->w[1]   ln(cos(xi_b))/tan(xi_b)**2, where xi_b = (90-theta_b)/2
*      prj->w[2]   1/2 - prj->w[1]
*      prj->w[3]   2*r0*prj->w[2]
*      prj->w[4]   tol, cutoff for using small angle approximation, in
*                  radians.
*      prj->w[5]   prj->w[2]*tol
*      prj->w[6]   (180/pi)/prj->w[2]
*===========================================================================*/

int airset(prj)

struct prjprm *prj;

{
   const double tol = 1.0e-4;
   double cxi;

   if (prj->r0 == 0.0) prj->r0 = R2D;

   prj->w[0] = 2.0*prj->r0;
   if (prj->p[1] == 90.0) {
      prj->w[1] = -0.5;
      prj->w[2] =  1.0;
   } else if (prj->p[1] > -90.0) {
      cxi = wcs_cosd((90.0 - prj->p[1])/2.0);
      prj->w[1] = log(cxi)*(cxi*cxi)/(1.0-cxi*cxi);
      prj->w[2] = 0.5 - prj->w[1];
   } else {
      return 1;
   }

   prj->w[3] = prj->w[0] * prj->w[2];
   prj->w[4] = tol;
   prj->w[5] = prj->w[2]*tol;
   prj->w[6] = R2D/prj->w[2];

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int airfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double cxi, r, txi, xi;

   if (prj->flag != PRJSET) {
      if (airset(prj)) return 1;
   }

   if (theta == 90.0) {
      r = 0.0;
   } else if (theta > -90.0) {
      xi = D2R*(90.0 - theta)/2.0;
      if (xi < prj->w[4]) {
         r = xi*prj->w[3];
      } else {
         cxi = wcs_cosd((90.0 - theta)/2.0);
         txi = sqrt(1.0-cxi*cxi)/cxi;
         r = -prj->w[0]*(log(cxi)/txi + prj->w[1]*txi);
      }
   } else {
      return 2;
   }

   *x =  r*wcs_sind(phi);
   *y = -r*wcs_cosd(phi);

   return 0;
}

/*--------------------------------------------------------------------------*/

int airrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   int   j;
   double cxi, lambda, r, r1, r2, rt, txi, x1, x2, xi;
   const double tol = 1.0e-12;

   if (prj->flag != PRJSET) {
      if (airset(prj)) return 1;
   }

   r = sqrt(x*x + y*y)/prj->w[0];

   if (r == 0.0) {
      xi = 0.0;
   } else if (r < prj->w[5]) {
      xi = r*prj->w[6];
   } else {
      /* Find a solution interval. */
      x1 = 1.0;
      r1 = 0.0;
      for (j = 0; j < 30; j++) {
         x2 = x1/2.0;
         txi = sqrt(1.0-x2*x2)/x2;
         r2 = -(log(x2)/txi + prj->w[1]*txi);

         if (r2 >= r) break;
         x1 = x2;
         r1 = r2;
      }
      if (j == 30) return 2;

      for (j = 0; j < 100; j++) {
         /* Weighted division of the interval. */
         lambda = (r2-r)/(r2-r1);
         if (lambda < 0.1) {
            lambda = 0.1;
         } else if (lambda > 0.9) {
            lambda = 0.9;
         }
         cxi = x2 - lambda*(x2-x1);

         txi = sqrt(1.0-cxi*cxi)/cxi;
         rt = -(log(cxi)/txi + prj->w[1]*txi);

         if (rt < r) {
             if (r-rt < tol) break;
             r1 = rt;
             x1 = cxi;
         } else {
             if (rt-r < tol) break;
             r2 = rt;
             x2 = cxi;
         }
      }
      if (j == 100) return 2;

      xi = wcs_acosd(cxi);
   }

   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = wcs_atan2d(x, -y);
   }
   *theta = 90.0 - 2.0*xi;

   return 0;
}

/*============================================================================
*   CYP: cylindrical perspective projection.
*
*   Given:
*      prj->p[1]   Distance of point of projection from the centre of the
*                  generating sphere, mu, in units of r0.
*      prj->p[2]   Radius of the cylinder of projection, lambda, in units
*                  of r0.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*lambda*(pi/180)
*      prj->w[1]   (180/pi)/(r0*lambda)
*      prj->w[2]   r0*(mu + lambda)
*      prj->w[3]   1/(r0*(mu + lambda))
*===========================================================================*/

int cypset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;

      prj->w[0] = prj->p[2];
      if (prj->w[0] == 0.0) {
         return 1;
      }

      prj->w[1] = 1.0/prj->w[0];

      prj->w[2] = R2D*(prj->p[1] + prj->p[2]);
      if (prj->w[2] == 0.0) {
         return 1;
      }

      prj->w[3] = 1.0/prj->w[2];
   } else {
      prj->w[0] = prj->r0*prj->p[2]*D2R;
      if (prj->w[0] == 0.0) {
         return 1;
      }

      prj->w[1] = 1.0/prj->w[0];

      prj->w[2] = prj->r0*(prj->p[1] + prj->p[2]);
      if (prj->w[2] == 0.0) {
         return 1;
      }

      prj->w[3] = 1.0/prj->w[2];
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int cypfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double s;

   if (prj->flag != PRJSET) {
      if (cypset(prj)) return 1;
   }

   s = prj->p[1] + wcs_cosd(theta);
   if (s == 0.0) {
         return 2;
      }

   *x = prj->w[0]*phi;
   *y = prj->w[2]*wcs_sind(theta)/s;

   return 0;
}

/*--------------------------------------------------------------------------*/

int cyprev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double eta;

   if (prj->flag != PRJSET) {
      if (cypset(prj)) return 1;
   }

   *phi   = x*prj->w[1];
   eta    = y*prj->w[3];
   *theta = wcs_atan2d(eta,1.0) + wcs_asind(eta*prj->p[1]/sqrt(eta*eta+1.0));

   return 0;
}

/*============================================================================
*   CAR: Cartesian projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/180)
*      prj->w[1]   (180/pi)/r0
*===========================================================================*/

int carset(prj)

struct prjprm *prj;

{

   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 1.0;
      prj->w[1] = 1.0;
   } else {
      prj->w[0] = prj->r0*D2R;
      prj->w[1] = 1.0/prj->w[0];
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int carfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   if (prj->flag != PRJSET) {
      if (carset(prj)) return 1;
   }

   *x = prj->w[0]*phi;
   *y = prj->w[0]*theta;

   return 0;
}

/*--------------------------------------------------------------------------*/

int carrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   if (prj->flag != PRJSET) {
      if (carset(prj)) return 1;
   }

   *phi   = prj->w[1]*x;
   *theta = prj->w[1]*y;

   return 0;
}

/*============================================================================
*   MER: Mercator's projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/180)
*      prj->w[1]   (180/pi)/r0
*===========================================================================*/

int merset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 1.0;
      prj->w[1] = 1.0;
   } else {
      prj->w[0] = prj->r0*D2R;
      prj->w[1] = 1.0/prj->w[0];
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int merfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   if (prj->flag != PRJSET) {
      if (merset(prj)) return 1;
   }

   if (theta <= -90.0 || theta >= 90.0) {
      return 2;
   }

   *x = prj->w[0]*phi;
   *y = prj->r0*log(wcs_tand((90.0+theta)/2.0));

   return 0;
}

/*--------------------------------------------------------------------------*/

int merrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   if (prj->flag != PRJSET) {
      if (merset(prj)) return 1;
   }

   *phi   = x*prj->w[1];
   *theta = 2.0*wcs_atand(exp(y/prj->r0)) - 90.0;

   return 0;
}

/*============================================================================
*   CEA: cylindrical equal area projection.
*
*   Given:
*      prj->p[1]   Square of the cosine of the latitude at which the
*                  projection is conformal, lambda.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/180)
*      prj->w[1]   (180/pi)/r0
*      prj->w[2]   r0/lambda
*      prj->w[3]   lambda/r0
*===========================================================================*/

int ceaset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 1.0;
      prj->w[1] = 1.0;
      if (prj->p[1] <= 0.0 || prj->p[1] > 1.0) {
         return 1;
      }
      prj->w[2] = prj->r0/prj->p[1];
      prj->w[3] = prj->p[1]/prj->r0;
   } else {
      prj->w[0] = prj->r0*D2R;
      prj->w[1] = R2D/prj->r0;
      if (prj->p[1] <= 0.0 || prj->p[1] > 1.0) {
         return 1;
      }
      prj->w[2] = prj->r0/prj->p[1];
      prj->w[3] = prj->p[1]/prj->r0;
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int ceafwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   if (prj->flag != PRJSET) {
      if (ceaset(prj)) return 1;
   }

   *x = prj->w[0]*phi;
   *y = prj->w[2]*wcs_sind(theta);

   return 0;
}

/*--------------------------------------------------------------------------*/

int cearev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double s;
   const double tol = 1.0e-13;

   if (prj->flag != PRJSET) {
      if (ceaset(prj)) return 1;
   }

   s = y*prj->w[3];
   if (fabs(s) > 1.0) {
      if (fabs(s) > 1.0+tol) {
         return 2;
      }
      s = copysign(1.0,s);
   }

   *phi   = x*prj->w[1];
   *theta = wcs_asind(s);

   return 0;
}

/*============================================================================
*   COP: conic perspective projection.
*
*   Given:
*      prj->p[1]   sigma = (theta2+theta1)/2
*      prj->p[2]   delta = (theta2-theta1)/2, where theta1 and theta2 are the
*                  latitudes of the standard parallels, in degrees.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   C  = sin(sigma)
*      prj->w[1]   1/C
*      prj->w[2]   Y0 = r0*cos(delta)*cot(sigma)
*      prj->w[3]   r0*cos(delta)
*      prj->w[4]   1/(r0*cos(delta)
*      prj->w[5]   cot(sigma)
*===========================================================================*/

int copset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) prj->r0 = R2D;

   prj->w[0] = wcs_sind(prj->p[1]);
   if (prj->w[0] == 0.0) {
      return 1;
   }

   prj->w[1] = 1.0/prj->w[0];

   prj->w[3] = prj->r0*wcs_cosd(prj->p[2]);
   if (prj->w[3] == 0.0) {
      return 1;
   }

   prj->w[4] = 1.0/prj->w[3];
   prj->w[5] = 1.0/wcs_tand(prj->p[1]);

   prj->w[2] = prj->w[3]*prj->w[5];

   if (prj->flag == -1) {
      prj->flag = -PRJSET;
   } else {
      prj->flag = PRJSET;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int copfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double a, r, s, t;

   if (abs(prj->flag) != PRJSET) {
      if (copset(prj)) return 1;
   }

   t = theta - prj->p[1];
   s = wcs_cosd(t);
   if (s == 0.0) {
      return 2;
   }

   a = prj->w[0]*phi;
   r = prj->w[2] - prj->w[3]*wcs_sind(t)/s;

   *x =             r*wcs_sind(a);
   *y = prj->w[2] - r*wcs_cosd(a);

   if (prj->flag == PRJSET && r*prj->w[0] < 0.0) {
      return 2;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int coprev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double a, dy, r;

   if (abs(prj->flag) != PRJSET) {
      if (copset(prj)) return 1;
   }

   dy = prj->w[2] - y;
   r  = sqrt(x*x + dy*dy);
   if (prj->p[1] < 0.0) r = -r;

   if (r == 0.0) {
      a = 0.0;
   } else {
      a = wcs_atan2d(x/r, dy/r);
   }

   *phi   = a*prj->w[1];
   *theta = prj->p[1] + wcs_atand(prj->w[5] - r*prj->w[4]);

   return 0;
}

/*============================================================================
*   COD: conic equidistant projection.
*
*   Given:
*      prj->p[1]   sigma = (theta2+theta1)/2
*      prj->p[2]   delta = (theta2-theta1)/2, where theta1 and theta2 are the
*                  latitudes of the standard parallels, in degrees.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   C = r0*sin(sigma)*sin(delta)/delta
*      prj->w[1]   1/C
*      prj->w[2]   Y0 = delta*cot(delta)*cot(sigma)
*      prj->w[3]   Y0 + sigma
*===========================================================================*/

int codset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) prj->r0 = R2D;

   if (prj->p[2] == 0.0) {
      prj->w[0] = prj->r0*wcs_sind(prj->p[1])*D2R;
   } else {
      prj->w[0] = prj->r0*wcs_sind(prj->p[1])*wcs_sind(prj->p[2])/prj->p[2];
   }

   if (prj->w[0] == 0.0) {
      return 1;
   }

   prj->w[1] = 1.0/prj->w[0];
   prj->w[2] = prj->r0*wcs_cosd(prj->p[2])*wcs_cosd(prj->p[1])/prj->w[0];
   prj->w[3] = prj->w[2] + prj->p[1];

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int codfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double a, r;

   if (prj->flag != PRJSET) {
      if (codset(prj)) return 1;
   }

   a = prj->w[0]*phi;
   r = prj->w[3] - theta;

   *x =             r*wcs_sind(a);
   *y = prj->w[2] - r*wcs_cosd(a);

   return 0;
}

/*--------------------------------------------------------------------------*/

int codrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double a, dy, r;

   if (prj->flag != PRJSET) {
      if (codset(prj)) return 1;
   }

   dy = prj->w[2] - y;
   r  = sqrt(x*x + dy*dy);
   if (prj->p[1] < 0.0) r = -r;

   if (r == 0.0) {
      a = 0.0;
   } else {
      a = wcs_atan2d(x/r, dy/r);
   }

   *phi   = a*prj->w[1];
   *theta = prj->w[3] - r;

   return 0;
}

/*============================================================================
*   COE: conic equal area projection.
*
*   Given:
*      prj->p[1]   sigma = (theta2+theta1)/2
*      prj->p[2]   delta = (theta2-theta1)/2, where theta1 and theta2 are the
*                  latitudes of the standard parallels, in degrees.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   C = (sin(theta1) + sin(theta2))/2
*      prj->w[1]   1/C
*      prj->w[2]   Y0 = chi*sqrt(psi - 2C*wcs_sind(sigma))
*      prj->w[3]   chi = r0/C
*      prj->w[4]   psi = 1 + sin(theta1)*sin(theta2)
*      prj->w[5]   2C
*      prj->w[6]   (1 + sin(theta1)*sin(theta2))*(r0/C)**2
*      prj->w[7]   C/(2*r0**2)
*      prj->w[8]   chi*sqrt(psi + 2C)
*===========================================================================*/

int coeset(prj)

struct prjprm *prj;

{
   double theta1, theta2;

   if (prj->r0 == 0.0) prj->r0 = R2D;

   theta1 = prj->p[1] - prj->p[2];
   theta2 = prj->p[1] + prj->p[2];

   prj->w[0] = (wcs_sind(theta1) + wcs_sind(theta2))/2.0;
   if (prj->w[0] == 0.0) {
      return 1;
   }

   prj->w[1] = 1.0/prj->w[0];

   prj->w[3] = prj->r0/prj->w[0];
   prj->w[4] = 1.0 + wcs_sind(theta1)*wcs_sind(theta2);
   prj->w[5] = 2.0*prj->w[0];
   prj->w[6] = prj->w[3]*prj->w[3]*prj->w[4];
   prj->w[7] = 1.0/(2.0*prj->r0*prj->w[3]);
   prj->w[8] = prj->w[3]*sqrt(prj->w[4] + prj->w[5]);

   prj->w[2] = prj->w[3]*sqrt(prj->w[4] - prj->w[5]*wcs_sind(prj->p[1]));

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int coefwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double a, r;

   if (prj->flag != PRJSET) {
      if (coeset(prj)) return 1;
   }

   a = phi*prj->w[0];
   if (theta == -90.0) {
      r = prj->w[8];
   } else {
      r = prj->w[3]*sqrt(prj->w[4] - prj->w[5]*wcs_sind(theta));
   }

   *x =             r*wcs_sind(a);
   *y = prj->w[2] - r*wcs_cosd(a);

   return 0;
}

/*--------------------------------------------------------------------------*/

int coerev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double a, dy, r, w;
   const double tol = 1.0e-12;

   if (prj->flag != PRJSET) {
      if (coeset(prj)) return 1;
   }

   dy = prj->w[2] - y;
   r  = sqrt(x*x + dy*dy);
   if (prj->p[1] < 0.0) r = -r;

   if (r == 0.0) {
      a = 0.0;
   } else {
      a = wcs_atan2d(x/r, dy/r);
   }

   *phi = a*prj->w[1];
   if (fabs(r - prj->w[8]) < tol) {
      *theta = -90.0;
   } else {
      w = (prj->w[6] - r*r)*prj->w[7];
      if (fabs(w) > 1.0) {
         if (fabs(w-1.0) < tol) {
            *theta = 90.0;
         } else if (fabs(w+1.0) < tol) {
            *theta = -90.0;
         } else {
            return 2;
         }
      } else {
         *theta = wcs_asind(w);
      }
   }

   return 0;
}

/*============================================================================
*   COO: conic orthomorphic projection.
*
*   Given:
*      prj->p[1]   sigma = (theta2+theta1)/2
*      prj->p[2]   delta = (theta2-theta1)/2, where theta1 and theta2 are the
*                  latitudes of the standard parallels, in degrees.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   C = ln(cos(theta2)/cos(theta1))/ln(tan(tau2)/tan(tau1))
*                      where tau1 = (90 - theta1)/2
*                            tau2 = (90 - theta2)/2
*      prj->w[1]   1/C
*      prj->w[2]   Y0 = psi*tan((90-sigma)/2)**C
*      prj->w[3]   psi = (r0*cos(theta1)/C)/tan(tau1)**C
*      prj->w[4]   1/psi
*===========================================================================*/

int cooset(prj)

struct prjprm *prj;

{
   double cos1, cos2, tan1, tan2, theta1, theta2;

   if (prj->r0 == 0.0) prj->r0 = R2D;

   theta1 = prj->p[1] - prj->p[2];
   theta2 = prj->p[1] + prj->p[2];

   tan1 = wcs_tand((90.0 - theta1)/2.0);
   cos1 = wcs_cosd(theta1);

   if (theta1 == theta2) {
      prj->w[0] = wcs_sind(theta1);
   } else {
      tan2 = wcs_tand((90.0 - theta2)/2.0);
      cos2 = wcs_cosd(theta2);
      prj->w[0] = log(cos2/cos1)/log(tan2/tan1);
   }
   if (prj->w[0] == 0.0) {
      return 1;
   }

   prj->w[1] = 1.0/prj->w[0];

   prj->w[3] = prj->r0*(cos1/prj->w[0])/pow(tan1,prj->w[0]);
   if (prj->w[3] == 0.0) {
      return 1;
   }
   prj->w[2] = prj->w[3]*pow(wcs_tand((90.0 - prj->p[1])/2.0),prj->w[0]);
   prj->w[4] = 1.0/prj->w[3];

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int coofwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double a, r;

   if (prj->flag != PRJSET) {
      if (cooset(prj)) return 1;
   }

   a = prj->w[0]*phi;
   if (theta == -90.0) {
      if (prj->w[0] < 0.0) {
         r = 0.0;
      } else {
         return 2;
      }
   } else {
      r = prj->w[3]*pow(wcs_tand((90.0 - theta)/2.0),prj->w[0]);
   }

   *x =             r*wcs_sind(a);
   *y = prj->w[2] - r*wcs_cosd(a);

   return 0;
}

/*--------------------------------------------------------------------------*/

int coorev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double a, dy, r;

   if (prj->flag != PRJSET) {
      if (cooset(prj)) return 1;
   }

   dy = prj->w[2] - y;
   r  = sqrt(x*x + dy*dy);
   if (prj->p[1] < 0.0) r = -r;

   if (r == 0.0) {
      a = 0.0;
   } else {
      a = wcs_atan2d(x/r, dy/r);
   }

   *phi = a*prj->w[1];
   if (r == 0.0) {
      if (prj->w[0] < 0.0) {
         *theta = -90.0;
      } else {
         return 2;
      }
   } else {
      *theta = 90.0 - 2.0*wcs_atand(pow(r*prj->w[4],prj->w[1]));
   }

   return 0;
}

/*============================================================================
*   BON: Bonne's projection.
*
*   Given:
*      prj->p[1]   Bonne conformal latitude, theta1, in degrees.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[1]   r0*pi/180
*      prj->w[2]   Y0 = r0*cot(theta1) + theta1*pi/180)
*===========================================================================*/

int bonset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[1] = 1.0;
      prj->w[2] = prj->r0*wcs_cosd(prj->p[1])/wcs_sind(prj->p[1]) + prj->p[1];
   } else {
      prj->w[1] = prj->r0*D2R;
      prj->w[2] = prj->r0*(wcs_cosd(prj->p[1])/wcs_sind(prj->p[1]) + prj->p[1]*D2R);
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int bonfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double a, r;

   if (prj->p[1] == 0.0) {
      /* Sanson-Flamsteed. */
      return glsfwd(phi, theta, prj, x, y);
   }

   if (prj->flag != PRJSET) {
      if (bonset(prj)) return 1;
   }

   r = prj->w[2] - theta*prj->w[1];
   a = prj->r0*phi*wcs_cosd(theta)/r;

   *x =             r*wcs_sind(a);
   *y = prj->w[2] - r*wcs_cosd(a);

   return 0;
}

/*--------------------------------------------------------------------------*/

int bonrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double a, dy, costhe, r;

   if (prj->p[1] == 0.0) {
      /* Sanson-Flamsteed. */
      return glsrev(x, y, prj, phi, theta);
   }

   if (prj->flag != PRJSET) {
      if (bonset(prj)) return 1;
   }

   dy = prj->w[2] - y;
   r = sqrt(x*x + dy*dy);
   if (prj->p[1] < 0.0) r = -r;

   if (r == 0.0) {
      a = 0.0;
   } else {
      a = wcs_atan2d(x/r, dy/r);
   }

   *theta = (prj->w[2] - r)/prj->w[1];
   costhe = wcs_cosd(*theta);
   if (costhe == 0.0) {
      *phi = 0.0;
   } else {
      *phi = a*(r/prj->r0)/wcs_cosd(*theta);
   }

   return 0;
}

/*============================================================================
*   PCO: polyconic projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/180)
*      prj->w[1]   1/r0
*      prj->w[2]   2*r0
*===========================================================================*/

int pcoset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 1.0;
      prj->w[1] = 1.0;
      prj->w[2] = 360.0/PI;
   } else {
      prj->w[0] = prj->r0*D2R;
      prj->w[1] = 1.0/prj->w[0];
      prj->w[2] = 2.0*prj->r0;
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int pcofwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double a, costhe, cotthe, sinthe;

   if (prj->flag != PRJSET) {
      if (pcoset(prj)) return 1;
   }

   costhe = wcs_cosd(theta);
   sinthe = wcs_sind(theta);
   a = phi*sinthe;

   if (sinthe == 0.0) {
      *x = prj->w[0]*phi;
      *y = 0.0;
   } else {
      cotthe = costhe/sinthe;
      *x = prj->r0*cotthe*wcs_sind(a);
      *y = prj->r0*(cotthe*(1.0 - wcs_cosd(a)) + theta*D2R);
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int pcorev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   int   j;
   double f, fneg, fpos, lambda, tanthe, theneg, thepos, w, xp, xx, ymthe, yp;
   const double tol = 1.0e-12;

   if (prj->flag != PRJSET) {
      if (pcoset(prj)) return 1;
   }

   w = fabs(y*prj->w[1]);
   if (w < tol) {
      *phi = x*prj->w[1];
      *theta = 0.0;
   } else if (fabs(w-90.0) < tol) {
      *phi = 0.0;
      *theta = wcs_copysign(90.0,y);
   } else {
      /* Iterative solution using weighted division of the interval. */
      if (y > 0.0) {
         thepos =  90.0;
      } else {
         thepos = -90.0;
      }
      theneg = 0.0;

      xx = x*x;
      ymthe = y - prj->w[0]*thepos;
      fpos = xx + ymthe*ymthe;
      fneg = -999.0;

      for (j = 0; j < 64; j++) {
         if (fneg < -100.0) {
            /* Equal division of the interval. */
            *theta = (thepos+theneg)/2.0;
         } else {
            /* Weighted division of the interval. */
            lambda = fpos/(fpos-fneg);
            if (lambda < 0.1) {
               lambda = 0.1;
            } else if (lambda > 0.9) {
               lambda = 0.9;
            }
            *theta = thepos - lambda*(thepos-theneg);
         }

         /* Compute the residue. */
         ymthe = y - prj->w[0]*(*theta);
         tanthe = wcs_tand(*theta);
         f = xx + ymthe*(ymthe - prj->w[2]/tanthe);

         /* Check for convergence. */
         if (fabs(f) < tol) break;
         if (fabs(thepos-theneg) < tol) break;

         /* Redefine the interval. */
         if (f > 0.0) {
            thepos = *theta;
            fpos = f;
         } else {
            theneg = *theta;
            fneg = f;
         }
      }

      xp = prj->r0 - ymthe*tanthe;
      yp = x*tanthe;
      if (xp == 0.0 && yp == 0.0) {
         *phi = 0.0;
      } else {
         *phi = wcs_atan2d(yp, xp)/wcs_sind(*theta);
      }
   }

   return 0;
}

/*============================================================================
*   GLS: Sanson-Flamsteed ("global sinusoid") projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/180)
*      prj->w[1]   (180/pi)/r0
*===========================================================================*/

int glsset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 1.0;
      prj->w[1] = 1.0;
   } else {
      prj->w[0] = prj->r0*D2R;
      prj->w[1] = 1.0/prj->w[0];
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int glsfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   if (prj->flag != PRJSET) {
      if (glsset(prj)) return 1;
   }

   *x = prj->w[0]*phi*wcs_cosd(theta);
   *y = prj->w[0]*theta;

   return 0;
}

/*--------------------------------------------------------------------------*/

int glsrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double w;

   if (prj->flag != PRJSET) {
      if (glsset(prj)) return 1;
   }

   w = cos(y/prj->r0);
   if (w == 0.0) {
      *phi = 0.0;
   } else {
      *phi = x*prj->w[1]/cos(y/prj->r0);
   }
   *theta = y*prj->w[1];

   return 0;
}

/*============================================================================
*   PAR: parabolic projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/180)
*      prj->w[1]   (180/pi)/r0
*      prj->w[2]   pi*r0
*      prj->w[3]   1/(pi*r0)
*===========================================================================*/

int parset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 1.0;
      prj->w[1] = 1.0;
      prj->w[2] = 180.0;
      prj->w[3] = 1.0/prj->w[2];
   } else {
      prj->w[0] = prj->r0*D2R;
      prj->w[1] = 1.0/prj->w[0];
      prj->w[2] = PI*prj->r0;
      prj->w[3] = 1.0/prj->w[2];
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int parfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double s;

   if (prj->flag != PRJSET) {
      if (parset(prj)) return 1;
   }

   s = wcs_sind(theta/3.0);
   *x = prj->w[0]*phi*(1.0 - 4.0*s*s);
   *y = prj->w[2]*s;

   return 0;
}

/*--------------------------------------------------------------------------*/

int parrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double s, t;

   if (prj->flag != PRJSET) {
      if (parset(prj)) return 1;
   }

   s = y*prj->w[3];
   if (s > 1.0 || s < -1.0) {
      return 2;
   }

   t = 1.0 - 4.0*s*s;
   if (t == 0.0) {
      if (x == 0.0) {
         *phi = 0.0;
      } else {
         return 2;
      }
   } else {
      *phi = prj->w[1]*x/t;
   }

   *theta = 3.0*wcs_asind(s);

   return 0;
}

/*============================================================================
*   AIT: Hammer-Aitoff projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   2*r0**2
*      prj->w[1]   1/(2*r0)**2
*      prj->w[2]   1/(4*r0)**2
*      prj->w[3]   1/(2*r0)
*===========================================================================*/

int aitset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) prj->r0 = R2D;

   prj->w[0] = 2.0*prj->r0*prj->r0;
   prj->w[1] = 1.0/(2.0*prj->w[0]);
   prj->w[2] = prj->w[1]/4.0;
   prj->w[3] = 1.0/(2.0*prj->r0);

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int aitfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double costhe, w;

   if (prj->flag != PRJSET) {
      if (aitset(prj)) return 1;
   }

   costhe = wcs_cosd(theta);
   w = sqrt(prj->w[0]/(1.0 + costhe*wcs_cosd(phi/2.0)));
   *x = 2.0*w*costhe*wcs_sind(phi/2.0);
   *y = w*wcs_sind(theta);

   return 0;
}

/*--------------------------------------------------------------------------*/

int aitrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double s, u, xp, yp, z;
   const double tol = 1.0e-13;

   if (prj->flag != PRJSET) {
      if (aitset(prj)) return 1;
   }

   u = 1.0 - x*x*prj->w[2] - y*y*prj->w[1];
   if (u < 0.0) {
      if (u < -tol) {
         return 2;
      }

      u = 0.0;
   }

   z = sqrt(u);
   s = z*y/prj->r0;
   if (fabs(s) > 1.0) {
      if (fabs(s) > 1.0+tol) {
         return 2;
      }
      s = wcs_copysign(1.0,s);
   }

   xp = 2.0*z*z - 1.0;
   yp = z*x*prj->w[3];
   if (xp == 0.0 && yp == 0.0) {
      *phi = 0.0;
   } else {
      *phi = 2.0*wcs_atan2d(yp, xp);
   }
   *theta = wcs_asind(s);

   return 0;
}

/*============================================================================
*   MOL: Mollweide's projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   sqrt(2)*r0
*      prj->w[1]   sqrt(2)*r0/90
*      prj->w[2]   1/(sqrt(2)*r0)
*      prj->w[3]   90/r0
*===========================================================================*/

int molset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) prj->r0 = R2D;

   prj->w[0] = SQRT2*prj->r0;
   prj->w[1] = prj->w[0]/90.0;
   prj->w[2] = 1.0/prj->w[0];
   prj->w[3] = 90.0/prj->r0;
   prj->w[4] = 2.0/PI;

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int molfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   int   j;
   double alpha, resid, u, v, v0, v1;
   const double tol = 1.0e-13;

   if (prj->flag != PRJSET) {
      if (molset(prj)) return 1;
   }

   if (fabs(theta) == 90.0) {
      *x = 0.0;
      *y = wcs_copysign(prj->w[0],theta);
   } else if (theta == 0.0) {
      *x = prj->w[1]*phi;
      *y = 0.0;
   } else {
      u  = PI*wcs_sind(theta);
      v0 = -PI;
      v1 =  PI;
      v  = u;
      for (j = 0; j < 100; j++) {
         resid = (v - u) + sin(v);
         if (resid < 0.0) {
            if (resid > -tol) break;
            v0 = v;
         } else {
            if (resid < tol) break;
            v1 = v;
         }
         v = (v0 + v1)/2.0;
      }

      alpha = v/2.0;
      *x = prj->w[1]*phi*cos(alpha);
      *y = prj->w[0]*sin(alpha);
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int molrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double s, y0, z;
   const double tol = 1.0e-12;

   if (prj->flag != PRJSET) {
      if (molset(prj)) return 1;
   }

   y0 = y/prj->r0;
   s  = 2.0 - y0*y0;
   if (s <= tol) {
      if (s < -tol) {
         return 2;
      }
      s = 0.0;

      if (fabs(x) > tol) {
         return 2;
      }
      *phi = 0.0;
   } else {
      s = sqrt(s);
      *phi = prj->w[3]*x/s;
   }

   z = y*prj->w[2];
   if (fabs(z) > 1.0) {
      if (fabs(z) > 1.0+tol) {
         return 2;
      }
      z = wcs_copysign(1.0,z) + y0*s/PI;
   } else {
      z = asin(z)*prj->w[4] + y0*s/PI;
   }

   if (fabs(z) > 1.0) {
      if (fabs(z) > 1.0+tol) {
         return 2;
      }
      z = wcs_copysign(1.0,z);
   }

   *theta = wcs_asind(z);

   return 0;
}

/*============================================================================
*   CSC: COBE quadrilateralized spherical cube projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/4)
*      prj->w[1]   (4/pi)/r0
*===========================================================================*/

int cscset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 45.0;
      prj->w[1] = 1.0/45.0;
   } else {
      prj->w[0] = prj->r0*PI/4.0;
      prj->w[1] = 1.0/prj->w[0];
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int cscfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   int   face;
   double costhe, eta, l, m, n, rho, xi;
   const float tol = 1.0e-7;

   float a, a2, a2b2, a4, ab, b, b2, b4, ca2, cb2, x0, xf, y0, yf;
   const float gstar  =  1.37484847732;
   const float mm     =  0.004869491981;
   const float gamma  = -0.13161671474;
   const float omega1 = -0.159596235474;
   const float d0  =  0.0759196200467;
   const float d1  = -0.0217762490699;
   const float c00 =  0.141189631152;
   const float c10 =  0.0809701286525;
   const float c01 = -0.281528535557;
   const float c11 =  0.15384112876;
   const float c20 = -0.178251207466;
   const float c02 =  0.106959469314;

   if (prj->flag != PRJSET) {
      if (cscset(prj)) return 1;
   }

   costhe = wcs_cosd(theta);
   l = costhe*wcs_cosd(phi);
   m = costhe*wcs_sind(phi);
   n = wcs_sind(theta);

   face = 0;
   rho  = n;
   if (l > rho) {
      face = 1;
      rho  = l;
   }
   if (m > rho) {
      face = 2;
      rho  = m;
   }
   if (-l > rho) {
      face = 3;
      rho  = -l;
   }
   if (-m > rho) {
      face = 4;
      rho  = -m;
   }
   if (-n > rho) {
      face = 5;
      rho  = -n;
   }

   if (face == 0) {
      xi  =  m;
      eta = -l;
      x0  =  0.0;
      y0  =  2.0;
   } else if (face == 1) {
      xi  =  m;
      eta =  n;
      x0  =  0.0;
      y0  =  0.0;
   } else if (face == 2) {
      xi  = -l;
      eta =  n;
      x0  =  2.0;
      y0  =  0.0;
   } else if (face == 3) {
      xi  = -m;
      eta =  n;
      x0  =  4.0;
      y0  =  0.0;
   } else if (face == 4) {
      xi  =  l;
      eta =  n;
      x0  =  6.0;
      y0  =  0.0;
   } else if (face == 5) {
      xi  =  m;
      eta =  l;
      x0  =  0.0;
      y0  = -2.0;
   }

   a =  xi/rho;
   b = eta/rho;

   a2 = a*a;
   b2 = b*b;
   ca2 = 1.0 - a2;
   cb2 = 1.0 - b2;

   /* Avoid floating underflows. */
   ab   = fabs(a*b);
   a4   = (a2 > 1.0e-16) ? a2*a2 : 0.0;
   b4   = (b2 > 1.0e-16) ? b2*b2 : 0.0;
   a2b2 = (ab > 1.0e-16) ? a2*b2 : 0.0;

   xf = a*(a2 + ca2*(gstar + b2*(gamma*ca2 + mm*a2 +
          cb2*(c00 + c10*a2 + c01*b2 + c11*a2b2 + c20*a4 + c02*b4)) +
          a2*(omega1 - ca2*(d0 + d1*a2))));
   yf = b*(b2 + cb2*(gstar + a2*(gamma*cb2 + mm*b2 +
          ca2*(c00 + c10*b2 + c01*a2 + c11*a2b2 + c20*b4 + c02*a4)) +
          b2*(omega1 - cb2*(d0 + d1*b2))));

   if (fabs(xf) > 1.0) {
      if (fabs(xf) > 1.0+tol) {
         return 2;
      }
      xf = wcs_copysign(1.0,xf);
   }
   if (fabs(yf) > 1.0) {
      if (fabs(yf) > 1.0+tol) {
         return 2;
      }
      yf = wcs_copysign(1.0,yf);
   }

   *x = prj->w[0]*(x0 + xf);
   *y = prj->w[0]*(y0 + yf);

   return 0;
}

/*--------------------------------------------------------------------------*/

int cscrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   int   face;
   double l, m, n;

   float     a, b, xf, xx, yf, yy, z0, z1, z2, z3, z4, z5, z6;
   const float p00 = -0.27292696;
   const float p10 = -0.07629969;
   const float p20 = -0.22797056;
   const float p30 =  0.54852384;
   const float p40 = -0.62930065;
   const float p50 =  0.25795794;
   const float p60 =  0.02584375;
   const float p01 = -0.02819452;
   const float p11 = -0.01471565;
   const float p21 =  0.48051509;
   const float p31 = -1.74114454;
   const float p41 =  1.71547508;
   const float p51 = -0.53022337;
   const float p02 =  0.27058160;
   const float p12 = -0.56800938;
   const float p22 =  0.30803317;
   const float p32 =  0.98938102;
   const float p42 = -0.83180469;
   const float p03 = -0.60441560;
   const float p13 =  1.50880086;
   const float p23 = -0.93678576;
   const float p33 =  0.08693841;
   const float p04 =  0.93412077;
   const float p14 = -1.41601920;
   const float p24 =  0.33887446;
   const float p05 = -0.63915306;
   const float p15 =  0.52032238;
   const float p06 =  0.14381585;

   if (prj->flag != PRJSET) {
      if (cscset(prj)) return 1;
   }

   xf = x*prj->w[1];
   yf = y*prj->w[1];

   /* Check bounds. */
   if (fabs(xf) <= 1.0) {
      if (fabs(yf) > 3.0) return 2;
   } else {
      if (fabs(xf) > 7.0) return 2;
      if (fabs(yf) > 1.0) return 2;
   }

   /* Map negative faces to the other side. */
   if (xf < -1.0) xf += 8.0;

   /* Determine the face. */
   if (xf > 5.0) {
      face = 4;
      xf = xf - 6.0;
   } else if (xf > 3.0) {
      face = 3;
      xf = xf - 4.0;
   } else if (xf > 1.0) {
      face = 2;
      xf = xf - 2.0;
   } else if (yf > 1.0) {
      face = 0;
      yf = yf - 2.0;
   } else if (yf < -1.0) {
      face = 5;
      yf = yf + 2.0;
   } else {
      face = 1;
   }

   xx  =  xf*xf;
   yy  =  yf*yf;

   z0 = p00 + xx*(p10 + xx*(p20 + xx*(p30 + xx*(p40 + xx*(p50 + xx*(p60))))));
   z1 = p01 + xx*(p11 + xx*(p21 + xx*(p31 + xx*(p41 + xx*(p51)))));
   z2 = p02 + xx*(p12 + xx*(p22 + xx*(p32 + xx*(p42))));
   z3 = p03 + xx*(p13 + xx*(p23 + xx*(p33)));
   z4 = p04 + xx*(p14 + xx*(p24));
   z5 = p05 + xx*(p15);
   z6 = p06;

   a = z0 + yy*(z1 + yy*(z2 + yy*(z3 + yy*(z4 + yy*(z5 + yy*z6)))));
   a = xf + xf*(1.0 - xx)*a;

   z0 = p00 + yy*(p10 + yy*(p20 + yy*(p30 + yy*(p40 + yy*(p50 + yy*(p60))))));
   z1 = p01 + yy*(p11 + yy*(p21 + yy*(p31 + yy*(p41 + yy*(p51)))));
   z2 = p02 + yy*(p12 + yy*(p22 + yy*(p32 + yy*(p42))));
   z3 = p03 + yy*(p13 + yy*(p23 + yy*(p33)));
   z4 = p04 + yy*(p14 + yy*(p24));
   z5 = p05 + yy*(p15);
   z6 = p06;

   b = z0 + xx*(z1 + xx*(z2 + xx*(z3 + xx*(z4 + xx*(z5 + xx*z6)))));
   b = yf + yf*(1.0 - yy)*b;

   if (face == 0) {
      n =  1.0/sqrt(a*a + b*b + 1.0);
      l = -b*n;
      m =  a*n;
   } else if (face == 1) {
      l =  1.0/sqrt(a*a + b*b + 1.0);
      m =  a*l;
      n =  b*l;
   } else if (face == 2) {
      m =  1.0/sqrt(a*a + b*b + 1.0);
      l = -a*m;
      n =  b*m;
   } else if (face == 3) {
      l = -1.0/sqrt(a*a + b*b + 1.0);
      m =  a*l;
      n = -b*l;
   } else if (face == 4) {
      m = -1.0/sqrt(a*a + b*b + 1.0);
      l = -a*m;
      n = -b*m;
   } else if (face == 5) {
      n = -1.0/sqrt(a*a + b*b + 1.0);
      l = -b*n;
      m = -a*n;
   }

   if (l == 0.0 && m == 0.0) {
      *phi = 0.0;
   } else {
      *phi = wcs_atan2d(m, l);
   }
   *theta = wcs_asind(n);

   return 0;
}

/*============================================================================
*   QSC: quadrilaterilized spherical cube projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/4)
*      prj->w[1]   (4/pi)/r0
*===========================================================================*/

int qscset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 45.0;
      prj->w[1] = 1.0/45.0;
   } else {
      prj->w[0] = prj->r0*PI/4.0;
      prj->w[1] = 1.0/prj->w[0];
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int qscfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   int   face;
   double chi, costhe, eta, l, m, n, p, psi, rho, rhu, t, x0, xf, xi, y0, yf;
   const double tol = 1.0e-12;

   if (prj->flag != PRJSET) {
      if (qscset(prj)) return 1;
   }

   if (fabs(theta) == 90.0) {
      *x = 0.0;
      *y = wcs_copysign(2.0*prj->w[0],theta);
      return 0;
   }

   costhe = wcs_cosd(theta);
   l = costhe*wcs_cosd(phi);
   m = costhe*wcs_sind(phi);
   n = wcs_sind(theta);

   face = 0;
   rho  = n;
   if (l > rho) {
      face = 1;
      rho  = l;
   }
   if (m > rho) {
      face = 2;
      rho  = m;
   }
   if (-l > rho) {
      face = 3;
      rho  = -l;
   }
   if (-m > rho) {
      face = 4;
      rho  = -m;
   }
   if (-n > rho) {
      face = 5;
      rho  = -n;
   }

   rhu = 1.0 - rho;

   if (face == 0) {
      xi  =  m;
      eta = -l;
      if (rhu < 1.0e-8) {
         /* Small angle formula. */
         t = (90.0 - theta)*D2R;
         rhu = t*t/2.0;
      }
      x0  =  0.0;
      y0  =  2.0;
   } else if (face == 1) {
      xi  =  m;
      eta =  n;
      if (rhu < 1.0e-8) {
         /* Small angle formula. */
         t = theta*D2R;
         p = fmod(phi,360.0);
         if (p < -180.0) p += 360.0;
         if (p >  180.0) p -= 360.0;
         p *= D2R;
         rhu = (p*p + t*t)/2.0;
      }
      x0  =  0.0;
      y0  =  0.0;
   } else if (face == 2) {
      xi  = -l;
      eta =  n;
      if (rhu < 1.0e-8) {
         /* Small angle formula. */
         t = theta*D2R;
         p = fmod(phi,360.0);
         if (p < -180.0) p += 360.0;
         p = (90.0 - p)*D2R;
         rhu = (p*p + t*t)/2.0;
      }
      x0  =  2.0;
      y0  =  0.0;
   } else if (face == 3) {
      xi  = -m;
      eta =  n;
      if (rhu < 1.0e-8) {
         /* Small angle formula. */
         t = theta*D2R;
         p = fmod(phi,360.0);
         if (p < 0.0) p += 360.0;
         p = (180.0 - p)*D2R;
         rhu = (p*p + t*t)/2.0;
      }
      x0  =  4.0;
      y0  =  0.0;
   } else if (face == 4) {
      xi  =  l;
      eta =  n;
      if (rhu < 1.0e-8) {
         /* Small angle formula. */
         t = theta*D2R;
         p = fmod(phi,360.0);
         if (p > 180.0) p -= 360.0;
         p *= (90.0 + p)*D2R;
         rhu = (p*p + t*t)/2.0;
      }
      x0  =  6;
      y0  =  0.0;
   } else if (face == 5) {
      xi  =  m;
      eta =  l;
      if (rhu < 1.0e-8) {
         /* Small angle formula. */
         t = (90.0 + theta)*D2R;
         rhu = t*t/2.0;
      }
      x0  =  0.0;
      y0  = -2;
   }

   if (xi == 0.0 && eta == 0.0) {
      xf  = 0.0;
      yf  = 0.0;
   } else if (-xi >= fabs(eta)) {
      psi = eta/xi;
      chi = 1.0 + psi*psi;
      xf  = -sqrt(rhu/(1.0-1.0/sqrt(1.0+chi)));
      yf  = (xf/15.0)*(wcs_atand(psi) - wcs_asind(psi/sqrt(chi+chi)));
   } else if (xi >= fabs(eta)) {
      psi = eta/xi;
      chi = 1.0 + psi*psi;
      xf  =  sqrt(rhu/(1.0-1.0/sqrt(1.0+chi)));
      yf  = (xf/15.0)*(wcs_atand(psi) - wcs_asind(psi/sqrt(chi+chi)));
   } else if (-eta > fabs(xi)) {
      psi = xi/eta;
      chi = 1.0 + psi*psi;
      yf  = -sqrt(rhu/(1.0-1.0/sqrt(1.0+chi)));
      xf  = (yf/15.0)*(wcs_atand(psi) - wcs_asind(psi/sqrt(chi+chi)));
   } else if (eta > fabs(xi)) {
      psi = xi/eta;
      chi = 1.0 + psi*psi;
      yf  =  sqrt(rhu/(1.0-1.0/sqrt(1.0+chi)));
      xf  = (yf/15.0)*(wcs_atand(psi) - wcs_asind(psi/sqrt(chi+chi)));
   }

   if (fabs(xf) > 1.0) {
      if (fabs(xf) > 1.0+tol) {
         return 2;
      }
      xf = wcs_copysign(1.0,xf);
   }
   if (fabs(yf) > 1.0) {
      if (fabs(yf) > 1.0+tol) {
         return 2;
      }
      yf = wcs_copysign(1.0,yf);
   }

   *x = prj->w[0]*(xf + x0);
   *y = prj->w[0]*(yf + y0);


   return 0;
}

/*--------------------------------------------------------------------------*/

int qscrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   int   direct, face;
   double chi, l, m, n, psi, rho, rhu, xf, yf, w;
   const double tol = 1.0e-12;

   if (prj->flag != PRJSET) {
      if (qscset(prj)) return 1;
   }

   xf = x*prj->w[1];
   yf = y*prj->w[1];

   /* Check bounds. */
   if (fabs(xf) <= 1.0) {
      if (fabs(yf) > 3.0) return 2;
   } else {
      if (fabs(xf) > 7.0) return 2;
      if (fabs(yf) > 1.0) return 2;
   }

   /* Map negative faces to the other side. */
   if (xf < -1.0) xf += 8.0;

   /* Determine the face. */
   if (xf > 5.0) {
      face = 4;
      xf = xf - 6.0;
   } else if (xf > 3.0) {
      face = 3;
      xf = xf - 4.0;
   } else if (xf > 1.0) {
      face = 2;
      xf = xf - 2.0;
   } else if (yf > 1.0) {
      face = 0;
      yf = yf - 2.0;
   } else if (yf < -1.0) {
      face = 5;
      yf = yf + 2.0;
   } else {
      face = 1;
   }

   direct = (fabs(xf) > fabs(yf));
   if (direct) {
      if (xf == 0.0) {
         psi = 0.0;
         chi = 1.0;
         rho = 1.0;
         rhu = 0.0;
      } else {
         w = 15.0*yf/xf;
         psi = wcs_sind(w)/(wcs_cosd(w) - SQRT2INV);
         chi = 1.0 + psi*psi;
         rhu = xf*xf*(1.0 - 1.0/sqrt(1.0 + chi));
         rho = 1.0 - rhu;
      }
   } else {
      if (yf == 0.0) {
         psi = 0.0;
         chi = 1.0;
         rho = 1.0;
         rhu = 0.0;
      } else {
         w = 15.0*xf/yf;
         psi = wcs_sind(w)/(wcs_cosd(w) - SQRT2INV);
         chi = 1.0 + psi*psi;
         rhu = yf*yf*(1.0 - 1.0/sqrt(1.0 + chi));
         rho = 1.0 - rhu;
      }
   }

   if (rho < -1.0) {
      if (rho < -1.0-tol) {
         return 2;
      }

      rho = -1.0;
      rhu =  2.0;
      w   =  0.0;
   } else {
      w = sqrt(rhu*(2.0-rhu)/chi);
   }

   if (face == 0) {
      n = rho;
      if (direct) {
         m = w;
         if (xf < 0.0) m = -m;
         l = -m*psi;
      } else {
         l = w;
         if (yf > 0.0) l = -l;
         m = -l*psi;
      }
   } else if (face == 1) {
      l = rho;
      if (direct) {
         m = w;
         if (xf < 0.0) m = -m;
         n = m*psi;
      } else {
         n = w;
         if (yf < 0.0) n = -n;
         m = n*psi;
      }
   } else if (face == 2) {
      m = rho;
      if (direct) {
         l = w;
         if (xf > 0.0) l = -l;
         n = -l*psi;
      } else {
         n = w;
         if (yf < 0.0) n = -n;
         l = -n*psi;
      }
   } else if (face == 3) {
      l = -rho;
      if (direct) {
         m = w;
         if (xf > 0.0) m = -m;
         n = -m*psi;
      } else {
         n = w;
         if (yf < 0.0) n = -n;
         m = -n*psi;
      }
   } else if (face == 4) {
      m = -rho;
      if (direct) {
         l = w;
         if (xf < 0.0) l = -l;
         n = l*psi;
      } else {
         n = w;
         if (yf < 0.0) n = -n;
         l = n*psi;
      }
   } else if (face == 5) {
      n = -rho;
      if (direct) {
         m = w;
         if (xf < 0.0) m = -m;
         l = m*psi;
      } else {
         l = w;
         if (yf < 0.0) l = -l;
         m = l*psi;
      }
   }

   if (l == 0.0 && m == 0.0) {
      *phi = 0.0;
   } else {
      *phi = wcs_atan2d(m, l);
   }
   *theta = wcs_asind(n);

   return 0;
}

/*============================================================================
*   TSC: tangential spherical cube projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/4)
*      prj->w[1]   (4/pi)/r0
*===========================================================================*/

int tscset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 45.0;
      prj->w[1] = 1.0/45.0;
   } else {
      prj->w[0] = prj->r0*PI/4.0;
      prj->w[1] = 1.0/prj->w[0];
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int tscfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   int   face;
   double costhe, l, m, n, rho, x0, xf, y0, yf;
   const double tol = 1.0e-12;

   if (prj->flag != PRJSET) {
      if (tscset(prj)) return 1;
   }

   costhe = wcs_cosd(theta);
   l = costhe*wcs_cosd(phi);
   m = costhe*wcs_sind(phi);
   n = wcs_sind(theta);

   face = 0;
   rho  = n;
   if (l > rho) {
      face = 1;
      rho  = l;
   }
   if (m > rho) {
      face = 2;
      rho  = m;
   }
   if (-l > rho) {
      face = 3;
      rho  = -l;
   }
   if (-m > rho) {
      face = 4;
      rho  = -m;
   }
   if (-n > rho) {
      face = 5;
      rho  = -n;
   }

   if (face == 0) {
      xf =  m/rho;
      yf = -l/rho;
      x0 =  0.0;
      y0 =  2.0;
   } else if (face == 1) {
      xf =  m/rho;
      yf =  n/rho;
      x0 =  0.0;
      y0 =  0.0;
   } else if (face == 2) {
      xf = -l/rho;
      yf =  n/rho;
      x0 =  2.0;
      y0 =  0.0;
   } else if (face == 3) {
      xf = -m/rho;
      yf =  n/rho;
      x0 =  4.0;
      y0 =  0.0;
   } else if (face == 4) {
      xf =  l/rho;
      yf =  n/rho;
      x0 =  6.0;
      y0 =  0.0;
   } else if (face == 5) {
      xf =  m/rho;
      yf =  l/rho;
      x0 =  0.0;
      y0 = -2.0;
   }

   if (fabs(xf) > 1.0) {
      if (fabs(xf) > 1.0+tol) {
         return 2;
      }
      xf = wcs_copysign(1.0,xf);
   }
   if (fabs(yf) > 1.0) {
      if (fabs(yf) > 1.0+tol) {
         return 2;
      }
      yf = wcs_copysign(1.0,yf);
   }

   *x = prj->w[0]*(xf + x0);
   *y = prj->w[0]*(yf + y0);

   return 0;
}

/*--------------------------------------------------------------------------*/

int tscrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double l, m, n, xf, yf;

   if (prj->flag != PRJSET) {
      if (tscset(prj)) return 1;
   }

   xf = x*prj->w[1];
   yf = y*prj->w[1];

   /* Check bounds. */
   if (fabs(xf) <= 1.0) {
      if (fabs(yf) > 3.0) return 2;
   } else {
      if (fabs(xf) > 7.0) return 2;
      if (fabs(yf) > 1.0) return 2;
   }

   /* Map negative faces to the other side. */
   if (xf < -1.0) xf += 8.0;

   /* Determine the face. */
   if (xf > 5.0) {
      /* face = 4 */
      xf = xf - 6.0;
      m  = -1.0/sqrt(1.0 + xf*xf + yf*yf);
      l  = -m*xf;
      n  = -m*yf;
   } else if (xf > 3.0) {
      /* face = 3 */
      xf = xf - 4.0;
      l  = -1.0/sqrt(1.0 + xf*xf + yf*yf);
      m  =  l*xf;
      n  = -l*yf;
   } else if (xf > 1.0) {
      /* face = 2 */
      xf = xf - 2.0;
      m  =  1.0/sqrt(1.0 + xf*xf + yf*yf);
      l  = -m*xf;
      n  =  m*yf;
   } else if (yf > 1.0) {
      /* face = 0 */
      yf = yf - 2.0;
      n  = 1.0/sqrt(1.0 + xf*xf + yf*yf);
      l  = -n*yf;
      m  =  n*xf;
   } else if (yf < -1.0) {
      /* face = 5 */
      yf = yf + 2.0;
      n  = -1.0/sqrt(1.0 + xf*xf + yf*yf);
      l  = -n*yf;
      m  = -n*xf;
   } else {
      /* face = 1 */
      l  =  1.0/sqrt(1.0 + xf*xf + yf*yf);
      m  =  l*xf;
      n  =  l*yf;
   }

   if (l == 0.0 && m == 0.0) {
      *phi = 0.0;
   } else {
      *phi = wcs_atan2d(m, l);
   }
   *theta = wcs_asind(n);

   return 0;
}


/*============================================================================
*   TNX: IRAF's gnomonic projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*===========================================================================*/

int tnxset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) prj->r0 = R2D;

   if (prj->flag == -1) {
      prj->flag = -PRJSET;
   } else {
      prj->flag = PRJSET;
   } 

   return 0;
}

/*--------------------------------------------------------------------------*/

int tnxfwd(phi, theta, prj, x, y)

const double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   double r, s, xp[2];

   if (abs(prj->flag) != PRJSET) {
      if(tnxset(prj)) return 1;
   }

   s = wcs_sind(theta);
   if (s == 0.0) return 2;

   r =  prj->r0*wcs_cosd(theta)/s;
   xp[0] =  r*wcs_sind(phi);
   xp[1] = -r*wcs_cosd(phi);
   *x = prj->inv_x? poly_func(prj->inv_x, xp) : xp[0];
   *y = prj->inv_y? poly_func(prj->inv_y, xp) : xp[1];

   if (prj->flag == PRJSET && s < 0.0) {
      return 2;
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int tnxrev(x, y, prj, phi, theta)

const double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double	rp,xp,yp;

   if (abs(prj->flag) != PRJSET) {
      if (tanset(prj)) return 1;
   }

   xp = x+raw_to_tnxaxis(prj->tnx_lngcor, x, y);
   yp = y+raw_to_tnxaxis(prj->tnx_latcor, x, y);
   if ((rp = sqrt(xp*xp+yp*yp)) == 0.0) {
     *phi = 0.0;
   } else {
     *phi = wcs_atan2d(xp, -yp);
   }
   *theta = wcs_atan2d(prj->r0, rp);

   return 0;
}

/*--------------------------------------------------------------------------*/

int raw_to_pv(struct prjprm *prj, double x, double y, double *xo, double *yo)

{
   int		k;
   double	*a,*b,
		r,r3,r5,r7,xy,x2,x3,x4,x5,x6,x7,y2,y3,y4,y5,y6,y7,xp,yp;

   if (abs(prj->flag) != PRJSET) {
      if (tanset(prj)) return 1;
   }

   k=prj->n;
   a = prj->p;			/* Longitude */
   b = prj->p+100;		/* Latitude */
   xp = *(a++);
   xp += *(a++)*x;
   yp = *(b++);
   yp += *(b++)*y;
   if (!--k) goto poly_end;
   xp += *(a++)*y;
   yp += *(b++)*x;
   if (!--k) goto poly_end;
   r = sqrt(x*x + y*y);
   xp += *(a++)*r;
   yp += *(b++)*r;
   if (!--k) goto poly_end;
   xp += *(a++)*(x2=x*x);
   yp += *(b++)*(y2=y*y);
   if (!--k) goto poly_end;
   xp += *(a++)*(xy=x*y);
   yp += *(b++)*xy;
   if (!--k) goto poly_end;
   xp += *(a++)*y2;
   yp += *(b++)*x2;
   if (!--k) goto poly_end;
   xp += *(a++)*(x3=x*x2);
   yp += *(b++)*(y3=y*y2);
   if (!--k) goto poly_end;
   xp += *(a++)*x2*y;
   yp += *(b++)*y2*x;
   if (!--k) goto poly_end;
   xp += *(a++)*x*y2;
   yp += *(b++)*y*x2;
   if (!--k) goto poly_end;
   xp += *(a++)*y3;
   yp += *(b++)*x3;
   if (!--k) goto poly_end;
   xp += *(a++)*(r3=r*r*r);
   yp += *(b++)*r3;
   if (!--k) goto poly_end;
   xp += *(a++)*(x4=x*x);
   yp += *(b++)*(y4=y*y);
   if (!--k) goto poly_end;
   xp += *(a++)*x3*y;
   yp += *(b++)*y3*x;
   if (!--k) goto poly_end;
   xp += *(a++)*x2*y2;
   yp += *(b++)*x2*y2;
   if (!--k) goto poly_end;
   xp += *(a++)*x*y3;
   yp += *(b++)*y*x3;
   if (!--k) goto poly_end;
   xp += *(a++)*y4;
   yp += *(b++)*x4;
   if (!--k) goto poly_end;
   xp += *(a++)*(x5=x4*x);
   yp += *(b++)*(y5=y4*y);
   if (!--k) goto poly_end;
   xp += *(a++)*x4*y;
   yp += *(b++)*y4*x;
   if (!--k) goto poly_end;
   xp += *(a++)*x3*y2;
   yp += *(b++)*y3*x2;
   if (!--k) goto poly_end;
   xp += *(a++)*x2*y3;
   yp += *(b++)*y2*x3;
   if (!--k) goto poly_end;
   xp += *(a++)*x*y4;
   yp += *(b++)*y*x4;
   if (!--k) goto poly_end;
   xp += *(a++)*y5;
   yp += *(b++)*x5;
   if (!--k) goto poly_end;
   xp += *(a++)*(r5=r3*r*r);
   yp += *(b++)*r5;
   if (!--k) goto poly_end;
   xp += *(a++)*(x6=x5*x);
   yp += *(b++)*(y6=y5*y);
   if (!--k) goto poly_end;
   xp += *(a++)*x5*y;
   yp += *(b++)*y5*x;
   if (!--k) goto poly_end;
   xp += *(a++)*x4*y2;
   yp += *(b++)*y4*x2;
   if (!--k) goto poly_end;
   xp += *(a++)*x3*y3;
   yp += *(b++)*y3*x3;
   if (!--k) goto poly_end;
   xp += *(a++)*x2*y4;
   yp += *(b++)*y4*x2;
   if (!--k) goto poly_end;
   xp += *(a++)*x*y5;
   yp += *(b++)*y*x5;
   if (!--k) goto poly_end;
   xp += *(a++)*y6;
   yp += *(b++)*x6;
   if (!--k) goto poly_end;
   xp += *(a++)*(x7=x6*x);
   yp += *(b++)*(y7=y6*y);
   if (!--k) goto poly_end;
   xp += *(a++)*x6*y;
   yp += *(b++)*y6*x;
   if (!--k) goto poly_end;
   xp += *(a++)*x5*y2;
   yp += *(b++)*y5*x2;
   if (!--k) goto poly_end;
   xp += *(a++)*x4*y3;
   yp += *(b++)*y4*x3;
   if (!--k) goto poly_end;
   xp += *(a++)*x3*y4;
   yp += *(b++)*y3*x4;
   if (!--k) goto poly_end;
   xp += *(a++)*x2*y5;
   yp += *(b++)*y2*x5;
   if (!--k) goto poly_end;
   xp += *(a++)*x*y6;
   yp += *(b++)*y*x6;
   if (!--k) goto poly_end;
   xp += *(a++)*y7;
   yp += *(b++)*x7;
   if (!--k) goto poly_end;
   xp += *a*(r7=r5*r*r);
   yp += *b*r7;

poly_end:

  *xo = xp;
  *yo = yp;

   return 0;
}

