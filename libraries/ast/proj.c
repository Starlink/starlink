/*============================================================================
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
*  This version of proj.c is based on the version in wcslib-2.2, but has
*  been modified in the following ways by the Starlink project (e-mail: 
*  ussc@star.rl.ac.uk):
*     -  The copysign macro is now always defined within this file
*        instead of only being defined if the COPYSIGN macro has previously 
*        been defined.
*     -  "i == 10" changed to "i == 180" at line 726.
*     -  Check has been added on the magnitude of the argument of the 
*        "asind" function at lines 980 and 1247.
*     -  Sine values which are slightly larger than 1.0 are now treated 
*        as 1.0 in function astCyprev.
*     -  Redundant re-calculation of "w" removed at line 2230.
*     -  All local variables in the CSC projection functions converted from 
*        single to double precision.
*     -  Fixes (supplied by M.Calabretta) included to produce a range error
*        in astCscrev, astQscrev and astTscrev if the supplied (x,y) values are not
*        within the principal domain of the projection.
*     -  Bug fix (supplied by M.Calabretta) in astSinrev, which caused a floating 
*        exception if the sum of the squares of alpha (PROJP1) and beta 
*        (PROJP2) is zero. 
*
*  D.S. Berry (6th August 1996)
*
*     -  Perspective zenithal projections (AZP, TAN, SIN) modified so that
*     bad (x,y) coordinates are returned if the supplied native latitude
*     represents a point on the "far-side" of the unit sphere (i.e. the
*     side away from the projection plane).
*
*  D.S. Berry (11th February 2000)
*     -  Lots of changes to use 24/9/1999 conventions. Changed static
*     projp[10] array to a dynamic array of parameter values for each axis.
*     Extended TAN projection to include polynomial corrections. Changed
*     ZPN projection to handle up to 100 co-efficients. GLS projection
*     re-named as SFL.
*  D.S. Berry (26th September 2001)
*     - Changed names of all function to avoid name clashes with wcslib.
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
*      astAzpset astAzpfwd astAzprev   AZP: zenithal/azimuthal perspective
*      astTanset astTanfwd astTanrev   TAN: gnomonic
*      astSinset astSinfwd astSinrev   SIN: orthographic/synthesis
*      astStgset astStgfwd astStgrev   STG: stereographic
*      astArcset astArcfwd astArcrev   ARC: zenithal/azimuthal equidistant
*      astZpnset astZpnfwd astZpnrev   ZPN: zenithal/azimuthal polynomial
*      astZeaset astZeafwd astZearev   ZEA: zenithal/azimuthal equal area
*      astAirset astAirfwd astAirrev   AIR: Airy
*      astCypset astCypfwd astCyprev   CYP: cylindrical perspective
*      astCarset astCarfwd astCarrev   CAR: Cartesian
*      astMerset astMerfwd astMerrev   MER: Mercator
*      astCeaset astCeafwd astCearev   CEA: cylindrical equal area
*      astCopset astCopfwd astCoprev   COP: conic perspective
*      astCodset astCodfwd astCodrev   COD: conic equidistant
*      astCoeset astCoefwd astCoerev   COE: conic equal area
*      astCooset astCoofwd astCoorev   COO: conic orthomorphic
*      astBonset astBonfwd astBonrev   BON: Bonne
*      astPcoset astPcofwd astPcorev   PCO: polyconic
*      astSflset astSflfwd astSflrev   SFL: Sanson-Flamsteed (global sinusoidal)
*      astParset astParfwd astParrev   PAR: parabolic
*      astAitset astAitfwd astAitrev   AIT: Hammer-Aitoff
*      astMolset astMolfwd astMolrev   MOL: Mollweide
*      astCscset astCscfwd astCscrev   CSC: COBE quadrilateralized spherical cube
*      astQscset astQscfwd astQscrev   QSC: quadrilateralized spherical cube
*      astTscset astTscfwd astTscrev   TSC: tangential spherical cube
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
*                           100 - 399: Longitude axis projection parameter 
*                              (status-100) not supplied.
*                           400 - 699: Latitude axis projection parameter 
*                              (status-400) not supplied.
*
*   Forward transformation; *fwd()
*   -----------------------------
*   Compute (x,y) coordinates in the plane of projection from native spherical
*   coordinates (phi,theta).
*
*   Given:
*      phi,     double   Longitude and latitude of the projected point in
*      theta             native spherical coordinates, in degrees.
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
*                           100 - 399: Longitude axis projection parameter 
*                              (status-100) not supplied.
*                           400 - 699: Latitude axis projection parameter 
*                              (status-400) not supplied.
*
*   Reverse transformation; *rev()
*   -----------------------------
*   Compute native spherical coordinates (phi,theta) from (x,y) coordinates in
*   the plane of projection.
*
*   Given:
*      x,y      double   Projected coordinates.
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
*                           100 - 399: Longitude axis projection parameter 
*                              (status-100) not supplied.
*                           400 - 699: Latitude axis projection parameter 
*                              (status-400) not supplied.
*
*   Projection parameters
*   ---------------------
*   The prjprm struct consists of the following:
*
*      int flag
*         This flag must be set to zero whenever any of p, np, axlat,
*         axlon  or r0 are set or changed.  This signals the initialization 
*         routine to recompute intermediaries.
*      double r0
*         r0; The radius of the generating sphere for the projection, a linear
*         scaling parameter.  If this is zero, it will be reset to the default
*         value of 180/pi (the value for FITS WCS).
*      double *p[]
*         An array of pointers, one for each intermediate world
*         co-ordinate axis. Each pointer points to an array of projection
*         parameter values for the associated axis. Any parameter equal
*         to the value of "unset" (see below) is considered not to have a
*         value. Such values will either be defaulted, or will result in
*         an error status being returned.
*      int np[]
*         An array of integers, one for each intermediate world
*         co-ordinate axis. Each integer gives the number of parameter
*         values for the axis (i.e. the length of the array pointed to 
*         by p[j]). Note, p[j][0] is the zeroth parameter which is only
*         used by a few projections (e.g. ZPN and TAN). A projection such
*         as AZP which uses PVj_1 will ignore p[j][0], and obtain PVj_1
*         from p[j][1]. In this case np[j] must be 2 even though only
*         one parameter value is required, because the required parameter 
*         value is stored in the second element of the array (i.e. at 
*         index 1).
*      double unset
*         A "magic" projection parameter value, which indicates that the
*         parameter has not been set.
*      int axlat
*         The index within p[] and np[] of the latitude axis values.
*      int axlon
*         The index within p[] and np[] of the longitude axis values.

*
*   The remaining members of the prjprm struct are maintained by the
*   initialization routines and should not be modified.  This is done for the
*   sake of efficiency and to allow an arbitrary number of contexts to be
*   maintained simultaneously.
*
*      int n
*      double w[110]
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
*      Although many of the forward projections will accept values of theta
*      outside the range [-90,90] such latitudes are not meaningful and should
*      normally be marked as an error.  However, in the interests of
*      efficiency, the forward projection routines do not check for this,
*      although they do check for any invalid values of theta within the
*      range [-90,90].
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
*   Closure to a precision of at least 1.0-10 degree of longitude and latitude
*   has been verified for typical projection parameters on the 1 degree grid
*   of native longitude and latitude (to within 5 degrees of any latitude
*   where the projection may diverge).
*
*   Notwithstanding this, absolutely no claim is made for the accuracy or
*   reliability of these routines.  They are supplied as is, with no warranty
*   of fitness for any purpose.
*
*   Author: Mark Calabretta, Australia Telescope National Facility
*===========================================================================*/

#include "proj.h"

#define copysign(X, Y) ((Y) < 0.0 ? -fabs(X) : fabs(X))


/*============================================================================
*   AZP: zenithal/azimuthal perspective projection.
*
*   Given:
*      prj->axlat         Index of latitude axis
*      prj->p[axlat][1]   AZP distance parameter, mu in units of r0 (error 
*                         401 returned if not set).
*      prj->n[axlat]      No. of parameters supplied.
*      prj->unset         Value used to indicate an unset projection parameter
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(mu+1)
*      prj->w[1]   1/prj->w[0]
*      prj->w[2]   -1/mu
*      prj->w[3]   mu
*===========================================================================*/

int astAzpset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) prj->r0 = R2D;

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 1 ){
      prj->w[3] = prj->p[ prj->axlat ][ 1 ];
      if( prj->w[3] == prj->unset ) return 401;
   } else {
      return  401;
   }

   prj->w[0] = prj->r0*(prj->w[3] + 1.0);
   if (prj->w[0] == 0.0) {
      return 1;
   }

   prj->w[1] = 1.0/prj->w[0];

   prj->flag = PRJSET;

   if (prj->w[3] != 0.0) prj->w[2] = -1.0/prj->w[3];

   return 0;
}

/*--------------------------------------------------------------------------*/

int astAzpfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double r, s, sinth, mu;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astAzpset( prj );
      if( seterr ) return seterr;
   }

   sinth = sind(theta);
   mu = prj->w[3];
   s = mu + sinth;

   if (s == 0.0) {
      return 2;

   } else if (mu <= 1.0) {
      if( sinth < -mu ) return 2;

   } else {
      if( sinth < prj->w[2] ) return 2;

   }

   r =  prj->w[0]*cosd(theta)/s;
   *x =  r*sind(phi);
   *y = -r*cosd(phi);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astAzprev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double r, rho, s;
   const double tol = 1.0e-13;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astAzpset( prj );
      if( seterr ) return seterr;
   }

   r = sqrt(x*x + y*y);
   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = atan2d(x, -y);
   }

   rho = r*prj->w[1];
   s = rho*prj->w[3]/sqrt(rho*rho+1.0);
   if (fabs(s) > 1.0) {
      if (fabs(s) > 1.0+tol) {
         return 2;
      }
      *theta = atan2d(1.0,rho) - copysign(90.0,s);
   } else {
      *theta = atan2d(1.0,rho) - asind(s);
   }

   return 0;
}

/*============================================================================
*   TAN: gnomonic projection.
*
*   Given:
*      prj->axlat         Index of latitude axis
*      prj->axlon         Index of longitude axis
*      prj->p             Array of coefficients
*      prj->n             No. of coeffificients for each axis
*      prj->unset         Value used to indicate an unset projection parameter
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->n      Set to 0 if a simple tan projection is required
*                  (with no polynomial correction). Otherwise, set to 1.
*      prj->w[0:NPTAN-1]         Co-efficients for the longitude axis
*      prj->w[NPTAN:2*NPTAN-1]   Co-efficients for the latitude axis

*===========================================================================*/

int astTanset(prj)

struct prjprm *prj;

{
   int m;
   double x, y;

   if (prj->r0 == 0.0) prj->r0 = R2D;

   /* Initially assume all are unset */
   for( m = 0; m < NPTAN; m++ ){
      prj->w[ m ] = prj->unset;
      prj->w[ m + NPTAN ] = prj->unset;
   }   

   /* Copy values for any supplied parameters */
   if( prj->np && prj->p ){

      for( m = 0; m < prj->np[ prj->axlon ]; m++ ){
         prj->w[ m ] = prj->p[ prj->axlon ][ m ];      
      }   

      for( m = 0; m < prj->np[ prj->axlat ]; m++ ){
         prj->w[ m + NPTAN ] = prj->p[ prj->axlat ][ m ];      
      }   
   }

   /* A1 and B1 default to unity */
   if( prj->w[ 1 ] == prj->unset ) prj->w[ 1 ] = 1.0;
   if( prj->w[ 1 + NPTAN ] == prj->unset ) prj->w[ 1 + NPTAN ] = 1.0;

   /* All others default to zero */
   for( m = 0; m < NPTAN; m++ ){
      if( prj->w[ m ] == prj->unset ) prj->w[ m ] = 0.0;
      if( prj->w[ m + NPTAN ] == prj->unset ) prj->w[ m + NPTAN ] = 0.0;
   }   

   /* If all co-efficients have their default values, we do not need to
      use the polynomial correction. */
   prj->n = 0;

   if( prj->w[ 0 ] != 0.0 || prj->w[ NPTAN ] != 0.0 ) {
      prj->n = 1;

   } else if( prj->w[ 1 ] != 1.0 || prj->w[ 1 + NPTAN ] != 1.0 ) {
      prj->n = 1;

   } else {
      for( m = 2; m < NPTAN; m++ ){
         if( prj->w[ m ] != 0.0 || prj->w[ m + NPTAN ] != 0.0 ){
            prj->n = 1;
            break;
         } 
      }   
   }

   prj->flag = PRJSET;

   return 0;
}

/*--------------------------------------------------------------------------*/

int astTanfwd(phi, theta, prj, xx, yy)

double phi, theta, *xx, *yy;
struct prjprm *prj;

{
   double r, s, xi, eta, x2, xy, y2, r2, x3, x2y, xy2, y3, r3, x4, x3y,
          x2y2, xy3, y4, x5, x4y, x3y2, x2y3, xy4, y5, r5, x6, x5y, x4y2,
	  x3y3, x2y4, xy5, y6, x7, x6y, x5y2, x4y3, x3y4, x2y5, xy6, y7,
	  r7, tol, f, g, fx, fy, gx, gy, dx, dy, x, y, denom;
   double *a, *b;
   int i, seterr, ok;

   if (prj->flag != PRJSET) {
      seterr = astTanset( prj );
      if( seterr ) return seterr;
   }

   s = sind(theta);
   if (s <= 0.0) return 2;

   r =  prj->r0*cosd(theta)/s;
   xi =  r*sind(phi);
   eta = -r*cosd(phi);

   /* Simple tan */
   if( !prj->n ){
      *xx = xi;
      *yy = eta;

   /* Tan with polynomial corrections: Iterate using Newton's method to
      get the (x,y) corresponding to the above (xi,eta). */ 
   } else {
      a = ( prj->w );
      b = ( prj->w ) + NPTAN;

   /* Initial guess: linear solution assuming a3,... and b3,... are zero. */
      denom = a[1]*b[1] - a[2]*b[2];
      if( denom != 0.0 ) {
         x = ( xi*b[1] - eta*a[2] - a[0]*b[1] + b[0]*a[2] )/denom;
         y = -( xi*b[2] - eta*a[1] - a[0]*b[2] + b[0]*a[1] )/denom;

      } else {
         if( a[1] != 0.0 ){
            x = ( xi - a[0] )/a[1];
         } else {
            x = a[0];
         }

         if( b[1] != 0.0 ){
            y = ( eta - b[0] )/b[1];
         } else {
            y = b[0];
         }
      }

/* Iterate up to 50 times, until the required relative accuracy is
   achieved. */
      tol = 1.0E-5;
      ok = 0;
      for (i = 0; i < 50; i++) {

/* Get required products of the current x and y values */
         x2 = x*x;
         xy = x*y;
         y2 = y*y;
   
         r2 = x2 + y2;
         r = sqrt( r2 );
   
         x3 = x2*x;
         x2y = x2*y;
         xy2 = x*y2;
         y3 = y*y2;
   
         r3 = r*r2;
   
         x4 = x3*x;
         x3y = x3*y;
         x2y2 = x2*y2;
         xy3 = x*y3;
         y4 = y*y3;
         
         x5 = x4*x;
         x4y = x4*y;
         x3y2 = x3*y2;
         x2y3 = x2*y3;
         xy4 = x*y4;
         y5 = y*y4;
   
         r5 = r3*r2;
   
         x6 = x5*x;
         x5y = x5*y;
         x4y2 = x4*y2;
         x3y3 = x3*y3;
         x2y4 = x2*y4;
         xy5 = x*y5;
         y6 = y*y5;
   
         x7 = x6*x;
         x6y = x6*y;
         x5y2 = x5*y2;
         x4y3 = x4*y3;
         x3y4 = x3*y4;
         x2y5 = x2*y5;
         xy6 = x*y6;
         y7 = y*y6;
   
         r7 = r5*r2;
   
/* Get the xi and eta models corresponding to the current x and y values */
         f =   a[0]       + a[1]*x     + a[2]*y     + a[3]*r     + a[4]*x2 
              + a[5]*xy    + a[6]*y2    + a[7]*x3    + a[8]*x2y   + a[9]*xy2   
              + a[10]*y3   + a[11]*r3   + a[12]*x4   + a[13]*x3y  + a[14]*x2y2 
              + a[15]*xy3  + a[16]*y4   + a[17]*x5   + a[18]*x4y  + a[19]*x3y2 
              + a[20]*x2y3 + a[21]*xy4  + a[22]*y5   + a[23]*r5   + a[24]*x6   
              + a[25]*x5y  + a[26]*x4y2 + a[27]*x3y3 + a[28]*x2y4 + a[29]*xy5  
              + a[30]*y6   + a[31]*x7   + a[32]*x6y  + a[33]*x5y2 + a[34]*x4y3 
              + a[35]*x3y4 + a[36]*x2y5 + a[37]*xy6  + a[38]*y7   + a[39]*r7;
   
         g =  b[0]       + b[1]*y     + b[2]*x     + b[3]*r     + b[4]*y2 
              + b[5]*xy    + b[6]*x2    + b[7]*y3    + b[8]*xy2   + b[9]*x2y   
              + b[10]*x3   + b[11]*r3   + b[12]*y4   + b[13]*xy3  + b[14]*x2y2 
              + b[15]*x3y  + b[16]*x4   + b[17]*y5   + b[18]*xy4  + b[19]*x2y3
              + b[20]*x3y2 + b[21]*x4y  + b[22]*x5   + b[23]*r5   + b[24]*y6   
              + b[25]*xy5  + b[26]*x2y4 + b[27]*x3y3 + b[28]*x4y2 + b[29]*x5y  
              + b[30]*x6   + b[31]*y7   + b[32]*xy6  + b[33]*x2y5 + b[34]*x3y4 
              + b[35]*x4y3 + b[36]*x5y2 + b[37]*x6y  + b[38]*x7   + b[39]*r7;

/* Partial derivative of xi wrt x... */
         fx = a[1]         + a[3]*( (r!=0.0)?(x/r):0.0 ) + 2*a[4]*x + 
              a[5]*y       + 3*a[7]*x2    + 2*a[8]*xy    + a[9]*y2 + 
              3*a[11]*r*x  + 4*a[12]*x3   + 3*a[13]*x2y  + 2*a[14]*xy2  + 
	      a[15]*y3     + 5*a[17]*x4   + 4*a[18]*x3y  + 3*a[19]*x2y2 + 
	      2*a[20]*xy3  + a[21]*y4     + 5*a[23]*r3*x + 6*a[24]*x5  + 
              5*a[25]*x4y  + 4*a[26]*x3y2 + 3*a[27]*x2y3 + 2*a[28]*xy4  + 
	      a[29]*y5     + 7*a[31]*x6   + 6*a[32]*x5y  + 5*a[33]*x4y2 + 
              4*a[34]*x3y3 + 3*a[35]*x2y4 + 2*a[36]*xy5  + a[37]*y6 + 
              7*a[39]*r5*x;

/* Partial derivative of xi wrt y... */
         fy = a[2]         + a[3]*( (r!=0.0)?(y/r):0.0 ) + a[5]*x +
              2*a[6]*y     + a[8]*x2      + 2*a[9]*xy    + 3*a[10]*y2 +
	      3*a[11]*r*y  + a[13]*x3     + 2*a[14]*x2y  + 3*a[15]*xy2 +
	      4*a[16]*y3   + a[18]*x4     + 2*a[19]*x3y  + 3*a[20]*x2y2 +
	      4*a[21]*xy3  + 5*a[22]*y4   + 5*a[23]*r3*y + a[25]*x5 +
	      2*a[26]*x4y  + 3*a[27]*x3y2 + 4*a[28]*x2y3 + 5*a[29]*xy4 + 
              6*a[30]*y5   + a[32]*x6     + 2*a[33]*x5y  + 3*a[34]*x4y2 + 
              4*a[35]*x3y3 + 5*a[36]*x2y4 + 6*a[37]*xy5  + 7*a[38]*y6 + 
              7*a[39]*r5*y;

/* Partial derivative of eta wrt x... */
         gx = b[2]         + b[3]*( (r!=0.0)?(x/r):0.0 ) + b[5]*y +
              2*b[6]*x     + b[8]*y2      + 2*b[9]*xy    + 3*b[10]*x2 +
	      3*b[11]*r*x  + b[13]*y3     + 2*b[14]*xy2  + 3*b[15]*x2y +
	      4*b[16]*x3   + b[18]*y4     + 2*b[19]*xy3  + 3*b[20]*x2y2 +
	      4*b[21]*x3y  + 5*b[22]*x4   + 5*b[23]*r3*x + b[25]*y5 +
	      2*b[26]*xy4  + 3*b[27]*x2y3 + 4*b[28]*x3y2 + 5*b[29]*x4y +
	      6*b[30]*x5   + b[32]*y6     + 2*b[33]*xy5  + 3*b[34]*x2y4 +
	      4*b[35]*x3y3 + 5*b[36]*x4y2 + 6*b[37]*x5y  + 7*b[38]*x6 +
	      7*b[39]*r5*x;

/* Partial derivative of eta wrt y... */
         gy = b[1]         + b[3]*( (r!=0.0)?(y/r):0.0 ) + 2*b[4]*y +
              b[5]*x       + 3*b[7]*y2    + 2*b[8]*xy    + b[9]*x2 + 
              3*b[11]*r*y  + 4*b[12]*y3   + 3*b[13]*xy2  + 2*b[14]*x2y  + 
	      b[15]*x3     + 5*b[17]*y4   + 4*b[18]*xy3  + 3*b[19]*x2y2 + 
	      2*b[20]*x3y  + b[21]*x4     + 5*b[23]*r3*y + 6*b[24]*y5  + 
	      5*b[25]*xy4  + 4*b[26]*x2y3 + 3*b[27]*x3y2 + 2*b[28]*x4y  + 
	      b[29]*x5     + 7*b[31]*y6   + 6*b[32]*xy5  + 5*b[33]*x2y4 +
	      4*b[34]*x3y3 + 3*b[35]*x4y2 + 2*b[36]*x5y  + b[37]*x6     + 
              7*b[39]*r5*y;

/* Calculate new x and y values. */
         f = f - xi;
         g = g - eta;
         dx = ( (-f*gy) + (g*fy) ) / ( (fx*gy) - (fy*gx) );
         dy = ( (-g*fx) + (f*gx) ) / ( (fx*gy) - (fy*gx) );
         x += dx;
         y += dy;

/* Check if convergence has been achieved. */
         if( fabs(dx) <= tol*fabs(x) && fabs(dy) <= tol*fabs(y) ) {
            ok = 1;
            break;
         }
      }

      *xx = x;
      *yy = y;

      if( !ok ) return 2;

   }

   return 0;

}

/*--------------------------------------------------------------------------*/

int astTanrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double r, xi, eta, x2, xy, y2, r2, x3, x2y, xy2, y3, r3, x4, x3y,
          x2y2, xy3, y4, x5, x4y, x3y2, x2y3, xy4, y5, r5, x6, x5y, x4y2,
	  x3y3, x2y4, xy5, y6, x7, x6y, x5y2, x4y3, x3y4, x2y5, xy6, y7,
	  r7;
   double *a, *b;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astTanset( prj );
      if( seterr ) return seterr;
   }

   /* Simple tan */
   if( !prj->n ) {
      xi = x;
      eta = y;

   /* Tan with polynomial corrections. */
   } else {
      x2 = x*x;
      xy = x*y;
      y2 = y*y;

      r2 = x2 + y2;
      r = sqrt( r2 );

      x3 = x2*x;
      x2y = x2*y;
      xy2 = x*y2;
      y3 = y*y2;

      r3 = r*r2;

      x4 = x3*x;
      x3y = x3*y;
      x2y2 = x2*y2;
      xy3 = x*y3;
      y4 = y*y3;
      
      x5 = x4*x;
      x4y = x4*y;
      x3y2 = x3*y2;
      x2y3 = x2*y3;
      xy4 = x*y4;
      y5 = y*y4;

      r5 = r3*r2;

      x6 = x5*x;
      x5y = x5*y;
      x4y2 = x4*y2;
      x3y3 = x3*y3;
      x2y4 = x2*y4;
      xy5 = x*y5;
      y6 = y*y5;

      x7 = x6*x;
      x6y = x6*y;
      x5y2 = x5*y2;
      x4y3 = x4*y3;
      x3y4 = x3*y4;
      x2y5 = x2*y5;
      xy6 = x*y6;
      y7 = y*y6;

      r7 = r5*r2;

      a = prj->w;
      xi =   a[0]       + a[1]*x     + a[2]*y     + a[3]*r     + a[4]*x2 
           + a[5]*xy    + a[6]*y2    + a[7]*x3    + a[8]*x2y   + a[9]*xy2   
           + a[10]*y3   + a[11]*r3   + a[12]*x4   + a[13]*x3y  + a[14]*x2y2 
           + a[15]*xy3  + a[16]*y4   + a[17]*x5   + a[18]*x4y  + a[19]*x3y2  
           + a[20]*x2y3 + a[21]*xy4  + a[22]*y5   + a[23]*r5   + a[24]*x6   
           + a[25]*x5y  + a[26]*x4y2 + a[27]*x3y3 + a[28]*x2y4 + a[29]*xy5  
           + a[30]*y6   + a[31]*x7   + a[32]*x6y  + a[33]*x5y2 + a[34]*x4y3 
           + a[35]*x3y4 + a[36]*x2y5 + a[37]*xy6  + a[38]*y7   + a[39]*r7;

      b = ( prj->w ) + NPTAN;
      eta =  b[0]       + b[1]*y     + b[2]*x     + b[3]*r     + b[4]*y2 
           + b[5]*xy    + b[6]*x2    + b[7]*y3    + b[8]*xy2   + b[9]*x2y   
           + b[10]*x3   + b[11]*r3   + b[12]*y4   + b[13]*xy3  + b[14]*x2y2 
           + b[15]*x3y  + b[16]*x4   + b[17]*y5   + b[18]*xy4  + b[19]*x2y3  
           + b[20]*x3y2 + b[21]*x4y  + b[22]*x5   + b[23]*r5   + b[24]*y6   
           + b[25]*xy5  + b[26]*x2y4 + b[27]*x3y3 + b[28]*x4y2 + b[29]*x5y  
           + b[30]*x6   + b[31]*y7   + b[32]*xy6  + b[33]*x2y5 + b[34]*x3y4 
           + b[35]*x4y3 + b[36]*x5y2 + b[37]*x6y  + b[38]*x7   + b[39]*r7;

   }

   /* Now do the tan projection */
   r = sqrt(xi*xi + eta*eta);
   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = atan2d(xi, -eta);
   }
   *theta = atan2d(prj->r0, r);

   return 0;
}

/*============================================================================
*   SIN: orthographic/synthesis projection.
*
*   Given:
*      prj->axlat         Index of latitude axis
*      prj->p[axlat][1:2] SIN obliqueness parameters, alpha and beta
*                         (both default to zero).
*      prj->n[axlat]      No. of parameters supplied.
*      prj->unset         Value used to indicate an unset projection parameter
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   1/r0
*      prj->w[1]   alpha**2 + beta**2
*      prj->w[2]   2*(alpha**2 + beta**2)
*      prj->w[3]   2*(alpha**2 + beta**2 + 1)
*      prj->w[4]   alpha**2 + beta**2 - 1
*      prj->w[5]   alpha
*      prj->w[6]   beta

*===========================================================================*/

int astSinset(prj)

struct prjprm *prj;

{
   if (prj->r0 == 0.0) prj->r0 = R2D;

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 1 ){
      prj->w[5] = prj->p[ prj->axlat ][ 1 ];
      if( prj->w[5] == prj->unset ) prj->w[5] = 0.0;
   } else {
      prj->w[5] = 0.0;
   }

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 2 ){
      prj->w[6] = prj->p[ prj->axlat ][ 2 ];
      if( prj->w[6] == prj->unset ) prj->w[6] = 0.0;
   } else {
      prj->w[6] = 0.0;
   }

   prj->w[0] = 1.0/prj->r0;
   prj->w[1] = prj->w[5]*prj->w[5] + prj->w[6]*prj->w[6];
   prj->w[2] = 2.0*prj->w[1];
   prj->w[3] = prj->w[2] + 2.0;
   prj->w[4] = prj->w[1] - 1.0;

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int astSinfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double cthe, t, z, sinth;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astSinset( prj );
      if( seterr ) return seterr;
   }

   t = (90.0 - fabs(theta))*D2R;
   if (t < 1.0e-5) {
      if (theta > 0.0) {
         z = -t*t/2.0;
      } else {
         return 2;
      }
      cthe = t;
   } else {
      sinth =  sind(theta);
      if( sinth < -1.0e-5 ) return 2;
      z =  sinth - 1.0;
      cthe = cosd(theta);
   }

   *x =  prj->r0*(cthe*sind(phi) + prj->w[5]*z);
   *y = -prj->r0*(cthe*cosd(phi) + prj->w[6]*z);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astSinrev (x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   const double tol = 1.0e-13;
   double a, b, c, d, r2, sth, sth1, sth2, sxy, x0, xp, y0, yp, z;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astSinset( prj );
      if( seterr ) return seterr;
   }

   /* Compute intermediaries. */
   x0 = x*prj->w[0];
   y0 = y*prj->w[0];
   r2 = x0*x0 + y0*y0;

   if (prj->w[1] == 0.0) {
      if (r2 != 0.0) {
         *phi = atan2d(x0, -y0);
      } else {
         *phi = 0.0;
      }

      if (r2 < 0.5) {
         *theta = acosd(sqrt(r2));
      } else if( r2 <= 1.0 ){
         *theta = asind(sqrt(1.0 - r2));
      } else {
         return 2;
      }

   } else {
      if (r2 < 1.0e-10) {
         /* Use small angle formula. */
         z = -r2/2.0;
         *theta = 90.0 - R2D*sqrt(r2/(1.0 - x0*prj->w[5] + y0*prj->w[6]));

      } else {
         sxy = 2.0*(prj->w[5]*x0 - prj->w[6]*y0);

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

         *theta = asind(sth);
         z = sth - 1.0;
      }

   /* Compute native coordinates. */
      xp = -y0 - prj->w[6]*z;
      yp =  x0 - prj->w[5]*z;
      if (xp == 0.0 && yp == 0.0) {
         *phi = 0.0;
      } else {
         *phi   = atan2d(yp,xp);
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

int astStgset(prj)

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

int astStgfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double r, s;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astStgset( prj );
      if( seterr ) return seterr;
   }

   s = 1.0 + sind(theta);
   if (s == 0.0) {
      return 2;
   }

   r =  prj->w[0]*cosd(theta)/s;
   *x =  r*sind(phi);
   *y = -r*cosd(phi);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astStgrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astStgset( prj );
      if( seterr ) return seterr;
   }

   r = sqrt(x*x + y*y);
   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = atan2d(x, -y);
   }
   *theta = 90.0 - 2.0*atand(r*prj->w[1]);

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

int astArcset(prj)

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

int astArcfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astArcset( prj );
      if( seterr ) return seterr;
   }

   r =  prj->w[0]*(90.0 - theta);
   *x =  r*sind(phi);
   *y = -r*cosd(phi);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astArcrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astArcset( prj );
      if( seterr ) return seterr;
   }

   r = sqrt(x*x + y*y);
   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = atan2d(x, -y);
   }
   *theta = 90.0 - r*prj->w[1];

   return 0;
}

/*============================================================================
*   ZPN: zenithal/azimuthal polynomial projection.
*
*   Given and/or returned:
*      prj->unset          Value used to indicate an unset projection parameter
*      prj->axlat          Index of latitude axis
*      prj->p[axlat][0:99] Polynomial co-efficients (error 401 if PVj_1 is
*                          not set, all others default to zero).
*      prj->n[axlat]       No. of parameters supplied.
*      prj->r0      r0; reset to 180/pi if 0.
*      prj->n       Degree of the polynomial, N.
*      prj->w[0:99] Polynomial co-efficients 0 to 99
*      prj->w[100]  Co-latitude of the first point of inflection (N > 2).
*      prj->w[101]  Radius of the first point of inflection (N > 2).
*===========================================================================*/

int astZpnset(prj)

struct prjprm *prj;

{
   int   i, j, k;
   double d, d1, d2, r, zd, zd1, zd2;
   const double tol = 1.0e-13;

   if (prj->r0 == 0.0) prj->r0 = R2D;

   for( i = 0; i < 100; i++ ) prj->w[ i ] = prj->unset;

   if( prj->np && prj->p ){
      for( i = 0; i < prj->np[ prj->axlat ]; i++ ){
         prj->w[ i ] = prj->p[ prj->axlat ][ i ];
      }
   } 

   /* Parameter 1 must be supplied */
   if( prj->w[ 1 ] == prj->unset ) return 401;

   /* Others default to zero */
   for( i = 0; i < 100; i++ ){
      if( prj->w[ i ] == prj->unset ) prj->w[ i ] = 0.0;
   }

   /* Find the highest non-zero coefficient. */
   for( k = 99; k >= 0 && prj->w[ k ] == 0.0; k-- );
   if( k < 0 ) return 1;

   prj->n = k;

   if (k >= 3) {
      /* Find the point of inflection closest to the pole. */
      zd1 = 0.0;
      d1  = prj->w[1];
      if (d1 <= 0.0) {
         return 1;
      }

      /* Find the point where the derivative first goes negative. */
      for (i = 0; i < 180; i++) {
         zd2 = i*D2R;
         d2  = 0.0;
         for (j = k; j > 0; j--) {
            d2 = d2*zd2 + j*prj->w[j];
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
               d = d*zd + j*prj->w[j];
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
         r = r*zd + prj->w[j];
      }
      prj->w[100] = zd;
      prj->w[101] = r;
   }

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int astZpnfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   int   j;
   double r, s;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astZpnset( prj );
      if( seterr ) return seterr;
   }

   s = (90.0 - theta)*D2R;
   r = 0.0;
   for (j = prj->n; j >= 0; j--) {
      r = r*s + prj->w[j];
   }
   r = prj->r0*r;

   *x =  r*sind(phi);
   *y = -r*cosd(phi);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astZpnrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   int   i, j, k;
   double a, b, c, d, lambda, r, r1, r2, rt, zd, zd1, zd2;
   const double tol = 1.0e-13;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astZpnset( prj );
      if( seterr ) return seterr;
   }

   k = prj->n;

   r = sqrt(x*x + y*y)/prj->r0;

   if (k < 1) {
      /* Constant - no solution. */
      return 1;
   } else if (k == 1) {
      /* Linear. */
      zd = (r - prj->w[0])/prj->w[1];
   } else if (k == 2) {
      /* Quadratic. */
      a = prj->w[2];
      b = prj->w[1];
      c = prj->w[0] - r;

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
      r1  = prj->w[0];
      zd2 = prj->w[100];
      r2  = prj->w[101];

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
                rt = (rt * zd) + prj->w[i];
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
      *phi = atan2d(x, -y);
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

int astZeaset(prj)

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

int astZeafwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astZeaset( prj );
      if( seterr ) return seterr;
   }

   r =  prj->w[0]*sind((90.0 - theta)/2.0);
   *x =  r*sind(phi);
   *y = -r*cosd(phi);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astZearev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double r;
   const double tol = 1.0e-12;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astZeaset( prj );
      if( seterr ) return seterr;
   }

   r = sqrt(x*x + y*y);
   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = atan2d(x, -y);
   }

   
   if (fabs(r-prj->w[0]) < tol) {
      *theta = -90.0;
   } else {
      r *= prj->w[1];
      if( fabs( r ) > 1.0 ) return 2;
      *theta = 90.0 - 2.0*asind(r);
   }

   return 0;
}

/*============================================================================
*   AIR: Airy's projection.
*
*   Given:
*      prj->axlat        Index of latitude axis
*      prj->p[axlat][1]  Latitude theta_b within which the error is minimized,
*                        in degrees (defaults to 90).
*      prj->n[axlat]     No. of parameters supplied.
*      prj->unset        Value used to indicate an unset projection parameter
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   ln(cos(xi_b))/tan(xi_b)**2, where xi_b = (90-theta_b)/2
*      prj->w[1]   1/2 - prj->w[0]
*      prj->w[2]   r0*prj->w[1]
*      prj->w[3]   tol, cutoff for using small angle approximation, in
*                  radians.
*      prj->w[4]   prj->w[1]*tol
*      prj->w[5]   (180/pi)/prj->w[1]
*      prj->w[6]   theta_b
*===========================================================================*/

int astAirset(prj)

struct prjprm *prj;

{
   const double tol = 1.0e-4;
   double cxi;

   if (prj->r0 == 0.0) prj->r0 = R2D;

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 1 ){
      prj->w[6] = prj->p[ prj->axlat ][ 1 ];
      if( prj->w[6] == prj->unset ) prj->w[6] = 90.0;
   } else {
      prj->w[6] = 90.0;
   }

   if (prj->w[6] == 90.0) {
      prj->w[0] = -0.5;
      prj->w[1] =  1.0;
   } else if (prj->w[6] > -90.0) {
      cxi = cosd((90.0 - prj->w[6])/2.0);
      prj->w[0] = log(cxi)*(cxi*cxi)/(1.0-cxi*cxi);
      prj->w[1] = 0.5 - prj->w[0];
   } else {
      return 1;
   }

   prj->w[2] = prj->r0 * prj->w[1];
   prj->w[3] = tol;
   prj->w[4] = prj->w[1]*tol;
   prj->w[5] = R2D/prj->w[1];

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int astAirfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double cxi, r, txi, xi;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astAirset( prj );
      if( seterr ) return seterr;
   }

   if (theta == 90.0) {
      r = 0.0;
   } else if (theta > -90.0) {
      xi = D2R*(90.0 - theta)/2.0;
      if (xi < prj->w[3]) {
         r = xi*prj->w[2];
      } else {
         cxi = cosd((90.0 - theta)/2.0);
         txi = sqrt(1.0-cxi*cxi)/cxi;
         r = -prj->r0*(log(cxi)/txi + prj->w[0]*txi);
      }
   } else {
      return 2;
   }

   *x =  r*sind(phi);
   *y = -r*cosd(phi);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astAirrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   int   j;
   double cxi, lambda, r, r1, r2, rt, txi, x1, x2, xi;
   const double tol = 1.0e-12;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astAirset( prj );
      if( seterr ) return seterr;
   }

   r = sqrt(x*x + y*y)/prj->r0;

   if (r == 0.0) {
      xi = 0.0;
   } else if (r < prj->w[4]) {
      xi = r*prj->w[5];
   } else {
      /* Find a solution interval. */
      x1 = 1.0;
      r1 = 0.0;
      for (j = 0; j < 30; j++) {
         x2 = x1/2.0;
         txi = sqrt(1.0-x2*x2)/x2;
         r2 = -(log(x2)/txi + prj->w[0]*txi);

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
         rt = -(log(cxi)/txi + prj->w[0]*txi);

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

      xi = acosd(cxi);
   }

   if (r == 0.0) {
      *phi = 0.0;
   } else {
      *phi = atan2d(x, -y);
   }
   *theta = 90.0 - 2.0*xi;

   return 0;
}

/*============================================================================
*   CYP: cylindrical perspective projection.
*
*   Given:
*      prj->axlat        Index of latitude axis
*      prj->p[axlat][1]  Distance of point of projection from the centre of the
*                        generating sphere, mu, in units of r0 (error 401
*                        if not supplied).
*      prj->p[axlat][2]  Radius of the cylinder of projection, lambda, in units
*                        of r0 (error 402 if not supplied).
*      prj->n[axlat]     No. of parameters supplied.
*                        reported if either parameter is unset).
*      prj->unset        Value used to indicate an unset projection parameter
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*lambda*(pi/180)
*      prj->w[1]   (180/pi)/(r0*lambda)
*      prj->w[2]   r0*(mu + lambda)
*      prj->w[3]   1/(r0*(mu + lambda))
*      prj->w[4]   mu
*      prj->w[5]   lambda
*===========================================================================*/

int astCypset(prj)

struct prjprm *prj;

{

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 1 ){
      prj->w[4] = prj->p[ prj->axlat ][ 1 ];
      if( prj->w[4] == prj->unset ) return 401;
   } else {
      return 401;
   }

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 2 ){
      prj->w[5] = prj->p[ prj->axlat ][ 2 ];
      if( prj->w[5] == prj->unset ) return 402;
   } else {
      return 402;
   }

   if (prj->r0 == 0.0) {
      prj->r0 = R2D;

      prj->w[0] = prj->w[5];
      if (prj->w[0] == 0.0) {
         return 1;
      }

      prj->w[1] = 1.0/prj->w[0];

      prj->w[2] = R2D*(prj->w[4] + prj->w[5]);
      if (prj->w[2] == 0.0) {
         return 1;
      }

      prj->w[3] = 1.0/prj->w[2];
   } else {
      prj->w[0] = prj->r0*prj->w[5]*D2R;
      if (prj->w[0] == 0.0) {
         return 1;
      }

      prj->w[1] = 1.0/prj->w[0];

      prj->w[2] = prj->r0*(prj->w[4] + prj->w[5]);
      if (prj->w[2] == 0.0) {
         return 1;
      }

      prj->w[3] = 1.0/prj->w[2];
   }

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int astCypfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double s;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCypset( prj );
      if( seterr ) return seterr;
   }

   s = prj->w[4] + cosd(theta);
   if (s == 0.0) {
         return 2;
      }

   *x = prj->w[0]*phi;
   *y = prj->w[2]*sind(theta)/s;

   return 0;
}

/*--------------------------------------------------------------------------*/

int astCyprev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double eta, a;
   const double tol = 1.0e-13;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCypset( prj );
      if( seterr ) return seterr;
   }

   *phi   = x*prj->w[1];
   eta    = y*prj->w[3];
   a = eta*prj->w[4]/sqrt(eta*eta+1.0);

   if( fabs( a ) < 1.0 ) {
      *theta = atan2d(eta,1.0) + asind( a );
   
   } else if( fabs( a ) < 1.0 + tol ) {
      if( a > 0.0 ){
         *theta = atan2d(eta,1.0) + 90.0;
      } else {
         *theta = atan2d(eta,1.0) - 90.0;
      }

   } else {
      return 2;
   }

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

int astCarset(prj)

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

int astCarfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCarset( prj );
      if( seterr ) return seterr;
   }

   *x = prj->w[0]*phi;
   *y = prj->w[0]*theta;

   return 0;
}

/*--------------------------------------------------------------------------*/

int astCarrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCarset( prj );
      if( seterr ) return seterr;
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

int astMerset(prj)

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

int astMerfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astMerset( prj );
      if( seterr ) return seterr;
   }

   if (theta <= -90.0 || theta >= 90.0) {
      return 2;
   }

   *x = prj->w[0]*phi;
   *y = prj->r0*log(tand((90.0+theta)/2.0));

   return 0;
}

/*--------------------------------------------------------------------------*/

int astMerrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astMerset( prj );
      if( seterr ) return seterr;
   }

   *phi   = x*prj->w[1];
   *theta = 2.0*atand(exp(y/prj->r0)) - 90.0;

   return 0;
}

/*============================================================================
*   CEA: cylindrical equal area projection.
*
*   Given:
*      prj->axlat        Index of latitude axis
*      prj->p[axlat][1]  Square of the cosine of the latitude at which the
*                        projection is conformal, lambda (error 401 if
*                        not supplied).
*      prj->n[axlat]     No. of parameters supplied.
*      prj->unset        Value used to indicate an unset projection parameter
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/180)
*      prj->w[1]   (180/pi)/r0
*      prj->w[2]   r0/lambda
*      prj->w[3]   lambda/r0
*      prj->w[4]   lambda
*===========================================================================*/

int astCeaset(prj)

struct prjprm *prj;

{
   if( prj->np && prj->p && prj->np[ prj->axlat ] > 1 ){
      prj->w[4] = prj->p[ prj->axlat ][ 1 ];
      if( prj->w[4] == prj->unset ) return 401;
   } else {
      return 401;
   }

   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[0] = 1.0;
      prj->w[1] = 1.0;
      if (prj->w[4] <= 0.0 || prj->w[4] > 1.0) {
         return 1;
      }
      prj->w[2] = prj->r0/prj->w[4];
      prj->w[3] = prj->w[4]/prj->r0;
   } else {
      prj->w[0] = prj->r0*D2R;
      prj->w[1] = R2D/prj->r0;
      if (prj->w[4] <= 0.0 || prj->w[4] > 1.0) {
         return 1;
      }
      prj->w[2] = prj->r0/prj->w[4];
      prj->w[3] = prj->w[4]/prj->r0;
   }

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int astCeafwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCeaset( prj );
      if( seterr ) return seterr;
   }

   *x = prj->w[0]*phi;
   *y = prj->w[2]*sind(theta);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astCearev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double s;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCeaset( prj );
      if( seterr ) return seterr;
   }

   s = y*prj->w[3];
   if (fabs(s) > 1.0) {
      return 2;
   }

   *phi   = x*prj->w[1];
   *theta = asind(s);

   return 0;
}

/*============================================================================
*   COP: conic perspective projection.
*
*   Given:
*      prj->axlat        Index of latitude axis
*      prj->p[axlat][1]  sigma = (theta2+theta1)/2. Error 401 if not set.
*      prj->p[axlat][2]  delta = (theta2-theta1)/2, where theta1 and theta2 
*                        are the latitudes of the standard parallels, in 
*                        degrees. Defaults to zero.
*      prj->n[axlat]     No. of parameters supplied.
*      prj->unset        Value used to indicate an unset projection parameter
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   C  = sin(sigma)
*      prj->w[1]   1/C
*      prj->w[2]   Y0 = r0*cos(delta)*cot(sigma)
*      prj->w[3]   r0*cos(delta)
*      prj->w[4]   1/(r0*cos(delta)
*      prj->w[5]   cot(sigma)
*      prj->w[6]   sigma
*      prj->w[7]   delta
*===========================================================================*/

int astCopset(prj)

struct prjprm *prj;

{

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 1 ){
      prj->w[6] = prj->p[ prj->axlat ][ 1 ];
      if( prj->w[6] == prj->unset ) return 401;
   } else {
      return 401;
   }

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 2 ){
      prj->w[7] = prj->p[ prj->axlat ][ 2 ];
      if( prj->w[7] == prj->unset ) prj->w[7] = 0.0;
   } else {
      prj->w[7] = 0.0;
   }

   if (prj->r0 == 0.0) prj->r0 = R2D;

   prj->w[0] = sind(prj->w[6]);
   if (prj->w[0] == 0.0) {
      return 1;
   }

   prj->w[1] = 1.0/prj->w[0];

   prj->w[3] = prj->r0*cosd(prj->w[7]);
   if (prj->w[3] == 0.0) {
      return 1;
   }

   prj->w[4] = 1.0/prj->w[3];
   prj->w[5] = 1.0/tand(prj->w[6]);

   prj->w[2] = prj->w[3]*prj->w[5];

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int astCopfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double a, r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCopset( prj );
      if( seterr ) return seterr;
   }

   a = prj->w[0]*phi;
   r = prj->w[2] - prj->w[3]*tand(theta-prj->w[6]);

   *x =             r*sind(a);
   *y = prj->w[2] - r*cosd(a);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astCoprev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double a, dy, r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCopset( prj );
      if( seterr ) return seterr;
   }

   dy = prj->w[2] - y;
   r  = sqrt(x*x + dy*dy);
   if (prj->w[6] < 0.0) r = -r;

   if (r == 0.0) {
      a = 0.0;
   } else {
      a = atan2d(x/r, dy/r);
   }

   *phi   = a*prj->w[1];
   *theta = prj->w[6] + atand(prj->w[5] - r*prj->w[4]);

   return 0;
}

/*============================================================================
*   COD: conic equidistant projection.
*
*   Given:
*      prj->axlat        Index of latitude axis
*      prj->p[axlat][1]  sigma = (theta2+theta1)/2. Error 401 if not set.
*      prj->p[axlat][2]  delta = (theta2-theta1)/2, where theta1 and theta2 
*                        are the latitudes of the standard parallels, in 
*                        degrees. Defaults to zero.
*      prj->n[axlat]     No. of parameters supplied.
*      prj->unset        Value used to indicate an unset projection parameter
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   C = r0*sin(sigma)*sin(delta)/delta
*      prj->w[1]   1/C
*      prj->w[2]   Y0 = delta*cot(delta)*cot(sigma)
*      prj->w[3]   Y0 + sigma
*      prj->w[4]   sigma
*      prj->w[5]   delta
*===========================================================================*/

int astCodset(prj)

struct prjprm *prj;

{
   if( prj->np && prj->p && prj->np[ prj->axlat ] > 1 ){
      prj->w[4] = prj->p[ prj->axlat ][ 1 ];
      if( prj->w[4] == prj->unset ) return 401;
   } else {
      return 401;
   }

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 2 ){
      prj->w[5] = prj->p[ prj->axlat ][ 2 ];
      if( prj->w[5] == prj->unset ) prj->w[5] = 0.0;
   } else {
      prj->w[5] = 0.0;
   }

   if (prj->r0 == 0.0) prj->r0 = R2D;

   if (prj->w[5] == 0.0) {
      prj->w[0] = prj->r0*sind(prj->w[4])*D2R;
   } else {
      prj->w[0] = prj->r0*sind(prj->w[4])*sind(prj->w[5])/prj->w[5];
   }

   if (prj->w[0] == 0.0) {
      return 1;
   }

   prj->w[1] = 1.0/prj->w[0];
   prj->w[2] = prj->r0*cosd(prj->w[5])*cosd(prj->w[4])/prj->w[0];
   prj->w[3] = prj->w[2] + prj->w[4];

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int astCodfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double a, r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCodset( prj );
      if( seterr ) return seterr;
   }

   a = prj->w[0]*phi;
   r = prj->w[3] - theta;

   *x =             r*sind(a);
   *y = prj->w[2] - r*cosd(a);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astCodrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double a, dy, r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCodset( prj );
      if( seterr ) return seterr;
   }

   dy = prj->w[2] - y;
   r  = sqrt(x*x + dy*dy);
   if (prj->w[4] < 0.0) r = -r;

   if (r == 0.0) {
      a = 0.0;
   } else {
      a = atan2d(x/r, dy/r);
   }

   *phi   = a*prj->w[1];
   *theta = prj->w[3] - r;

   return 0;
}

/*============================================================================
*   COE: conic equal area projection.
*
*   Given:
*      prj->axlat        Index of latitude axis
*      prj->p[axlat][1]  sigma = (theta2+theta1)/2. Error 401 if not set.
*      prj->p[axlat][2]  delta = (theta2-theta1)/2, where theta1 and theta2 
*                        are the latitudes of the standard parallels, in 
*                        degrees. Defaults to zero.
*      prj->n[axlat]     No. of parameters supplied.
*      prj->unset        Value used to indicate an unset projection parameter
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   C = (sin(theta1) + sin(theta2))/2
*      prj->w[1]   1/C
*      prj->w[2]   Y0 = chi*sqrt(psi - 2C*sind(sigma))
*      prj->w[3]   chi = r0/C
*      prj->w[4]   psi = 1 + sin(theta1)*sin(theta2)
*      prj->w[5]   2C
*      prj->w[6]   (1 + sin(theta1)*sin(theta2))*(r0/C)**2
*      prj->w[7]   C/(2*r0**2)
*      prj->w[8]   chi*sqrt(psi + 2C)
*      prj->w[9]   sigma
*      prj->w[10]  delta
*===========================================================================*/

int astCoeset(prj)

struct prjprm *prj;

{
   double theta1, theta2;

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 1 ){
      prj->w[9] = prj->p[ prj->axlat ][ 1 ];
      if( prj->w[9] == prj->unset ) return 401;
   } else {
      return 401;
   }

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 2 ){
      prj->w[10] = prj->p[ prj->axlat ][ 2 ];
      if( prj->w[10] == prj->unset ) prj->w[10] = 0.0;
   } else {
      prj->w[10] = 0.0;
   }

   if (prj->r0 == 0.0) prj->r0 = R2D;

   theta1 = prj->w[9] - prj->w[10];
   theta2 = prj->w[9] + prj->w[10];

   prj->w[0] = (sind(theta1) + sind(theta2))/2.0;
   if (prj->w[0] == 0.0) {
      return 1;
   }

   prj->w[1] = 1.0/prj->w[0];

   prj->w[3] = prj->r0/prj->w[0];
   prj->w[4] = 1.0 + sind(theta1)*sind(theta2);
   prj->w[5] = 2.0*prj->w[0];
   prj->w[6] = prj->w[3]*prj->w[3]*prj->w[4];
   prj->w[7] = 1.0/(2.0*prj->r0*prj->w[3]);
   prj->w[8] = prj->w[3]*sqrt(prj->w[4] + prj->w[5]);

   prj->w[2] = prj->w[3]*sqrt(prj->w[4] - prj->w[5]*sind(prj->w[9]));

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int astCoefwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double a, r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCoeset( prj );
      if( seterr ) return seterr;
   }

   a = phi*prj->w[0];
   if (theta == -90.0) {
      r = prj->w[8];
   } else {
      r = prj->w[3]*sqrt(prj->w[4] - prj->w[5]*sind(theta));
   }

   *x =             r*sind(a);
   *y = prj->w[2] - r*cosd(a);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astCoerev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double a, dy, r, w;
   const double tol = 1.0e-12;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCoeset( prj );
      if( seterr ) return seterr;
   }

   dy = prj->w[2] - y;
   r  = sqrt(x*x + dy*dy);
   if (prj->w[9] < 0.0) r = -r;

   if (r == 0.0) {
      a = 0.0;
   } else {
      a = atan2d(x/r, dy/r);
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
         *theta = asind(w);
      }
   }

   return 0;
}

/*============================================================================
*   COO: conic orthomorphic projection.
*
*   Given:
*      prj->axlat        Index of latitude axis
*      prj->p[axlat][1]  sigma = (theta2+theta1)/2. Error 401 if not set.
*      prj->p[axlat][2]  delta = (theta2-theta1)/2, where theta1 and theta2 
*                        are the latitudes of the standard parallels, in 
*                        degrees. Defaults to zero.
*      prj->n[axlat]     No. of parameters supplied.
*      prj->unset        Value used to indicate an unset projection parameter
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
*      prj->w[5]   sigma
*      prj->w[6]   delta
*===========================================================================*/

int astCooset(prj)

struct prjprm *prj;

{
   double cos1, cos2, tan1, tan2, theta1, theta2;

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 1 ){
      prj->w[5] = prj->p[ prj->axlat ][ 1 ];
      if( prj->w[5] == prj->unset ) return 401;
   } else {
      return 401;
   }

   if( prj->np && prj->p && prj->np[ prj->axlat ] > 2 ){
      prj->w[6] = prj->p[ prj->axlat ][ 2 ];
      if( prj->w[6] == prj->unset ) prj->w[6] = 0.0;
   } else {
      prj->w[6] = 0.0;
   }

   if (prj->r0 == 0.0) prj->r0 = R2D;

   theta1 = prj->w[5] - prj->w[6];
   theta2 = prj->w[5] + prj->w[6];

   tan1 = tand((90.0 - theta1)/2.0);
   cos1 = cosd(theta1);

   if (theta1 == theta2) {
      prj->w[0] = sind(theta1);
   } else {
      tan2 = tand((90.0 - theta2)/2.0);
      cos2 = cosd(theta2);
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
   prj->w[2] = prj->w[3]*pow(tand((90.0 - prj->w[5])/2.0),prj->w[0]);
   prj->w[4] = 1.0/prj->w[3];

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int astCoofwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double a, r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCooset( prj );
      if( seterr ) return seterr;
   }

   a = prj->w[0]*phi;
   if (theta == -90.0) {
      if (prj->w[0] < 0.0) {
         r = 0.0;
      } else {
         return 2;
      }
   } else {
      r = prj->w[3]*pow(tand((90.0 - theta)/2.0),prj->w[0]);
   }

   *x =             r*sind(a);
   *y = prj->w[2] - r*cosd(a);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astCoorev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double a, dy, r;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astCooset( prj );
      if( seterr ) return seterr;
   }

   dy = prj->w[2] - y;
   r  = sqrt(x*x + dy*dy);
   if (prj->w[5] < 0.0) r = -r;

   if (r == 0.0) {
      a = 0.0;
   } else {
      a = atan2d(x/r, dy/r);
   }

   *phi = a*prj->w[1];
   if (r == 0.0) {
      if (prj->w[0] < 0.0) {
         *theta = -90.0;
      } else {
         return 2;
      }
   } else {
      *theta = 90.0 - 2.0*atand(pow(r*prj->w[4],prj->w[1]));
   }

   return 0;
}

/*============================================================================
*   BON: Bonne's projection.
*
*   Given:
*      prj->axlat        Index of latitude axis
*      prj->p[axlat][1]  Bonne conformal latitude, theta1, in degrees
*                        (error 401 if not supplied).
*      prj->n[axlat]     No. of parameters supplied.
*      prj->unset        Value used to indicate an unset projection parameter
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[1]   r0*pi/180
*      prj->w[2]   Y0 = r0*cot(theta1) + theta1
*      prj->w[3]   theta1
*===========================================================================*/

int astBonset(prj)

struct prjprm *prj;

{
   if( prj->np && prj->p && prj->np[ prj->axlat ] > 1 ){
      prj->w[3] = prj->p[ prj->axlat ][ 1 ];
      if( prj->w[3] == prj->unset ) return 401;
   } else {
      return 401;
   }

   if (prj->r0 == 0.0) {
      prj->r0 = R2D;
      prj->w[1] = 1.0;
      prj->w[2] = prj->r0*cosd(prj->w[3])/sind(prj->w[3]) + prj->w[3];
   } else {
      prj->w[1] = prj->r0*D2R;
      prj->w[2] = prj->r0*(cosd(prj->w[3])/sind(prj->w[3]) + prj->w[3]*D2R);
   }

   prj->flag = PRJSET;
   return 0;
}

/*--------------------------------------------------------------------------*/

int astBonfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double a, r;
   int seterr;

   if (prj->w[3] == 0.0) {
      /* Sanson-Flamsteed. */
      return astSflfwd(phi, theta, prj, x, y);
   }

   if (prj->flag != PRJSET) {
      seterr = astBonset( prj );
      if( seterr ) return seterr;
   }

   r = prj->w[2] - theta*prj->w[1];
   a = prj->r0*phi*cosd(theta)/r;

   *x =             r*sind(a);
   *y = prj->w[2] - r*cosd(a);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astBonrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double a, dy, costhe, r;
   int seterr;

   if (prj->w[3] == 0.0) {
      /* Sanson-Flamsteed. */
      return astSflrev(x, y, prj, phi, theta);
   }

   if (prj->flag != PRJSET) {
      seterr = astBonset( prj );
      if( seterr ) return seterr;
   }

   dy = prj->w[2] - y;
   r = sqrt(x*x + dy*dy);
   if (prj->w[3] < 0.0) r = -r;

   if (r == 0.0) {
      a = 0.0;
   } else {
      a = atan2d(x/r, dy/r);
   }

   *theta = (prj->w[2] - r)/prj->w[1];
   costhe = cosd(*theta);
   if (costhe == 0.0) {
      *phi = 0.0;
   } else {
      *phi = a*(r/prj->r0)/cosd(*theta);
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

int astPcoset(prj)

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

int astPcofwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double a, costhe, cotthe, sinthe;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astPcoset( prj );
      if( seterr ) return seterr;
   }

   costhe = cosd(theta);
   sinthe = sind(theta);
   a = phi*sinthe;

   if (sinthe == 0.0) {
      *x = prj->w[0]*phi;
      *y = 0.0;
   } else {
      cotthe = costhe/sinthe;
      *x = prj->r0*cotthe*sind(a);
      *y = prj->r0*(cotthe*(1.0 - cosd(a)) + theta*D2R);
   }

   return 0;
}

/*--------------------------------------------------------------------------*/

int astPcorev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   int   j;
   double f, fneg, fpos, lambda, tanthe, theneg, thepos, w, xp, xx, ymthe, yp;
   const double tol = 1.0e-12;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astPcoset( prj );
      if( seterr ) return seterr;
   }

   w = fabs(y*prj->w[1]);
   if (w < tol) {
      *phi = x*prj->w[1];
      *theta = 0.0;
   } else if (fabs(w-90.0) < tol) {
      *phi = 0.0;
      *theta = copysign(90.0,y);
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
         tanthe = tand(*theta);
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
         *phi = atan2d(yp, xp)/sind(*theta);
      }
   }

   return 0;
}

/*============================================================================
*   SFL: Sanson-Flamsteed ("global sinusoid") projection.
*
*   Given and/or returned:
*      prj->r0     r0; reset to 180/pi if 0.
*      prj->w[0]   r0*(pi/180)
*      prj->w[1]   (180/pi)/r0
*===========================================================================*/

int astSflset(prj)

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

int astSflfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astSflset( prj );
      if( seterr ) return seterr;
   }

   *x = prj->w[0]*phi*cosd(theta);
   *y = prj->w[0]*theta;

   return 0;
}

/*--------------------------------------------------------------------------*/

int astSflrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double w;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astSflset( prj );
      if( seterr ) return seterr;
   }

   w = cos(y/prj->r0);
   if (w == 0.0) {
      *phi = 0.0;
   } else {
      *phi = x*prj->w[1]/w;
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

int astParset(prj)

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

int astParfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double s;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astParset( prj );
      if( seterr ) return seterr;
   }

   s = sind(theta/3.0);
   *x = prj->w[0]*phi*(1.0 - 4.0*s*s);
   *y = prj->w[2]*s;

   return 0;
}

/*--------------------------------------------------------------------------*/

int astParrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double s, t;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astParset( prj );
      if( seterr ) return seterr;
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

   *theta = 3.0*asind(s);

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

int astAitset(prj)

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

int astAitfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   double costhe, w;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astAitset( prj );
      if( seterr ) return seterr;
   }

   costhe = cosd(theta);
   w = sqrt(prj->w[0]/(1.0 + costhe*cosd(phi/2.0)));
   *x = 2.0*w*costhe*sind(phi/2.0);
   *y = w*sind(theta);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astAitrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double s, u, xp, yp, z;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astAitset( prj );
      if( seterr ) return seterr;
   }

   u = 1.0 - x*x*prj->w[2] - y*y*prj->w[1];
   if (u < 0.0 ) {
      return 2;
   }
   z = sqrt(u);
   s = z*y/prj->r0;
   if (s < -1.0 || s > 1.0) {
      return 2;
   }

   xp = 2.0*z*z - 1.0;
   yp = z*x*prj->w[3];
   if (xp == 0.0 && yp == 0.0) {
      *phi = 0.0;
   } else {
      *phi = 2.0*atan2d(yp, xp);
   }
   *theta = asind(s);

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

int astMolset(prj)

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

int astMolfwd(phi, theta, prj, x, y)

double phi, theta, *x, *y;
struct prjprm *prj;

{
   int   j;
   double alpha, resid, u, v, v0, v1;
   const double tol = 1.0e-13;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astMolset( prj );
      if( seterr ) return seterr;
   }

   if (fabs(theta) == 90.0) {
      *x = 0.0;
      *y = copysign(prj->w[0],theta);
   } else if (theta == 0.0) {
      *x = prj->w[1]*phi;
      *y = 0.0;
   } else {
      u  = PI*sind(theta);
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

int astMolrev(x, y, prj, phi, theta)

double x, y, *phi, *theta;
struct prjprm *prj;

{
   double s, y0, z;
   const double tol = 1.0e-12;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astMolset( prj );
      if( seterr ) return seterr;
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
      z = copysign(1.0,z) + y0*s/PI;
   } else {
      z = asin(z)*prj->w[4] + y0*s/PI;
   }

   if (fabs(z) > 1.0) {
      if (fabs(z) > 1.0+tol) {
         return 2;
      }
      z = copysign(1.0,z);
   }

   *theta = asind(z);

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

int astCscset(prj)

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

int astCscfwd(phi, theta, prj, x, y)

double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   int   face;
   double costhe, eta, l, m, n, rho, xi;
   const double tol = 1.0e-7;
   int seterr;

   double a, a2, a2b2, a4, ab, b, b2, b4, ca2, cb2, x0, xf, y0, yf;
   const double gstar  =  1.37484847732;
   const double mm     =  0.004869491981;
   const double gamma  = -0.13161671474;
   const double omega1 = -0.159596235474;
   const double d0  =  0.0759196200467;
   const double d1  = -0.0217762490699;
   const double c00 =  0.141189631152;
   const double c10 =  0.0809701286525;
   const double c01 = -0.281528535557;
   const double c11 =  0.15384112876;
   const double c20 = -0.178251207466;
   const double c02 =  0.106959469314;

   if (prj->flag != PRJSET) {
      seterr = astCscset( prj );
      if( seterr ) return seterr;
   }

   costhe = cosd(theta);
   l = costhe*cosd(phi);
   m = costhe*sind(phi);
   n = sind(theta);

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
      xf = copysign(1.0,xf);
   }
   if (fabs(yf) > 1.0) {
      if (fabs(yf) > 1.0+tol) {
         return 2;
      }
      yf = copysign(1.0,yf);
   }

   *x = prj->w[0]*(x0 + xf);
   *y = prj->w[0]*(y0 + yf);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astCscrev(x, y, prj, phi, theta)

double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   int   face;
   double l, m, n;
   int seterr;

   double     a, b, xf, xx, yf, yy, z0, z1, z2, z3, z4, z5, z6;
   const double p00 = -0.27292696;
   const double p10 = -0.07629969;
   const double p20 = -0.22797056;
   const double p30 =  0.54852384;
   const double p40 = -0.62930065;
   const double p50 =  0.25795794;
   const double p60 =  0.02584375;
   const double p01 = -0.02819452;
   const double p11 = -0.01471565;
   const double p21 =  0.48051509;
   const double p31 = -1.74114454;
   const double p41 =  1.71547508;
   const double p51 = -0.53022337;
   const double p02 =  0.27058160;
   const double p12 = -0.56800938;
   const double p22 =  0.30803317;
   const double p32 =  0.98938102;
   const double p42 = -0.83180469;
   const double p03 = -0.60441560;
   const double p13 =  1.50880086;
   const double p23 = -0.93678576;
   const double p33 =  0.08693841;
   const double p04 =  0.93412077;
   const double p14 = -1.41601920;
   const double p24 =  0.33887446;
   const double p05 = -0.63915306;
   const double p15 =  0.52032238;
   const double p06 =  0.14381585;

   if (prj->flag != PRJSET) {
      seterr = astCscset( prj );
      if( seterr ) return seterr;
   }

   xf = x*prj->w[1];
   yf = y*prj->w[1];

   /* Determine the face. */
   if (xf > 7.0) {
      return 2;
   } else if (xf > 5.0) {
      if (fabs(yf) > 1.0) return 2;
      face = 4;
      xf = xf - 6.0;
   } else if (xf > 3.0) {
      if (fabs(yf) > 1.0) return 2;
      face = 3;
      xf = xf - 4.0;
   } else if (xf > 1.0) {
      if (fabs(yf) > 1.0) return 2;
      face = 2;
      xf = xf - 2.0;
   } else if (xf < -1.0) {
      return 2;
   } else if (yf > 1.0) {
      if (yf > 3.0) return 2;
      face = 0;
      yf = yf - 2.0;
   } else if (yf < -1.0) {
      if (yf < -3.0) return 2;
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
      *phi = atan2d(m, l);
   }
   *theta = asind(n);

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

int astQscset(prj)

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

int astQscfwd(phi, theta, prj, x, y)

double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   int   face;
   double chi, costhe, eta, l, m, n, p, psi, rho, rhu, t, x0, xf, xi, y0, yf;
   const double tol = 1.0e-12;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astQscset( prj );
      if( seterr ) return seterr;
   }

   if (fabs(theta) == 90.0) {
      *x = 0.0;
      *y = copysign(2.0*prj->w[0],theta);
      return 0;
   }

   costhe = cosd(theta);
   l = costhe*cosd(phi);
   m = costhe*sind(phi);
   n = sind(theta);

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
      yf  = (xf/15.0)*(atand(psi) - asind(psi/sqrt(chi+chi)));
   } else if (xi >= fabs(eta)) {
      psi = eta/xi;
      chi = 1.0 + psi*psi;
      xf  =  sqrt(rhu/(1.0-1.0/sqrt(1.0+chi)));
      yf  = (xf/15.0)*(atand(psi) - asind(psi/sqrt(chi+chi)));
   } else if (-eta > fabs(xi)) {
      psi = xi/eta;
      chi = 1.0 + psi*psi;
      yf  = -sqrt(rhu/(1.0-1.0/sqrt(1.0+chi)));
      xf  = (yf/15.0)*(atand(psi) - asind(psi/sqrt(chi+chi)));
   } else if (eta > fabs(xi)) {
      psi = xi/eta;
      chi = 1.0 + psi*psi;
      yf  =  sqrt(rhu/(1.0-1.0/sqrt(1.0+chi)));
      xf  = (yf/15.0)*(atand(psi) - asind(psi/sqrt(chi+chi)));
   }

   if (fabs(xf) > 1.0) {
      if (fabs(xf) > 1.0+tol) {
         return 2;
      }
      xf = copysign(1.0,xf);
   }
   if (fabs(yf) > 1.0) {
      if (fabs(yf) > 1.0+tol) {
         return 2;
      }
      yf = copysign(1.0,yf);
   }

   *x = prj->w[0]*(xf + x0);
   *y = prj->w[0]*(yf + y0);


   return 0;
}

/*--------------------------------------------------------------------------*/

int astQscrev(x, y, prj, phi, theta)

double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   int   direct, face;
   double chi, l, m, n, psi, rho, rhu, xf, yf, w;
   const double tol = 1.0e-12;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astQscset( prj );
      if( seterr ) return seterr;
   }

   xf = x*prj->w[1];
   yf = y*prj->w[1];

   /* Determine the face. */
   if (xf > 7.0) {
      return 2;
   } else if (xf > 5.0) {
      if (fabs(yf) > 1.0) return 2;
      face = 4;
      xf = xf - 6.0;
   } else if (xf > 3.0) {
      if (fabs(yf) > 1.0) return 2;
      face = 3;
      xf = xf - 4.0;
   } else if (xf > 1.0) {
      if (fabs(yf) > 1.0) return 2;
      face = 2;
      xf = xf - 2.0;
   } else if (xf < -1.0) {
      return 2;
   } else if (yf > 1.0) {
      if (yf > 3.0) return 2;
      face = 0;
      yf = yf - 2.0;
   } else if (yf < -1.0) {
      if (yf < -3.0) return 2;
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
         psi = sind(w)/(cosd(w) - SQRT2INV);
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
         psi = sind(w)/(cosd(w) - SQRT2INV);
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
      *phi = atan2d(m, l);
   }
   *theta = asind(n);

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

int astTscset(prj)

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

int astTscfwd(phi, theta, prj, x, y)

double phi, theta;
struct prjprm *prj;
double *x, *y;

{
   int   face;
   double costhe, l, m, n, rho, x0, xf, y0, yf;
   const double tol = 1.0e-12;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astTscset( prj );
      if( seterr ) return seterr;
   }

   costhe = cosd(theta);
   l = costhe*cosd(phi);
   m = costhe*sind(phi);
   n = sind(theta);

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
      xf = copysign(1.0,xf);
   }
   if (fabs(yf) > 1.0) {
      if (fabs(yf) > 1.0+tol) {
         return 2;
      }
      yf = copysign(1.0,yf);
   }

   *x = prj->w[0]*(xf + x0);
   *y = prj->w[0]*(yf + y0);

   return 0;
}

/*--------------------------------------------------------------------------*/

int astTscrev(x, y, prj, phi, theta)

double x, y;
struct prjprm *prj;
double *phi, *theta;

{
   double l, m, n, xf, yf;
   int seterr;

   if (prj->flag != PRJSET) {
      seterr = astTscset( prj );
      if( seterr ) return seterr;
   }

   xf = x*prj->w[1];
   yf = y*prj->w[1];

   /* Determine the face. */
   if (xf > 7.0) {
      return 2;
   } else if (xf > 5.0) {
      if (fabs(yf) > 1.0) return 2;
      /* face = 4 */
      xf = xf - 6.0;
      m  = -1.0/sqrt(1.0 + xf*xf + yf*yf);
      l  = -m*xf;
      n  = -m*yf;
   } else if (xf > 3.0) {
      if (fabs(yf) > 1.0) return 2;
      /* face = 3 */
      xf = xf - 4.0;
      l  = -1.0/sqrt(1.0 + xf*xf + yf*yf);
      m  =  l*xf;
      n  = -l*yf;
   } else if (xf > 1.0) {
      if (fabs(yf) > 1.0) return 2;
      /* face = 2 */
      xf = xf - 2.0;
      m  =  1.0/sqrt(1.0 + xf*xf + yf*yf);
      l  = -m*xf;
      n  =  m*yf;
   } else if (xf < -1.0) {
      return 2;
   } else if (yf > 1.0) {
      if (yf > 3.0) return 2;
      /* face = 0 */
      yf = yf - 2.0;
      n  = 1.0/sqrt(1.0 + xf*xf + yf*yf);
      l  = -n*yf;
      m  =  n*xf;
   } else if (yf < -1.0) {
      if (yf < -3.0) return 2;
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
      *phi = atan2d(m, l);
   }
   *theta = asind(n);

   return 0;
}
