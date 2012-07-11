/*
*     smf_math_functions

*  Purpose:
*     Library of functions and their partial derivatives, accessed
*     through the generic subroutine 'smf_math_function'. The routine
*     return the value, the derivatives, and/or the number of parameters
*     of the function in any combination.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*       void  smf_math_functions( smf_math_function fid, double xdat, const double fpar[],
*             int ncomp, double *value, double *deriv, int *npar,
*             const int iopt[], const double dopt[] );

*  Arguments:
*     fid  = smf_math_function Given
*        function identifier
*     xdat = double Given
*        coordinate to evaluate function at
*     fpar = const double[] Given
*        function parameters (depends on function called)
*     ncomp = int Given
*        number of components (see below) or order for polynomial.
*     value = double* Returned
*        function value at xdat
*     deriv = double* Returned
*        array of partial derivatives to parameters at xdat
*     npar = int* Returned
*        number of parameters associated with function
*     iopt = const int[] Given
*        optional integer parameter(s) to be passed-through to
*        the called function or derivate subroutine.
*     dopt = const double[] Given
*        optional double parameter(s) to be passed-through to
*        the called function or derivate subroutine.

*  Description:
*     This routines return the function value and partial derivatives
*     to the parameters at position xdat (which can be a multi-dimensional
*     array) and/or the number of the parameters associated with the
*     function (see below).
*
*     Currently the following functions are supported:
*         SMF__MATH_GAUSS         = 1-D Gaussian,
*         SMF__MATH_GAUSSHERMITE1 = 1-D Gaussian-hermite 1 (skewed gaussian),
*         SMF__MATH_GAUSSHERMITE2 = 1-D Gaussian-Hermite 2 (skewed and peaky gaussian),
*         SMF__MATH_VOIGT         = 1-D Voigt  (Gaussian+Lorentzian)
*         SMF__MATH_HISTOGRAM     = 1-D Histogram (semi-Poisson)
*         SMF__MATH_POLYNOMIAL    = 1-D Polynomial
*
*     Ncomp can be used to evaluate Sum(function): a profile that
*     consists of e.g. multiple gaussians. In that case fpar should have
*     a sets of parameters for ncomp versions of the function.
*
*     Note: for Polynomials 'ncomp' should have the order of the polynomial
*     to be evaluated.
*
*     Below the parameters (fpar) are defined for each function. If you fit
*     ncomp components fpar should have ncomp sets of parameters. Excepting
*     Polynomial and Histogram, it is possible to add upto a 2nd order
*     baseline, z0 + z1*x + z2 * x * x, in which case these parameters should
*     have been added to fpar after the list of function parameters. To add
*     a baseline set iopt[0] = 1, else set it to 0 or call the routine with
*     NULL in the iopt location.
*
*  Example calculate a single gaussian at 'x':
*        int fid = SMF__MATH_GAUSS;
*        double xdat = x;
*        int ncomp = 1;
*        int iopt[] = { 1 };
*                       Amp Xcen Disp  z0  z1  z2
*        double fpar = { 10,  2,  2.5,  3,  1,  2 };
*
*        double *value;
*        smf_math_func( fid, xdat, fpar, ncomp, value, NULL, NULL,
*                       iopt, NULL );
*
*     Value will be: 10 * exp(-0.5*((x-4)/2.5)**2  + 3 + x + 2*x**2
*
*     Calling the routine as
*        double *value;
*        double pderv[6];
*        smf_math_func( fid, xdat, fpar, ncomp, value, pderv, NULL,
*                       iopt, NULL );
*     will also return the 6 derivative values at 'x' in the array 'derv'.
*     The routine will skip calculations associated with return parameters
*     for which the pointer is NULL.
*
*     In general, for function f the returned value will be
*         F(x) = SUM[ f ]                           (iopt[0] = 0)
*         F(x) = SUM[ f ] + z0 + z1*x + z2*x*x      (iopt[0] = 1)
*     with the SUM running from 1..ncomp.
*
*     If npar is not NULL it will return the number of parameters associated
*     with the function. In its most general form this will be:
*              ncomp * nfuncpar + baseline
*     i.e. for a function that consists of 4 gaussians and a baseline
*     that will be 4 * 3 + 3 = 15. The baseline term will be omitted
*     (i.e. set to 0) if iopt = NULL or iopt[0] = 0. Likewise ncomp will
*     be assumed to be 1 if ncomp = 0. Thus
*
*     int npar;
*     int fid = SMF__MATH_GAUSS;
*     smf_math_func( fid, NULL, NULL, NULL, NULL, NULL, &npar,
*                    NULL, NULL );
*     will return npar = 3 as the number of parameters of a single
*     gaussian without a baseline.

*
*  Implemented Functions:
*
*  (Note: all functions use the Dispersion rather than FWHM -- equal to
*   DISP * 2.0*sqrt[2.0*log(2.0)] for a Gaussian -- )
*
*
*     - Gaussian (fid=SMF__MATH_GAUSS ):
*              fpar = { Amp A, Xcen xc, Disp s }
*                 g = (x-xc)/s
*                 f = A * Exp[ -0.5 * g^2 ]
*
*
*     - Gaussian-Hermite "1" (fid=SMF__MATH_GAUSSHERMITE1):
*              fpar = { Amp A, Xcen xc, Disp s, herm3 h3  }
*                 g = (x-xc)/s
*                 f =  A * Exp[ -0.5 * g^2 ] *
*                       (1 + h3 * [ c1 * g + c3*g^3 ] )
*              with c1 =-sqrt(3.0), c3 = 2.0*sqrt(3.0)/3.0,
*
*
*     - Gaussian-Hermite "2" (fid=SMF__MATH_GAUSSHERMITE2):
*              fpar = { Amp A, Xcen xc, Disp s, herm3 h3, herm4 h4 }
*                 g = (x-xc)/s
*                 f = A * Exp[ 0.5 * g^2 ] *
*                     { 1 + h3 * [ c1*g + c3*g^3 ] +
*                           h4 * [ c0 + c2*g^2 + c4*g^4 ] }
*              with c1 =-sqrt(3.0), c3 = 2.0*sqrt(3.0)/3.0,
*              c0 = sqrt(6.0)/4.0, c2 = -sqrt(6.0), c4 = sqrt(6.0)/3.0
*
*
*     - Voit (fid=SMF__MATH_VOIGT):
*              fpar = { Amp A, Xcen xc, Disp s, Lor l  }
*                 V = Integral {Gaussian * Lorentzian }
*                 g = (x-xc)/s
*                 Gaussian   = A.Exp[-0.5 * g^2]
*                 Lorentzian = l / pi*(g^2+l^2)
*
*
*    - Polynomial (fid=SMF__MATH_POLYNOMIAL):
*              fpar = { c0, c1, c2, c3, c4, ....)
*                 f = c0 + c1*x + c2*x^2 + c3*x^3 + ....
*      Note: ncomp = order of the polynomial(!). For compatibility
*      iopt = -1 will return 'ncomp', if ncomp is given, otherwise
*      it will return -1.
*
*
*    - Histogram (fid=SMF__MATH_HISTOGRAM):
*              fpar = { Amp A, Nu n, Beta a }
*                 f = A * x^n * exp(-b * x)
*      Values for iopt other than -1 will be ignored.
*

*  Notes:
*     Used by smf_lsqfit and smf_fit_profile. Major portions of the code
*     are derived (by permission) from the xgaufit routine of the GIPSY
*     software package of the Kapteyn Institute, Groningen, The Netherlands.

*  Authors:
*     Remo Tilanus (JAC, Hawaii)
*     Kor Begeman, Hans Terlouw, Martin Vogelaar (Kapteyn Institute, Groningen)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-09-27 (RPT):
*        Starlink version
*     2012-04-10 (TIMJ):
*        Use const and stop using unnecessary pointers.
*     2012-04-11 (TIMJ):
*        Use an enum for fid values.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010, 2012 Science and Technology Facilities Council.
*     Copyright (C) Kapteyn Laboratorium Groningen 2001
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <math.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "star/kaplibs.h"
#include "star/util.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

/* FIT1D includes */
#include "libsmf/smf_fit1d.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_math_functions_lib"

/* Gaussian */
static double gauss( double  X, const double fpar[], int ncomp, const int iopt[]);
static void gaussderv( double  X, const double fpar[], double *epar, int ncomp,
		       const int iopt[]);

/* Gaussian-Hermite "1" */
static double gausshermiteh3( double  X, const double fpar[], int ncomp, const int iopt[]);
static void gausshermiteh3derv( double  X, const double fpar[], double *epar,
                                int ncomp, const int iopt[]);

/* Gaussian-Hermite "2" */
static double gausshermiteh3h4( double  X, const double fpar[], int ncomp,
				const int iopt[]);
static void gausshermiteh3h4derv( double  X, const double fpar[], double *epar,
				  int ncomp, const int iopt[]);
/* Voigt plus helper function w*/
static double voigt( double  X, const double fpar[], int ncomp, const int iopt[]);
static void   voigtderv( double  X, const double fpar[], double *epar, int ncomp,
			 const int iopt[]);
static void w( double, double, double *, double * );

/* Polynomial */
static double polynomial( double  X, const double fpar[], int ncomp, const int iopt[]);
static void polyderv( double  X, double *epar, int ncomp);


/* Histogram: semi-poisson distribution */
static double histogram( double  X, const double fpar[], int ncomp, const int iopt[]);
static void histderv( double  X, const double fpar[], double *epar, int ncomp);


/* Returns function value at xdat */
void smf_math_functions( smf_math_function fid,
			 double  xdat,
			 const double fpar[],
			 int     ncomp,
			 double *value,
			 double *pderv,
			 int    *npar,
			 const int     iopt[],
			 const double  dopt[] __attribute__((unused)) )
/*--------------------------------------------------------------------
**
** Returns the value at xdat of the function identified by fid.
** Iopt and dopt are optional parameters that will be passed through
** to the function routine (not used so far).
**
**-------------------------------------------------------------------- */
{
  double   X;
  int      nopt;

  if ( xdat != VAL__BADD ) {
    X = xdat;
  } else {
    X = 0.0;
  }

  nopt = -1;

  if ( npar != NULL ) {

    /* Get number of parameters for basic function */
    if (fid == SMF__MATH_GAUSSHERMITE1)
      *npar = (int) ( gausshermiteh3( X, fpar, ncomp, &nopt ) + 0.5 );
    else if (fid == SMF__MATH_GAUSSHERMITE2)
      *npar = (int) ( gausshermiteh3h4( X, fpar, ncomp, &nopt ) + 0.5 );
    else if (fid == SMF__MATH_VOIGT)
      *npar = (int) ( voigt( X, fpar, ncomp, &nopt ) + 0.5 );
    else if (fid == SMF__MATH_POLYNOMIAL)
      *npar = (int) ( polynomial( X, fpar, ncomp, &nopt ) + 0.5 );
    else if (fid == SMF__MATH_HISTOGRAM)
      *npar = (int) ( histogram( X, fpar, ncomp, &nopt ) + 0.5 );
    else
      /* default to GAUSS */
      *npar = (int) ( gauss( X, fpar, ncomp, &nopt ) + 0.5 );

    /* Multiple with the number of instances of the function */
    if ( ncomp > 0 && fid != SMF__MATH_POLYNOMIAL ) *npar *= ncomp;

    if ( iopt != NULL && iopt[0] == 1 &&
         fid != SMF__MATH_POLYNOMIAL && fid != SMF__MATH_HISTOGRAM ) *npar += 3;

  }

  if ( value != NULL ) {

    if (fid == SMF__MATH_GAUSSHERMITE1)
      *value = gausshermiteh3( X, fpar, ncomp, iopt );
    else if (fid == SMF__MATH_GAUSSHERMITE2)
      *value = gausshermiteh3h4( X, fpar, ncomp, iopt );
    else if (fid == SMF__MATH_VOIGT)
      *value = voigt( X, fpar, ncomp, iopt );
    else if (fid == SMF__MATH_POLYNOMIAL)
      *value = polynomial( X, fpar, ncomp, iopt );
    else if (fid == SMF__MATH_HISTOGRAM)
      *value = histogram( X, fpar, ncomp, iopt );
    else
      /* default to GAUSS */
      *value = gauss( X, fpar, ncomp, iopt );

  }

  if ( pderv != NULL ) {

    if (fid == SMF__MATH_GAUSSHERMITE1)
      gausshermiteh3derv( X, fpar, pderv, ncomp, iopt );
    else if (fid == SMF__MATH_GAUSSHERMITE2)
      gausshermiteh3h4derv( X, fpar, pderv, ncomp, iopt );
    else if (fid == SMF__MATH_VOIGT)
      voigtderv( X, fpar, pderv, ncomp, iopt );
    else if (fid == SMF__MATH_POLYNOMIAL)
      polyderv( X, pderv, ncomp );
    else if (fid == SMF__MATH_HISTOGRAM)
      histderv( X, fpar, pderv, ncomp );
    else
      /* default to GAUSS */
      gaussderv( X, fpar, pderv, ncomp, iopt );
  }

}

/* ========================================================== */
/*                      F U N C T I O N S                     */
/* ========================================================== */


static double gauss( double  X,
                     const double fpar[],
                     int    ncomp,
                     const int    iopt[] )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate Gaussian at X.                          */
/*                                                            */
/* The function is a superposition of gaussians of the form   */
/*                                                            */
/* g = (x-xc)/s                                               */
/* f = A.Exp[-0.5 * g^2]                                      */
/*                                                            */
/* ==> F(x) = SUM[ f ] + z0 + z1*x + z2*x*x                   */
/*                                                            */
/* Amplitude        Center      dispersion                    */
/* A = fpar[0],  x0 = fpar[1],  s = fpar[2];                  */
/*                                                            */
/* etc.                                                       */
/* and the zero levels z0, z1, and z2 are stored in the last  */
/* elements of the input array: fpar[nfunc*3]+0,1,2           */
/*------------------------------------------------------------*/
{
   int      i;
   double   A, X0, s;
   double   result = 0.0;
   double   X_X0= 0.0;
   int      npar = 3;                  /* 3 components for 1 Gauss */

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   if ( iopt != NULL && iopt[0] == -1 ) return(npar);

   for (i = 0; i < nfunc; i++)
   {
     double  E, F, arg;                /* Exponent and its argument */
      int     offset = i * npar;

      A  = fpar[offset];  X0 = fpar[1+offset];  s = fpar[2+offset];
      X_X0 = X - X0;

      E = 0.0;
      if (A != 0.0 && s != 0.0)
      {
         F = X_X0 / s;
         arg = 0.5 * F*F;
         if (-arg > MINARG)         /* 'MINARG' is a global variable */
	 {
            E = exp( -arg );
	    result += A * E;
	 }
      }
   }
   if (iopt != NULL && iopt[0] == 1) {
     result += fpar[nfunc*npar+0] + fpar[nfunc*npar+1]*X_X0 +
               fpar[nfunc*npar+2]*X_X0*X_X0;
   }

   return( result );
}


static void gaussderv( double  X,
                       const double fpar[],
                       double *epar,
                       int     ncomp,
		       const int iopt[] )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate Gaussian derivatives at X.              */
/*                                                            */
/* The function is a superposition of gaussians of the form   */
/*                                                            */
/* g = (x-xc)/s                                               */
/* f = A.Exp[-0.5 * g^2]                                      */
/*                                                            */
/* ==> F(x) = SUM[ f ] + z0 + z1*x + z2*x*x                   */
/*                                                            */
/* Amplitude        Center      dispersion                    */
/* A = fpar[0],  x0 = fpar[1],  s = fpar[2];                  */
/*                                                            */
/* etc.                                                       */
/* and the zero level (z0), B, and A are stored in the last   */
/* elements of the input array: fpar[nfunc*3]+0,1,2           */
/*                                                            */
/* Let E = Exp[-(x-x0)^2(2s^2)], then                         */
/* D[F,A]  =  E                                               */
/* D[F,x0] =  A.(x-x0).E / s^2                                */
/* D[F,a]  =  A.(x-x0)^2.E / s^2                              */
/* D[F,z0] =  1                                               */
/* D[F,z1] =  (x-x0)                                          */
/* D[F,z2] =  (x-x0)^2                                        */
/*------------------------------------------------------------*/
{
   int      i;
   double   A, X0, X_X0=0.0, s;
   int      npar = 3;                  /* 3 components for 1 Gauss */

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   for (i = 0; i < nfunc; i++)
   {
      int     offset = i * npar;
      double  E, F, arg;               /* Exponent and its argument */
      double  AEX_X0;                  /* A*(X-Xc)*E  */

      A  = fpar[offset];  X0 = fpar[1+offset];  s = fpar[2+offset];

      X_X0 = X - X0;

      E = 0.0;
      epar[0+offset] = E;               /* Derivative wrt. Amplitude A */
      epar[1+offset] = 0.0;                     /* Derv. wrt Center Xc */
      epar[2+offset] = 0.0;                     /* Derv. wrt. dispersion */

      if ( s != 0.0 )
      {
        F = X_X0 / s;
	arg = 0.5 * F*F;
	if (-arg > MINARG)
	{
           E = exp( -arg );
           AEX_X0 = A * E * X_X0;
           epar[0+offset] = E;               /* Derivative wrt. Amplitude A */
  	   epar[1+offset] = AEX_X0/s/s;         /* Derv. wrt Center Xc */ 
	   epar[2+offset] = AEX_X0*X_X0/s/s/s;  /* Derv. wrt. dispersion */
	}
      }

   }

   /* Derivatives for the background */
   if (iopt != NULL && iopt[0] == 1) {
     epar[nfunc*npar+0] = 1.0;
     epar[nfunc*npar+1] = X_X0;
     epar[nfunc*npar+2] = X_X0*X_X0;
   }

}


static double gausshermiteh3( double  X,
                              const double fpar[],
                              int     ncomp,
			      const int iopt[] )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate Hermite polynomial at X.               */
/*                                                            */
/* The function is a superposition of functions of the form   */
/* g = (x-xc)/s                                               */
/* f =  A.Exp[-0.5 * g^2]* { 1 + h3[c1.g+c3.g^3] }            */
/*                                                            */
/* ==> F(x) = SUM[ f ] + z0 + z1*x + z2*x*x                   */
/*                                                            */
/* Amplitude        Center      dispersion    skewness        */
/* A = fpar[0],  X0 = fpar[1],  s = fpar[2], h3 = fpar[3]     */
/*                                                            */
/*------------------------------------------------------------*/
{
   int     i;
   int     npar = 4;               /* 4 parameters per component */
   double  result;
   double  X_X0 = 0.0;
   double  c1 = -sqrt(3.0);
   double  c3 = 2.0*sqrt(3.0)/3.0;

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   if ( iopt != NULL && iopt[0] == -1 ) return(npar);

   result = 0.0;
   for (i = 0; i < nfunc; i++)
   {
      double A, X0, s, h3;
      double arg;
      double E, F;

      A   = fpar[0+i*npar];
      X0  = fpar[1+i*npar];
      s   = fpar[2+i*npar];
      h3  = fpar[3+i*npar];

      X_X0 = X - X0;

      E = 0.0;
      if (A != 0.0 && s != 0.0)
      {
         F = X_X0 / s;
         arg = 0.5 * F*F;
         if (-arg > MINARG)         /* 'MINARG' is a global variable */
	 {
            E = exp( -arg );
	    E *= ( 1.0 + h3 * F * (c1 + c3*F*F) );
	    result += A * E;
	 }
      }
   }

   /* Add background components */
   if (iopt != NULL && iopt[0] == 1) {
     result += fpar[nfunc*npar+0] + fpar[nfunc*npar+1]*X_X0 +
               fpar[nfunc*npar+2]*X_X0*X_X0;
   }

   return( result );
}


static void gausshermiteh3derv( double  X,
                                const double fpar[],
                                double *epar,
                                int     ncomp,
				const int iopt[] )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate the derivatives for a skewed gauss at X */
/*------------------------------------------------------------*/
{
   int     i;
   int     npar = 4;               /* 4 parameters per component */
   double  c1 = -sqrt(3.0);
   double  c3 = 2.0*sqrt(3.0)/3.0;
   double  X_X0 = 0.0;

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   for (i = 0; i < nfunc; i++)
   {
      double A, X0, s, h3;
      double arg;
      double E, F, Q;

      A   = fpar[0+i*npar];
      X0  = fpar[1+i*npar];
      s   = fpar[2+i*npar];
      h3  = fpar[3+i*npar];

      X_X0 = X - X0;

      E = 0.0;
      epar[0+i*npar] = 0.0;
      epar[1+i*npar] = 0.0;
      epar[2+i*npar] = 0.0;
      epar[3+i*npar] = 0.0;

      if (s != 0.0)
      {
	F = X_X0 / s;
	arg = 0.5 * F*F;
         if (-arg > MINARG)         /* 'MINARG' is a global variable */
	 {
            E = exp( -arg );
	    Q = F * (c1 + c3*F*F);

	    /* Diff amplitude A */
	    epar[0+i*npar] = E * (1.0 + h3*Q);

	    /* Diff center X0 */
	    epar[1+i*npar] = (A*E/s) * ( -h3*(c1+3.0*c3*F*F) + F*(1.0+h3*Q) );

	    /* Diff sigma_0 s0 */
	    epar[2+i*npar] = F * epar[1+i*npar];

	    /*  Diff h3 */
	    epar[3+i*npar] = A*E*Q;
	 }
      }

   }
   /* Derivatives for the background */
   if (iopt != NULL && iopt[0] == 1) {
     epar[nfunc*npar+0] = 1.0;
     epar[nfunc*npar+1] = X_X0;
     epar[nfunc*npar+2] = X_X0*X_X0;
   }
}


double gausshermiteh3h4( double  X,
                         const double fpar[],
                         int     ncomp,
			 const int iopt[] )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate Hermite polynomial at X.                */
/*                                                            */
/* The function is a superposition of functions of the form   */
/*                                                            */
/* g = (x-xc)/s                                               */
/* f =  A.Exp[ -0.5 * g^2 ]*                                  */
/*      { 1 + h3[c1.g+c3.g^3] + h4[c0+c2.g^2+c4.g^4] }        */
/*                                                            */
/* ==> F(x) = SUM[ f ] + z0 + z1*x + z2*x*x                   */
/*                                                            */
/* Amplitude        Center      dispersion                    */
/* A = fpar[0],  X0 = fpar[1],  s = fpar[2], h3 = fpar[3]     */
/* h4 = fpar[4]                                               */
/*                                                            */
/*------------------------------------------------------------*/
{
   int     i;
   int     npar = 5;               /* 5 parameters per component */
   double  result;
   double  X_X0 = 0.0;
   double  c0 = sqrt(6.0)/4.0;
   double  c1 = -sqrt(3.0);
   double  c2 = -sqrt(6.0);
   double  c3 = 2.0*sqrt(3.0)/3.0;
   double  c4 = sqrt(6.0)/3.0;

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   if ( iopt != NULL && iopt[0] == -1 ) return(npar);

   result = 0.0;
   for (i = 0; i < nfunc; i++)
   {
      double A, X0, s, h3, h4;
      double arg;
      double E, F;

      A   = fpar[0+i*npar];
      X0  = fpar[1+i*npar];
      s   = fpar[2+i*npar];
      h3  = fpar[3+i*npar];
      h4  = fpar[4+i*npar];

      X_X0 = X - X0;

      E = 0.0;
      if (A != 0.0 && s != 0.0)
      {
	 F = X_X0 / s;
         arg = 0.5 * F * F;
         if (-arg > MINARG)         /* 'MINARG' is a global variable */
	 {
            E = exp( -arg );
	    E *= ( 1.0 + h3*F*(c3*F*F+c1) + h4*(c0+F*F*(c2+c4*F*F)) );
	    result += A * E;
         }
      }
   }
   /* Add background components */
   if (iopt != NULL && iopt[0] == 1) {
     result += fpar[nfunc*npar+0] + fpar[nfunc*npar+1]*X_X0 +
               fpar[nfunc*npar+2]*X_X0*X_X0;
   }

   return( result );
}


static void gausshermiteh3h4derv( double  X,
                                const double fpar[],
                                double   *epar,
				  int     ncomp,
				  const int iopt[] )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate the derivatives for a skewed gauss at X */
/*------------------------------------------------------------*/
{
   int     i;
   int     npar = 5;               /* 5 parameters per component */
   double  X_X0 = 0.0;
   double  c0 = sqrt(6.0)/4.0;
   double  c1 = -sqrt(3.0);
   double  c2 = -sqrt(6.0);
   double  c3 = 2.0*sqrt(3.0)/3.0;
   double  c4 = sqrt(6.0)/3.0;

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   for (i = 0; i < nfunc; i++)
   {
      double A, X0, s, h3, h4;
      double arg;
      double E, F, Q3, Q4;

      A   = fpar[0+i*npar];
      X0  = fpar[1+i*npar];
      s   = fpar[2+i*npar];
      h3  = fpar[3+i*npar];
      h4  = fpar[4+i*npar];


      X_X0 = X - X0;

      E = 0.0;
      epar[0+i*npar] = 0.0;
      epar[1+i*npar] = 0.0;
      epar[2+i*npar] = 0.0;
      epar[3+i*npar] = 0.0;
      epar[4+i*npar] = 0.0;

      if (s != 0.0)
      {
         arg = 0.5 * (X_X0/s) * (X_X0/s);
         if (-arg > MINARG)         /* 'MINARG' is a global variable */
	 {
            E = exp( -arg );
	    F = X_X0 / s;
	    Q3 = F * (c3*F*F+c1);
	    Q4 = c0 + F*F*(c2+c4*F*F);

	    /* Diff amplitude A */
	    epar[0+i*npar] = E * (1.0 + h3*Q3 + h4*Q4);

	    /* Diff center X0 */
	    epar[1+i*npar] = (A*E/s) *(
                         -h3*(c1+3.0*c3*F*F) - 2.0*h4*F*(c2+2.0*c4*F*F)
                         + F*(1.0+h3*Q3+h4*Q4) );

	    /* Diff sigma */
	    epar[2+i*npar] = F * epar[1+i*npar];

	    /*  Diff h3 */
	    epar[3+i*npar] = A*E*Q3;

	    /*  Diff h4 */
	    epar[4+i*npar] = A*E*Q4;
	 }
      }
   }

   /* Derivatives for the background */
   if (iopt != NULL && iopt[0] == 1) {
     epar[nfunc*npar+0] = 1.0;
     epar[nfunc*npar+1] = X_X0;
     epar[nfunc*npar+2] = X_X0*X_X0;
   }

}


static void w( double Rz,
               double Iz,
               double *Rr,
               double *Ir )
/*------------------------------------------------------------*/
/* PURPOSE: Error function for complex arguments.             */
/*          W(Z) = EXP(-Z*Z)ERFC(-iZ)                         */
/*          see Abromowitz and Stegun (chapter 7)             */
/*          |error|  <  2 * 10e-6                             */
/*------------------------------------------------------------*/
{
#define EPS     ( 0.0000001 )
   double	wdx = 0.0;		/* real part result */
   double	wdy = 0.0;		/* imag part result */
   double	x, y;			/* real and imaginary arg */

   x = fabs( Rz );                      /* real part */
   y = fabs( Iz );                      /* imaginairy part */
   if ( x > 6.0 || y > 6.0 ) {		/* approximation for large arguments */
      static double	at[] = { 0.5124242, 0.05176536 };
      static double	bt[] = { 0.2752551, 2.72474500 };
      int		i;

      for ( i = 0; i < 2; i++ ) {
         double	det, dtx, dty, sav;

         sav = ( x * x - y * y - bt[i] );
         det = ( sav * sav + 4.0 * x * x * y * y );
         dtx = ( 2.0 * x * x * y - y * sav ) * at[i];
         dty = ( 2.0 * x * y * y + x * sav ) * at[i];
         wdx += dtx / det;
         wdy += dty / det;
      }
   } else if ( x > 3.9 || y > 3.0 ) {	/* approximation for intermediate arguments */
      static double	at[] = { 0.4613135, 0.09999216, 0.002883894 };
      static double	bt[] = { 0.1901635, 1.78449270, 5.525343700 };
      int		i;

      for ( i = 0; i < 3; i++ ) {
         double	det, dtx, dty, sav;

         sav = ( x * x - y * y - bt[i] );
         det = ( sav * sav + 4.0 * x * x * y * y );
         dtx = ( 2.0 * x * x * y - y * sav ) * at[i];
         dty = ( 2.0 * x * y * y + x * sav ) * at[i];
         wdx += dtx / det;
         wdy += dty / det;
      }
   } else {				/* no approximation */
      double	r;

      wdx = 1.0;
      r = sqrt( x * x + y * y );
      if ( r > 0.0 ) {
         static double	tt[] = { 1.0000000000, 0.5641895835 };
         double		tn[2];
         double		del, csp, snp, tcn, tsn;
         int		n = 0;

         csp = -y / r;
         snp = x / r;
         tcn = 1.0; tsn = 0.0;
         tn[0] = tt[0]; tn[1] = tt[1];
         do {
            double	tc, ts;
            int		i;

            n += 1;				/* increment interation number */
            tc = tcn;				/* save */
            ts = tsn;				/* save */
            tcn = ( tc * csp - ts * snp );	/* next cosine term */
            tsn = ( ts * csp + tc * snp );	/* next sine term */
            i = n%2;				/* argument */
            tn[i] *= ( 2.0 / (double) n );
            tn[0] *= r;				/* multiply with radius */
            tn[1] *= r;				/* multiply with radius */
            del = tn[i];			/* increment */
            wdx += ( tcn * del );		/* add increment */
            wdy += ( tsn * del );		/* add increment */
         } while ( del > EPS );			/* precision is reached */
      }
   }
   if ( Rz >= 0.0 && Iz >= 0.0 ) {
      *Rr = wdx; *Ir = wdy;
   } else if ( Rz >= 0.0 && Iz < 0.0 ) {
      double	csp, snp, sav;

      csp = cos( 2.0 * x * y );
      snp = sin( 2.0 * x * y );
      sav = exp( y * y - x * x );
      *Rr = sav * csp - wdx;
      *Ir = sav * snp + wdy;
   } else if ( Rz < 0.0 && Iz >= 0.0 ) {
      *Rr = wdx;
      *Ir = -wdy;
   } else if ( Rz < 0.0 && Iz < 0.0 ) {
      double	csp, snp, sav;

      csp = cos( 2.0 * x * y );
      snp = sin( 2.0 * x * y );
      sav = exp( y * y - x * x );
      *Rr = sav * csp - wdx;
      *Ir = -sav * snp - wdy;
   }
#undef EPS
}


static double voigt( double  X,
                     const double fpar[],
                     int     ncomp,
		     const int iopt[] )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate Voigt function at X.                    */
/*                                                            */
/* The function is a superposition of Voit functions (V(x))   */
/*     V = Integral {Gaussian * Lorentzian }                  */
/* g = (x-xc)/s                                               */
/* Gaussian   = A.Exp[-0.5 * g^2]                             */
/* Lorentzian = l / pi*(g^2+l^2)                              */
/*                                                            */
/* ==> F(x) = SUM[ V ] + z0 + z1*x + z2*x*x                   */
/*                                                            */
/* Amplitude        Center      dispersions                   */
/* A = fpar[0],  X0 = fpar[1],  s = fpar[2], l = fpar[3]      */
/*                                                            */
/*------------------------------------------------------------*/
{
   int      npar = 4;                            /* Per component */
   int      i;
   double   x, y;
   double   result, V;
   double   sqln2 = sqrt( log(2.0) );
   double   Rres, Ires;
   double   X_X0 = 0.0;

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   if ( iopt != NULL && iopt[0] == -1 ) return(npar);

   result = 0.0;
   for (i = 0; i < nfunc; i++)
   {
      int      offset = i * npar;
      double   Int, X0;
      double   aD, aL;
      Int = fpar[0+offset];    X0 = fpar[1+offset];
      aD  = fpar[2+offset];    aL = fpar[3+offset];
      X_X0 = X - X0;

      if (aD == 0.0)
         V = 0.0;
      else
      {
         double ampfct;

         x = X_X0 * sqln2 / aD;
         y = aL * sqln2 / aD;
         w( x, y, &Rres, &Ires );
         ampfct = Int * sqln2/(aD*sqrt(M_PI));
         V = ampfct * Rres;
      }
      result += V;
   }
   /* Add background components */
   if (iopt != NULL && iopt[0] == 1) {
     result += fpar[nfunc*npar+0] + fpar[nfunc*npar+1]*X_X0 +
               fpar[nfunc*npar+2]*X_X0*X_X0;
   }

   return( result );
}



static void   voigtderv( double  X,
                         const double fpar[],
                         double *epar,
                         int     ncomp,
			 const int iopt[] )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate Voigt derivatives at X.                 */
/*------------------------------------------------------------*/
{
   int      npar = 4;                            /* Per component */
   int      i;
   double   x, y;
   double   sqln2 = sqrt( log(2.0) );
   double   X_X0 = 0.0;
   double   sqpi = sqrt(M_PI);

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   for (i = 0; i < nfunc; i++)
   {
      int      offset = i * npar;
      double   Int, X0;
      double   aD, aL;

      Int = fpar[0+offset];    X0 = fpar[1+offset];
      aD  = fpar[2+offset];    aL = fpar[3+offset];
      X_X0 = X - X0;

      if (aD == 0.0)
	epar[0+offset] = epar[1+offset] = epar[2+offset] = epar[3+offset] = 0.0;
      else
      {
         double Rezwz,Imzwz;
         double ampfct;
         double Rew, Imw;
         double dxVoigt, dyVoigt;

         x = X_X0 * sqln2 / aD;              /* Definitions for x and y */
         y = aL * sqln2 / aD;
         w( x, y, &Rew, &Imw );
         Rezwz  = ( x*Rew - y*Imw );         /* Real part of x.w(z) */
         Imzwz  = ( x*Imw + y*Rew );         /* Imaginary part of x.w(z) */
         dxVoigt = -2.0 * Rezwz;             /* dVoigt/dx */
         dyVoigt = -2.0/sqpi + 2.0 * Imzwz;  /* dVoigt/dy */
         ampfct = Int * sqln2/(aD*sqpi);     /* Help variable */


         /* The partial derivatives: */

         epar[0+offset] = sqln2/(aD*sqpi) * Rew;             /* Amplitude */
         epar[1+offset] = ampfct * (-sqln2/aD) * dxVoigt;    /* Center    */
         epar[2+offset] = (-ampfct/aD) *
                          (Rew + x*dxVoigt+ y*dyVoigt);      /* Doppler factor    */
         epar[3+offset] = ampfct * (sqln2/aD) * dyVoigt;     /* Lorentz factor    */
      }
   }

   /* Derivatives for the background */
   if (iopt != NULL && iopt[0] == 1) {
     epar[nfunc*npar+0] = 1.0;
     epar[nfunc*npar+1] = X_X0;
     epar[nfunc*npar+2] = X_X0*X_X0;
   }

}


/* Polynomial */
static double polynomial( double  X,
			  const double fpar[],
			  int     ncomp,
                          const int iopt[] )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate Polynomial function at X.               */
/*                                                            */
/* n = nfunc (order of the polynomial)                        */
/* f = c0 + c1*X + c2*X^2 + ... * c_n*X^n                     */
/*                                                            */
/* c0 = fpar[0], c1 = fpar[1], ... , c_n = fpar[n]            */
/*------------------------------------------------------------*/
{
   int n;
   double x_n;
   double result;

   if ( iopt != NULL && iopt[0] == -1 ) {
     if ( ncomp > 0 ) {
       return( ncomp );
     } else {
       return( -1 );
     }
   }

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   result = fpar[0];
   x_n = 1;
   for ( n = 1; n < nfunc; n++ ) {
     x_n *= X;
     result += fpar[n] * x_n;
   }

   return( result );
}


static void polyderv( double  X,
		      double *epar,
		      int     ncomp )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate Polynomial derivatives at X.            */
/*------------------------------------------------------------*/
{
   int n;
   double x_n;

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   x_n = 1;
   for ( n = 0; n < nfunc; n++ ) {
     epar[n] = x_n;
     x_n *= X;
   }
}


static double histogram( double  X,
                         const double fpar[],
			 int     ncomp,
			 const int iopt[] )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate Histogram function at X.                */
/*                                                            */
/* The function is a superposition of gaussians of the form   */
/*                                                            */
/* f = A . x^n * Exp[-b * x]                                  */
/*                                                            */
/* ==> F(x) = SUM[ f ]                                        */
/*                                                            */
/* Amplitude        Center      dispersion                    */
/* A = fpar[0],  n = fpar[1],  b = fpar[2];                   */
/*                                                            */
/*------------------------------------------------------------*/
{
  int      npar = 3;               /* 3 components for 1 Hist */

  int nfunc = 1;
  if ( ncomp > 0 ) nfunc = ncomp;

  if ( iopt != NULL && iopt[0] == -1 ) return(npar);

   int      i;
   double   A, n, b;
   double   result = 0.0;


   if ( iopt != NULL && iopt[0] == -1 ) return(npar);

   for (i = 0; i < nfunc; i++)
   {
      double  E, arg;                  /* Exponent and its argument */
      int     offset = i * npar;

      A  = fpar[offset];  n = fpar[1+offset];  b = fpar[2+offset];

      arg = b * X;
      if (-arg > MINARG)         /* 'MINARG' is a global variable */
	E = exp( -arg );
      else
	E = 0.0;

      result += A * pow(X,n) * E;
   }

   return( result );

}

static void histderv( double  X,
                      const double fpar[],
                      double *epar,
                      int     ncomp )
/*------------------------------------------------------------*/
/* PURPOSE: Calculate Histogram derivatives at X.             */
/*------------------------------------------------------------*/
{
   int      i;
   double   A, n, b;
   int      npar = 3;                  /* 3 components for 1 Hist */

   int nfunc = 1;
   if ( ncomp > 0 ) nfunc = ncomp;

   for (i = 0; i < nfunc; i++)
   {
      int     offset = i * npar;
      double  E, arg;                  /* Exponent and its argument */

      A  = fpar[offset];  n = fpar[1+offset];  b = fpar[2+offset];

      arg = b * X;
      if (-arg > MINARG)
	E = exp( -arg );
      else
	E = 0.0;

      /* x^n = exp(ln(x^n)) = exp(n.ln(x)) => d/dn x^n = ln(x).x^n */

      double f = A * pow(X,n) * E;

      epar[0+offset] = f / A;        /* Derivative wrt. Amplitude A */
      epar[1+offset] = log(X) * f;          /* Derv. wrt exponent n */
      epar[2+offset] = -X * f;                      /* Derv. wrt. b */
   }

}
