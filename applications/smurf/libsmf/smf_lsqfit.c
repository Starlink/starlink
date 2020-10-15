/* smf_lsqfit.c
*+
*  Name:
*     smf_lsqfit

*  Purpose:
*     smf_lsqfit is a routine for making a least-squares fit of a
*     function to a set of data points using a mixture of the steepest
*     descent method and the Taylor method.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*       (int) smf_lsqfit( smf_math_function fid, const double xdat[], int xdim, const double ydat[],
*             const float wdat[], int ndat, double *fpar, double *epar,
*             const int mpar[], int npar, int ncomp, float tol, int its,
*             float lab, const int iopt[], const double dopt[] )

*  Arguments:
*     fid  = smf_math_function (Given)
*        Function id (see smf_math_functions.c)
*     xdat = const double []   (Given)
*        Contains coordinates of data points. The fitted FWHM
*        and CENTRE will be expressed in these coordinates, but for
*        a simple fit this can be the pixel coordinates of the points.
*     xdim = int       (Given)
*        Dimension of fit (INT). E.g. spectrum = 1, map = 2, etc.
*     ydat = const double []  (Given)
*        Pointer to input data
*     wdat = const float []    (Given)
*        Contains weigths for data points.
*     ndat = int       (Given)
*        Number of data points.
*     fpar = double*   (Given and Returned)
*        On input contains initial estimates of the parameters for
*        non-linear fits, on output the fitted parameters.
*     epar = double*   (Returned)
*        Contains estimates of errors in fitted parameters.
*     mpar = const int []      (Given)
*        Logical mask telling which parameters are free
*        (MPAR(J)=non-zero) and which parameters are fixed
*        (MPAR(J)=0).
*     npar = int       (Given)
*        Number of parameter (fixed+free)
*     ncomp = int      (Given)
*        Number of components.
*     tol = float      (Given)
*        Relative tolerance. smf_lsqfit stops successive iterations
*        fail to produce a decrement in reduced chi-squared less than
*        TOL. If TOL is less than the minimum tolerance possible, TOL
*        will be set to this value. This means that maximum accuracy
*        can be obtained by setting TOL=0.0.
*     its = int        (Given)
*        Maximum number of iterations.
*     lab = float      (Given)
*        Mixing parameter, LAB determines the initial weight of
*        steepest descent method relative to the Taylor method. LAB
*        should be a small value (i.e. 0.01). LAB can only be zero
*        when the partial derivatives are independent of the
*        parameters. In fact in this case LAB should be exactly equal
*        to zero.
*     iopt = const int []      (Given)
*        An int array which is passed unmodified to smf_math_... (see below).
*     dopt = const double []   (Given)
*        A double array which is passed unmodified to smf_math_... (see below).

*  Returned value:
*     smf_lsqfit = int (Returned)
*        When this number is negative, the fitting was not  continued
*        because a fatal error occurred:
*        > 0 Number of iterations needed to achieve convergence
*            according to TOL.
*        -1 Too many free parameters, maximum is 32.
*        -2 No free parameters.
*        -3 Not enough degrees of freedom.
*        -4 Maximum number of iterations too small to obtain a
*           solution which satisfies TOL.
*        -5 Diagonal of matrix contains elements which are zero.
*        -6 Determinant of the coefficient matrix is zero.
*        -7 Square root of negative number.

*  Description:
*     smf_lsqfit is a routine for making a least-squares fit of function
*     to a set of data points. The method used is described in:
*     Marquardt, J.Soc.Ind.Appl.Math. 11, 431 (1963).  This method is
*     a mixture of the steepest descent method and the Taylor method.
*
*     The following routines have to be defined by the user:
*
*             DOUBLE  SMF_MATH_FVALUE( FID,     Input    *INTEGER
*                                     XDAT ,   Input    DOUBLE ARRAY
*                                     FPAR ,   Input    DOUBLE ARRAY
*                                     NCOMP ,  Input    *INTEGER
*                                     IOPT ,   Input    INTEGER ARRAY
*                                     DOPT )   Input    DOUBLE ARRAY
*
*             VOID SMF_MATH_FPDERV( FID,     Input    *INTEGER
*                                   XDAT ,   Input    DOUBLE ARRAY
*                                   FPAR ,   Input    DOUBLE ARRAY
*                                   NCOMP ,   Input    INTEGER
*                                   EPAR ,   Output   DOUBLE ARRAY
*                                   IOPT ,   Input    INTEGER ARRAY
*                                   DOPT )   Input    DOUBLE ARRAY
*
*             FUNC    Returns the function value of the function to
*                     be fitted.
*            *FID     Function identifier
*             XDAT    Coordinate(s) of data point.
*             FPAR    Parameter list.
*             EPAR    Partial derivatives to the parameters of the
*                     function to be fitted. (Returned)
*            *NCOMP   Number of components
*             IOPT    User defined parameters
*             DOPT    User defined parameters.
*
*  Notes:
*     If compiled as 'gcc -o fit -DTESTBED smf_lsqfit.c -lm' a
*     standalone executable 'fit' will be produced which will do 5
*     fits to gaussians with random errors added to the data and
*     initial guesses. The underlying parameters for the gaussians are
*     zerolevel: amplitude = 10, centre = 15, and dispersion = 2.0.

*  Authors:
*     Kor Begeman  (Kapteyn Institute, Groningen)
*     Remo Tilanus (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*   1990-05-07 (KGB)
*     Initial version for GIPSY
*   2010-09-24 (RPT)
*     Adapted for SMURF
*   2012-04-10 (TIMJ):
*        Use const and stop using unnecessary pointers.

*  Copyright:
*     Copyright (C) 2010,2012 Science and Technology Facilities Council.
*     Copyright (c) Kapteyn Laboratorium Groningen 1990
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
*     Software Lsq->Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#include	"stdio.h"			/* <stdio.h> */
#include	"stdlib.h"			/* <stdlib.h> */
#include	"math.h"			/* <math.h> */
#include	"float.h"			/* <float.h> */

#if defined (TESTBED)

static double smf_math_fvalue( smf_math_function fid,
			       double xdat,
			       const double fpar[],
			       int    ncomp,
			       const int    iopt[],
			       const double dopt[] __attribute__((unused)) );

static void smf_math_fpderv( smf_math_function fid,
			     double xdat,
			     const double fpar[],
			     int    ncomp,
			     double *epar,
			     const int    iopt[] __attribute__((unused)),
			     const double dopt[] __attribute__((unused)) );

static int smf_math_fnpar( smf_math_function fid );

#else

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"

#endif

#define	LABFAC	10.0				/* labda step factor */
#define	LABMAX	1.0e+10				/* maximum value for labda */
#define	LABMIN	1.0e-10				/* minimum value for labda */
#define	MAXPAR	53				/* number of free parameters */

typedef struct {
  double	chi1;			        /* old reduced chi-squared */
  double	chi2;			        /* new reduced chi-squared */
  double	labda;			        /* mixing parameter */
  double	tolerance;		        /* accuracy */
  double	vector[MAXPAR];		        /* correction lsq->vector */
  double	matrix1[MAXPAR][MAXPAR];        /* original matrix */
  double	matrix2[MAXPAR][MAXPAR];        /* inverse of lsq->matrix1 */
  int           itc;			        /* fate of fit */
  int 	        found;			        /* solution lsq->found ? */
  int 	        nfree;			        /* number of free parameters */
  int 	        nuse;			        /* nr of useable data points */
  int 	        parptr[MAXPAR];		        /* parameter pointer */
} lsqData;


static int  invmat( lsqData *lsq )
/*
 * invmat calculates the inverse of matrix2. The algorithm used is the
 * Gauss-Jordan algorithm described in Stoer, Numerische matematik, 1 Teil.
 */
{
   double even;
   double hv[MAXPAR];
   double mjk;
   double rowmax;
   int    evin;
   int    i;
   int    j;
   int    k;
   int    per[MAXPAR];
   int    row;

   for (i = 0; i < lsq->nfree; i++) per[i] = i;	/* set permutation array */
   for (j = 0; j < lsq->nfree; j++) {		/* in j-th column, ... */
      rowmax = fabs( lsq->matrix2[j][j] );	/* determine row with ... */
      row = j;					/* largest element. */
      for (i = j + 1; i < lsq->nfree; i++) {
         if (fabs( lsq->matrix2[i][j] ) > rowmax) {
            rowmax = fabs( lsq->matrix2[i][j] );
            row = i;
         }
      }
      if (lsq->matrix2[row][j] == 0.0)
	return( -6 );                           /* determinant is zero! */

      if (row > j) {			        /* if largest element not .. */
         for (k = 0; k < lsq->nfree; k++) {     /* on diagonal, then ... */
            even = lsq->matrix2[j][k];		/* permutate rows. */
            lsq->matrix2[j][k] = lsq->matrix2[row][k];
            lsq->matrix2[row][k] = even;
         }
         evin = per[j];				/* keep track of permutation */
         per[j] = per[row];
         per[row] = evin;
      }
      even = 1.0 / lsq->matrix2[j][j];		/* modify column */
      for (i = 0; i < lsq->nfree; i++)
	lsq->matrix2[i][j] *= even;
      lsq->matrix2[j][j] = even;
      for (k = 0; k < j; k++) {
         mjk = lsq->matrix2[j][k];
         for (i = 0; i < j; i++)
	   lsq->matrix2[i][k] -= lsq->matrix2[i][j] * mjk;

         for (i = j + 1; i < lsq->nfree; i++)
	   lsq->matrix2[i][k] -= lsq->matrix2[i][j] * mjk;
         lsq->matrix2[j][k] = -even * mjk;
      }
      for (k = j + 1; k < lsq->nfree; k++) {
         mjk = lsq->matrix2[j][k];
         for (i = 0; i < j; i++)
	   lsq->matrix2[i][k] -= lsq->matrix2[i][j] * mjk;
         for (i = j + 1; i < lsq->nfree; i++)
	   lsq->matrix2[i][k] -= lsq->matrix2[i][j] * mjk;
         lsq->matrix2[j][k] = -even * mjk;
      }
   }
   for (i = 0; i < lsq->nfree; i++) {	      /* finally, repermute the ... */
      for (k = 0; k < lsq->nfree; k++) {      /* columns. */
         hv[per[k]] = lsq->matrix2[i][k];
      }
      for (k = 0; k < lsq->nfree; k++) {
         lsq->matrix2[i][k] = hv[k];
      }
   }
   return( 0 );					/* all is well */
}

static void getmat( smf_math_function fid ,
                    lsqData *lsq ,
                    const double   xdat[] ,
                    dim_t xdim ,
                    const double   ydat[] ,
                    const float    wdat[] ,
                    dim_t ndat ,
                    const double   fpar[] ,
                    double  *epar ,
                    int      npar  __attribute__((unused)),
                    int      ncomp ,
                    const int    iopt[] ,
                    const double dopt[] )
/*
 * getmat builds the matrix.
 */
{
   double wd;
   double wn;
   double yd;
   dim_t i;
   dim_t j;
   dim_t n;

   for (j = 0; j < lsq->nfree; j++) {
      lsq->vector[j] = 0.0;			/* zero lsq->vector ... */
      for (i = 0; i <= j; i++) {		/* and matrix ... */
         lsq->matrix1[j][i] = 0.0;		/* only on and below diagonal */
      }
   }
   lsq->chi2 = 0.0;				/* reset reduced chi-squared */
   for (n = 0; n < ndat; n++) {		/* loop trough data points */
      wn = wdat[n];
      if (wn > 0.0) {				/* legal weight ? */
	yd = ydat[n] - smf_math_fvalue( fid, xdat[xdim * n], fpar, ncomp,
				      iopt, dopt );
	 smf_math_fpderv( fid, xdat[xdim * n], fpar, ncomp, epar,
			  iopt, dopt );
         lsq->chi2 += yd * yd * wn;		/* add to chi-squared */
         for (j = 0; j < lsq->nfree; j++) {
            wd = epar[lsq->parptr[j]] * wn;	/* weighted derivative */
            lsq->vector[j] += yd * wd;		/* fill lsq->vector */
            for (i = 0; i <= j; i++) {		/* fill matrix */
               lsq->matrix1[j][i] += epar[lsq->parptr[i]] * wd;
            }
         }
      }
   }
}

static int  getvec( smf_math_function fid ,
                    lsqData *lsq ,
                    const double xdat[] ,
                    dim_t xdim ,
                    const double ydat[] ,
                    const float  wdat[] ,
                    dim_t ndat ,
                    const double fpar[] ,
                    double *epar ,
                    int    npar ,
                    int    ncomp ,
                    const int    iopt[] ,
                    const double dopt[] )
/*
 * getvec calculates the correction vector. The matrix has been built by
 * getmat, we only have to rescale it for the current value for labda.
 * The matrix is rescaled so that the diagonal gets the value 1 + labda.
 * Next we calculate the inverse of the matrix and then the correction
 * vector.
 */
{
   double dj;
   double dy;
   double mii;
   double mji;
   double mjj;
   double wn;
   dim_t   i;
   dim_t   j;
   dim_t   n;
   int     r;

   for (j = 0; j < lsq->nfree; j++) {		/* loop to modify and ... */
      mjj = lsq->matrix1[j][j];			/* scale the matrix */
      if (mjj <= 0.0) return( -5 );		/* diagonal element wrong! */
      mjj = sqrt( mjj );
      for (i = 0; i < j; i++) {			/* scale it */
         mji = lsq->matrix1[j][i] / mjj / sqrt( lsq->matrix1[i][i] );
         lsq->matrix2[i][j] = lsq->matrix2[j][i] = mji;
      }
      lsq->matrix2[j][j] = 1.0 + lsq->labda;	/* scaled value on diagonal */
   }
   if ( (r = invmat( lsq )) ) return( r );      /* invert matrix inlace */
   for (i = 0; i < npar; i++) epar[i] = fpar[i];
   for (j = 0; j < lsq->nfree; j++) {		/* loop to calculate ... */
      dj = 0.0;					/* correction lsq->vector */
      mjj = lsq->matrix1[j][j];
      if (mjj <= 0.0) return( -7 );		/* not allowed! */
      mjj = sqrt( mjj );
      for (i = 0; i < lsq->nfree; i++) {
         mii = lsq->matrix1[i][i];
         if (mii <= 0.0) return( -7 );
         mii = sqrt( mii );
         dj += lsq->vector[i] * lsq->matrix2[j][i] / mjj / mii;
      }
      epar[lsq->parptr[j]] += dj;		/* new parameters */
   }
   lsq->chi1 = 0.0;				/* reset reduced chi-squared */
   for (n = 0; n < ndat; n++) {		/* loop through data points */
      wn = wdat[n];				/* get weight */
      if (wn > 0.0) {				/* legal weight */
	dy = ydat[n] - smf_math_fvalue( fid, xdat[xdim * n], epar, ncomp,
				      iopt, dopt );
         lsq->chi1 += wdat[n] * dy * dy;
      }
   }
   return( 0 );
}

int  smf_lsqfit( smf_math_function fid ,
		 const double xdat[] ,
		 int    xdim ,
		 const double ydat[] ,
		 const float  wdat[] ,
		 dim_t  ndat ,
		 double fpar[] ,
		 double *epar ,
		 const int    mpar[] ,
                 int    npar ,
		 int    ncomp ,
		 float  tol  ,
		 int    its  ,
		 float  lab  ,
		 const int    iopt[] ,
		 const double dopt[] )
/*
 * smf_lsqfit is exported, and callable from C as well as Fortran.
 */
{

   lsqData lsqdata;                             /* Common parameter struct */
   lsqData *lsq;                                /* Pointer to block        */
   int    i;
   dim_t  n;
   int    r;

   lsq = &lsqdata;

   lsq->itc = 0;  				/* fate of fit */
   lsq->found = 0;				/* reset */
   lsq->nfree = 0;				/* number of free parameters */
   lsq->nuse = 0;				/* nr of legal data points */

   if (tol < (FLT_EPSILON * 10.0)) {
      lsq->tolerance = FLT_EPSILON * 10.0;	/* default tolerance */
   } else {
      lsq->tolerance = tol;			/* tolerance */
   }
   lsq->labda = fabs( lab ) * LABFAC;	/* start value for mixing parameter */
   for (i = 0; i < npar; i++) {
      if (mpar[i]) {
         if (lsq->nfree > MAXPAR) return( -1 );	/* too many free parameters */
         lsq->parptr[lsq->nfree++] = i;		/* a free parameter */
      }
   }
   if (lsq->nfree == 0) return( -2 );	/* no free parameters */
   for (n = 0; n < ndat; n++) {
      if (wdat[n] > 0.0) lsq->nuse++;	/* legal weight */
   }

   if (lsq->nfree >= lsq->nuse) return( -3 );	/* no degrees of freedom */
   if (lsq->labda == 0.0) {			/* linear fit */
      for (i = 0; i < lsq->nfree; fpar[lsq->parptr[i++]] = 0.0);
      getmat( fid, lsq, xdat, xdim, ydat, wdat, ndat, fpar, epar, npar,
	      ncomp, iopt, dopt );
      r = getvec( fid, lsq, xdat, xdim, ydat, wdat, ndat, fpar, epar, npar,
		  ncomp, iopt, dopt );
      if (r) return( r );		/* error */
      for (i = 0; i < npar; i++) {
         fpar[i] = epar[i];		/* save new parameters */
         epar[i] = 0.0;			/* and set errors to zero */
      }
      lsq->chi1 = sqrt( lsq->chi1 / (double) (lsq->nuse - lsq->nfree) );
      for (i = 0; i < lsq->nfree; i++) {
         if ((lsq->matrix1[i][i] <= 0.0) ||
	     (lsq->matrix2[i][i] <= 0.0)) return( -7 );
         epar[lsq->parptr[i]] = 1.0;
         epar[lsq->parptr[i]] =
	   lsq->chi1 * sqrt( lsq->matrix2[i][i] ) /
	   sqrt( lsq->matrix1[i][i] );
      }
   } else {				/* Non-linear fit */
      /*
       * The non-linear fit uses the steepest descent method in combination
       * with the Taylor method. The mixing of these methods is controlled
       * by labda. In the outer loop (called the iteration loop) we build
       * the matrix and calculate the correction vector. In the inner loop
       * (called the interpolation loop) we check whether we have obtained
       * a better solution than the previous one. If so, we leave the
       * inner loop, else we increase labda (give more weight to the
       * steepest descent method), calculate the correction vector and check
       * again. After the inner loop we do a final check on the goodness of
       * the fit and if this satisfies the tolerance we calculate the
       * errors of the fitted parameters.
       */
      while (!lsq->found) {			       /* iteration loop */
         if (lsq->itc++ == its)
	   return( -4 );    /* increase iteration counter */
	 getmat( fid, lsq, xdat, xdim, ydat, wdat, ndat, fpar, epar, npar,
		 ncomp, iopt, dopt );
         /*
          * here we decrease lsq->labda since we may assume that each iteration
          * brings us closer to the answer.
          */
         if (lsq->labda > LABMIN) lsq->labda /= LABFAC;	/* decrease labda */
         r = getvec( fid, lsq, xdat, xdim, ydat, wdat, ndat, fpar, epar, npar,
		  ncomp, iopt, dopt );
         if (r) return( r );		/* error */
         while (lsq->chi1 >= lsq->chi2) {	    /* interpolation loop */
            /*
             * The next statement is based on experience, not on the
             * mathematics of the problem although I (KGB) think that it
             * is correct to assume that we have reached convergence
             * when the pure steepest descent method does not produce
             * a better solution. Think about this somewhat more, anyway,
             * as already stated, the next statement is based on experience.
             */
            if (lsq->labda > LABMAX) break;	/* assume solution found */
            lsq->labda *= LABFAC;		/* Increase mixing parameter */
            r = getvec( fid, lsq, xdat, xdim, ydat, wdat, ndat, fpar, epar,
                        npar, ncomp, iopt, dopt );
            if (r) return( r );		/* error */
         }
         if (lsq->labda <= LABMAX) {		/* save old parameters */
            for (i = 0; i < npar; i++) fpar[i] = epar[i];
         }
         if (fabs( lsq->chi2 - lsq->chi1 ) <= (lsq->tolerance * lsq->chi1)
	     || (lsq->labda > LABMAX)) {
            /*
             * We have a satisfying solution, so now we need to calculate
             * the correct errors of the fitted parameters. This we do
             * by using the pure Taylor method because we are very close
             * to the real solution.
             */
            lsq->labda = 0.0;		/* for Taylor solution */
            getmat( fid, lsq, xdat, xdim, ydat, wdat, ndat, fpar, epar, npar,
		    ncomp, iopt, dopt );
            r = getvec( fid, lsq, xdat, xdim, ydat, wdat, ndat, fpar, epar,
			npar, ncomp, iopt, dopt );

            if (r) return( r );		/* error */
            for (i = 0; i < npar; i++) {
               epar[i] = 0.0;		/* and set error to zero */
            }
            lsq->chi2 = sqrt( lsq->chi2 /
			      (double) (lsq->nuse - lsq->nfree) );
            for (i = 0; i < lsq->nfree; i++) {
               if ((lsq->matrix1[i][i] <= 0.0) ||
		   (lsq->matrix2[i][i] <= 0.0)) return( -7);
               epar[lsq->parptr[i]] = 1.0;
               epar[lsq->parptr[i]] =
		 lsq->chi2 * sqrt( lsq->matrix2[i][i] ) /
		             sqrt( lsq->matrix1[i][i] );
            }
            lsq->found = 1;			/* we found a solution */
         }
      }
   }
   return( lsq->itc );			/* return number of iterations */
}



#if defined (TESTBED)
/*
 * For testing purposes only. We try to fit a one-dimensional Gaussian
 * to data with a uniform noise distribution. Although the algorithm
 * is developped for Gaussian noise patterns, it should not cause to
 * much problems. For Poison noise the algorithm is not suitable!
 */
static double smf_math_fvalue( smf_math_function fid,
                      double *xdat,
                      double *fpar,
                      int    *ncomp,
                      int    *iopt,
                      double *dopt __attribute__((unused)) )
/*
 * if fid == 1:
 * f(x) = a * exp( -1/2 * (x - b)^2 / s^2 ) + d (one-dimension Gauss)
 * if fid == 2:
 * f(x) = c0 + c1*x +  c2*x^2 + c3*x^3         (polynomial)
 */
{
   double arg;
   double x = *xdat;

   if (fid == 1) {
      double a = fpar[1] - xdat[0];
      double b = fpar[2];

      arg = 0.5 * a / b * a / b;
      return( fpar[0] * exp( -arg ) + fpar[3] );
   } else if (*fid == 2) {

      return( fpar[0] + fpar[1] * x + fpar[2] * x * x + fpar[3] * x * x * x );
   } else if (fid == 3) {

     /*      f = c * (x/a)^(b-1) * exp((-x/a)^b); Weibull curve */


      arg = fpar[1] * x;
      return( pow(x,fpar[0]) * exp( -arg ) );
   }
   return( 0.0 );
}

static void smf_math_fpderv( smf_math_function fid,
		      double *xdat,
		      double *fpar,
		      int    *ncomp,
		      double *epar,
		      int    *iopt __attribute__((unused)),
		      double *dopt __attribute__((unused)) )
{
   double arg;
   double x = *xdat;

   if (fid == 1) {
      double a = fpar[1] - xdat[0];
      double b = fpar[2];

      arg = 0.5 * a / b * a / b;
      epar[0] = exp( -arg );
      epar[1] = -fpar[0] * epar[0] * a / b / b;
      epar[2] = fpar[0] * epar[0] * a * a / b / b / b;
      epar[3] = 1.0;
   } else if (fid == 2) {

      epar[0] = 1.0;
      epar[1] = x;
      epar[2] = x * x;
      epar[3] = x * x * x;
   } else if (fid == 3) {

      arg = fpar[2] * x;
      double f = fpar[0] +pow(x,fpar[1]) * exp( -arg );
      epar[0] = f / fpar[0];           /* Derivative wrt. Amplitude A */
      epar[1] = log(x) * f;                   /* Derv. wrt exponent n */
      epar[2] = -x * f;                               /* Derv. wrt. b */
   }
}


static int smf_math_fnpar( smf_math_function fid ) {
  if (fid < 3 )
    return(4);
  else
    return(3);
}


int main( )
{
   int   i, m, n, r;
   int   xdim = 1;
   int   its = 50;
   int   ndat = 30;
   int   nfit;
   smf_math_function fid = 1;
#if defined (POLY)
   fid = 2;
#endif
#if defined (HIST)
   fid = 3;
#else
#endif
   int   ncomp = 1;
   int   mpar[4];
   float tol = 0.001;
   float lab = 0.01;
#if defined (LINEAR)
   lab = 0.0;
#endif
   double xdat[30];
   double ydat[30];
   float  wdat[30];
   double fpar[4];
   double epar[4];


   int npar = smf_math_fnpar( &fid );

   mpar[0] = 1; mpar[1] = 1; mpar[2] = 1; mpar[3] = 1;

#if defined (SINGLE)
   int fitmax = 11;
#else
   int fitmax = 10;
#endif

   printf( "Fitting function id = %d\n", fid );
   for (nfit = 0; nfit < fitmax; nfit++) {

     if ( fid == 2 ) {
        fpar[0] = 2.0; fpar[1] = 1.5; fpar[2] = 0.01; fpar[3] = 0.003;
     } else if ( fid == 3 ) {
        fpar[0] = 2.0; fpar[1] = 0.3; fpar[2] = 0.4;
     } else {
        fpar[0] = 10.0; fpar[1] = 15; fpar[2] = 3.0; fpar[3] = 0.5;
     }
     printf( "%32s", "Generating parameters: " );
     for ( i = 0; i < npar; i++ ) {
       printf("  %8.4f", fpar[i] );
     }

      printf("\n");
      for (n = 0; n < ndat; n++) {
         double rndm;
         xdat[n] = (float) n;
         wdat[n] = 1.0;
         ydat[n] = smf_math_fvalue( &fid, xdat[n], fpar, &ncomp, NULL, NULL );
         if ( fitmax == 1)
	   printf("%2d: %6.2f", n+1, ydat[n]);
         rndm = ((float) ( rand( ) - RAND_MAX / 2 )) / (double) RAND_MAX * 1.0;
         ydat[n] += rndm;
         if ( fitmax == 1)
	   printf("   =>  %6.2f\n", ydat[n]);
      }
      for (m = 0; m < npar; m++) {
         double rndm;
         rndm = (float) ( rand( ) - RAND_MAX / 2 ) / (double) RAND_MAX * 0.3;
         fpar[m] += rndm;
      }
      printf( "%32s", "Initial Estimates: " );
      for ( i = 0; i < npar; i++ ) {
	printf("  %8.4f", fpar[i] );
      }
      printf("\n");
      printf( "smf_lsqfit:" );
      r = (int) smf_lsqfit( &fid, xdat, &xdim, ydat, wdat, &ndat, fpar, epar,
			    mpar, &npar, &ncomp, &tol, &its, &lab, NULL, NULL );
      printf( " %d", r );
      if (r < 0) {
	printf( ", error!\n" );
      } else {
        printf( ", success!\n" );
	printf( "%32s", "Fitted parameters: " );
	for ( i = 0; i < npar; i++ ) {
	  printf("  %8.4f", fpar[i] );
	}
	printf("\n");
	printf( "%32s", "Errors: " );
	for ( i = 0; i < npar; i++ ) {
	  printf("  %8.4f", epar[i] );
	}
	printf("\n");
      }
   }
   return( 0 );
}
#endif
