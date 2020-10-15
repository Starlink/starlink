

/* smf_gauest.c
*+
*  Name:
*     smf_gauest

*  Purpose:
*     smf_gauest is a routine to search for gaussian components
*     in a profile and to supply initial estimates for a fit.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*    (int) smf_gauest( const double y[],  int n, double *p, int np,
*          double rms, double cutamp, double cutsig, int q )

*  Arguments:
*     y = const double [] (Given)
*        Data points on profile (dimension N).
*     n = int (Given)
*         Number of points in profile (dimension of Y).
*     p = double* (Returned)
*         Contains the estimated gaussian parameters on output. The
*         dimension of P should be at least 3*NP. Each gaussian takes
*         three elements of P to store the parameters. First the
*         amplitude, then the centre and third the dispersion. The
*         dispersion is in units of pixels, to get the dispersion in
*         physical units multiply by the grid separation.  The centre
*         is in pixels [1..n].
*     np = int* (Given)
*         Maximum number of parameters which can be stored in P.
*     rms = double (Given)
*         The  r.m.s. noise level of the profile.
*     cutamp = double (Given)
*         Critical amplitude of gaussian. Gaussians below this amplitude
*         will be discarded.
*     cutsig = double (Given)
*         Critical dispersion of gaussian.
*     q = int (Given)
*         Smoothing parameter used in calculating the second
*         derivative of the profile. Q must be greater than zero.

*  Returned value:
*     smf_gauest = int (Returned)
*         Returns number of gaussians found.

* Description:
*    The routine uses first an automatic window method to define the
*    signal region. Then the second derivative of the profile in the
*    signal region is calculated by fitting a second degree
*    polynomal. The smoothing parameter Q determines the number of
*    points used for this (=2*Q+1).  The gaussians can then be
*    estimated as described by Schwarz, 1968,
*    Bull.Astr.Inst.Netherlands, Volume 19, 405.

*  Notes:
*     If compiled as 'gcc -o est -DTESTBED smf_gauest.c -lm' a
*     standalone executable 'est' will be produced.

*  Authors:
*     Kor Begeman  (Kapteyn Institute, Groningen)
*     Remo Tilanus (JAC, Hawaii)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1990-05-07 (KGB)
*        Initial version for GIPSY
*     2010-10-19 (RPT)
*        Adapted for SMURF
*     2012-04-11 (TIMJ):
*        Consting and removal of pointers

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

/*
 * includes and definitions:
 */

#include        "float.h"                       /* <float.h> */
#include        "math.h"                        /* <math.h> */
#include        "stdio.h"                       /* <stdio.h> */
#include        "stdlib.h"                      /* <stdlib.h> */

#if defined (TESTBED)
#define VAL__BADD -1.7976931348623157e+308
#define NINT(x) ( ( (x) > 0 ) ? (dim_t)( (x) + 0.5 ) : (dim_t)( (x) - 0.5 ) )
#else
/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "msg_par.h"
#include "prm_par.h"
#include "star/kaplibs.h"

/* SMURF includes */
#include "smf.h"
#include "smurf_par.h"
#include "smurf_typ.h"
#endif

/* FIT1D includes */
#include "libsmf/smf_fit1d.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_gfauest"

typedef struct {
   double       a;                              /* amplitude */
   double       c;                              /* centre */
   double       s;                              /* dispersion */
} par_struct;

/*
 * compar returns 1 if the amplitude of component 1 (first argument)
 * is greater than component 2 (second argument). compar is called by qsort.
 */

static int compar( const void *v1, const void *v2 )
{
   const par_struct   *p1, *p2;                       /* the types */

   p1 = (const par_struct *) v1;                      /* assign component 1 */
   p2 = (const par_struct *) v2;                      /* assign component 2 */
   if (p1->a > p2->a) {                         /* one > two */
      return( -1 );
   } else if (p1->a < p2->a) {                  /* one < two */
      return( 1 );
   } else {                                     /* one == two */
      return( 0 );
   }
}


/*
 * window determines the signal region of the profile. It uses a
 * modified automatic window method. window returns the width of the
 * window.
 */

#ifdef  WITHWINDOW
static int window( const double  y[],           /* the profile */
                   int     n,                   /* length of profile */
                   double  cutoff,              /* the critical level */
                   double  rms,                 /* nois level in profile */
                   int     *wstart,             /* begin of signal region */
                   int     *wend )              /* end of signal region */
{
   int          i;                              /* loop counter */
   int          imax = 0;                       /* position of maximum */
   int          nw = 0;                         /* width of window */
   double       flux = 0.0;                     /* total flux in profile */
   double       ymax;                           /* maximum in profile */

   for (ymax = y[0], i = 1; i < n; i++) {       /* loop to get max. and flux */
      double    yi = y[i];

      if (yi > ymax) { ymax = yi; imax = i; }   /* new maximum */
      flux += yi;                               /* add to total flux */
   }
   if (ymax > cutoff) {                         /* above critical level */
      int       iwhi, iwlo;                     /* window borders */
      double    bnew, bold;                     /* new and old base level */
      double     cnew, cold;                     /* new and old centre */
      double    width = -1.0;                   /* window width */

      cnew = imax;                              /* start centre */
      bnew = flux;                              /* start base level */
      do {                                      /* loop */
         double c, s;

         width += 1.0;                          /* increase width of window */
         cold = cnew;                           /* save current centre */
         bold = bnew;                           /* save current base */
         iwlo = NINT( cold - width );           /* start of window */
         iwhi = NINT( cold + width );           /* end of window */
         iwlo = MYMAX( 0, iwlo );               /* start of window */
         iwhi = MYMIN( n - 1, iwhi );           /* end of window */
         s = c = 0.0;                           /* reset */
         for (i = iwlo; i <= iwhi; i++) {       /* loop over new window */
            double      yi = y[i];

            s += yi;                            /* flux in window */
            c += yi * i;                        /* first moment in window */
         }
         bnew = flux - s;                       /* new base */
         nw = n - iwhi + iwlo - 1;              /* window width */
         if (s != 0.0) {                        /* we can devide */
            cnew = c / s;                       /* normalized first moment */
            if (cnew < 0 || cnew > (n - 1)) cnew = cold;
         }
      } while (fabs( bnew - bold ) > rms && nw );
      (*wstart) = iwlo;                         /* start of window */
      (*wend) = iwhi;                           /* end of window */
   }
   return( nw );                                /* return width of window */
}
#endif

/*
 * findc2 calculates the second derivative of the profile inside the
 * window determined by window. This is done by fitting a second degree
 * polynomial to the profile. The smoothing parameter q determines the
 * number of points used for the fit: np = 2*q + 1,
 */

static void findc2( const double  y[],          /* the profile */
                    double  work[],             /* the derivative */
                    dim_t   n,                  /* length of profile */
                    dim_t   iwlo,               /* start of window */
                    dim_t   iwhi,               /* end of window */
                    int     q )                 /* smoothing parameter */
{
   dim_t        i, j;                           /* loop counters */
   static int   oldq = 1;                       /* save this q */
   static double        a = 3.0, b = 0.6666667;         /* save a and b */

   if (oldq != q) {                             /* re-calculate a and b */
      a = 90.0 / (double) ( q * ( q + 1 ) * ( 4 * q * q - 1 ) * ( 2 * q + 3 ) );
      b = (double) ( q * ( q + 1 ) ) / 3.0;
      oldq = q;
   }
   for (i = iwlo; i <= iwhi; i++) {             /* loop over window */
      double     m0 = 0.0;                      /* reset zeroeth moment */
      double     m2 = 0.0;                      /* reset second moment */

      for (j = -q; j <= q; j++) {               /* width is 2 * q + 1 */
         dim_t   k = i + j;

         if (k >= 0 && k < n) {                 /* inside range */
            double      yi = y[k];              /* value from profile */

            m0 += yi;                           /* add to zeroeth moment */
            m2 += ( yi * (double) ( j * j ) );  /* add to second moment */
         }
      }
      work[i] = a * ( m2 - b * m0 );            /* this is the derivative */
   }
}


/*
 * findga estimates the gaussian parameters by looking for local maxima in
 * the second derivative of the profile. It returns the number of
 * gaussians found.
 */

static int findga( const double  y[],           /* the profile */
                   double  work[],              /* the derivative */
                   dim_t   n,                   /* length of profile */
                   dim_t   iwlo,                /* start of window */
                   dim_t   iwhi,                /* end of window */
                   double  cutoff,              /* critical level */
                   double   sigmin,             /* minimum value for sigma */
                   par_struct  *pars )          /* contains the parameters */
{
   dim_t i;                                     /* loop counter */
   dim_t iclo, ichi;                            /* window on gaussian */
   dim_t nmax = 0;                              /* peak counter */
   int  r = 0;                                  /* return value */

   iclo = iwlo;                                 /* reset */
   i = iwlo - 1;                                /* reset */
   while (i++ < iwhi) {                         /* loop over window */
      if (work[i] > 0.0) {                      /* we should check this */
         if (i > iwlo && i < iwhi) {            /* not at edge of  window */
            if (work[i-1] < work[i] && work[i+1] < work[i]) {
               nmax += 1;                       /* peak in 2nd derivative */
            }
         } else if (i == iwlo && work[i+1] < work[i]) {
            nmax += 1;                          /* peak at start of window */
         } else if (i == iwhi && work[i-1] < work[i]) {
            nmax += 1;                          /* peak at end of window */
         }
      }
      switch( nmax ) {                          /* how many peaks ? */
         case 1: {                              /* search for next peak */
            break;
         }
         case 2: {                              /* we have a gaussian */
            dim_t       ic;                     /* loop counter */
            double      a, b;
            double      det;                    /* determinant */
            double      m0m, m0, m1, m2;        /* some moments */

            ichi = i;                           /* end of gauss window */
            b = work[iclo];                     /* now  we use the Schwarz */
            a = (work[ichi] - b) / (double) (ichi - iclo);
            m0m = m0 = m1 = m2 = 0.0;
            for (ic = iclo; ic <= ichi; ic++) { /* loop over gaussian */
               double   wi;

               m0m += MYMIN( work[ic], 0.0 );
               wi = work[ic] - a * (double) (ic - iclo) - b;
               m0 += wi;
               m1 += wi * (double) ic;
               m2 += wi * (double) (ic*ic);
            }
            det = m2 * m0 - m1 * m1;            /* determinant */
            if (det > 0.0 && fabs( m0m ) >  FLT_EPSILON) {
               int      im, is;
               double   pg, sg;
               double   xm = m1 / m0;
               double   yh, yl, ym;

               sg = 1.69 * sqrt( det ) / fabs( m0 );
               if (sg > sigmin) {               /* above critical width */
                  is = NINT( 1.73 * sg );
                  im = NINT( xm );
                  if ((im - is) < 0) yl = 0.0; else yl = y[im-is];
                  ym = y[im];
                  if ((im + is) > (n-1)) yh = 0.0; else yh = y[im+is];
                  pg = (ym-0.5*(yh+yl))/(1.0-exp(-0.5*((double)(is*is))/sg/sg));
                  pg = MYMIN( pg, ym );
                  if (pg > cutoff) {            /* above critical level */
                     if (r < MAXPAR) {          /* add to list */
                        pars[r].a = pg;         /* amplitude */
                        pars[r].s = sg;         /* width */
                        pars[r].c = xm;         /* centre */
                     }
                     r += 1;                    /* increase # gaussians */
                  }
               }
            }
            iclo = ichi;                        /* next gauss starts here */
            nmax -= 1;                          /* one peak less */
            break;
         }
         default: {                             /* no gauss */
            iclo = i + 1;                       /* next gauss could start here */
            break;
         }
      }
   }
   return( r );
}


/*
 * the works
 */

int smf_gauest( const double    y[],           /* profile data */
                dim_t n,                       /* number of points */
                double   *p,                   /* parameter estimates */
                int       np,                  /* number of parameters */
                double    rms __attribute__((unused)), /* rms noise level */
                double    cutoff,              /* cutoff level */
                double    minsig,              /* minimum width of gaussians */
                int       q )                  /* smoothing parameter */
{
   dim_t        iwhi, iwlo;                    /* window limits */
   dim_t        l, m;                          /* loop counters */
   int          r = 0;                         /* return value */
   par_struct   pars[MAXPAR];                  /* contains the parameters */
   double       *work;                         /* Work array for 2nd derv */

#ifdef  WITHWINDOW
   if (!window( y, n, cutoff, rms, &iwlo, &iwhi )) {
      return( r );                              /* no signal in profile */
   }
   iwhi = MYMIN( iwhi + q,  n - 1 );      /* extend window with ... */
   iwlo = MYMAX( iwlo - q,  0 );             /* smoothing parameter */
#endif

#if defined (TESTBED)
   work = calloc( n, sizeof(double) );
#else
   work = astCalloc( n, sizeof(double) );
#endif

   iwhi = n - 1;
   iwlo = 0;
   findc2( y, work, n, iwlo, iwhi, q );       /* get second derivative */
   r = findga( y, work, n, iwlo, iwhi, cutoff, minsig, pars );
   if (r > MAXPAR) r = MAXPAR;                  /* this  should neven happen */
   if (r > 1) qsort( pars, r, sizeof( par_struct ), compar );
   for (l = 0, m = 0; m < np; l++) {
      if (l < r) {                              /* still something in list */
         p[m++] = pars[l].a;                    /* the amplitude */
         p[m++] = pars[l].c+1;                  /* the centre 1..n */
         p[m++] = pars[l].s;                    /* the width */
      } else {                                  /* list is exhausted */
         p[m++] = VAL__BADD;                        /* set to blank */
         p[m++] = VAL__BADD;                        /* set to blank */
         p[m++] = VAL__BADD;                        /* set to blank */
      }
   }

#if defined (TESTBED)
   free(work);                                  /* Free memory */
#else
   astFree(work);
#endif

   return( r );                                 /* return number of gaussians */
}

#if     defined(TESTBED)

#define NPAR    9
#define NPTS    100

int    main( )
{
   int          q = 1;
   int          r;
   int          n = NPTS;
   int          np = NPAR;
   double       rms = 0.5;
   double       cutoff= 1.0;
   double       minsig = 0.5;
   double       par[NPAR];
   double       y[NPTS];

   par[0] = 5.0; par[1] = 45.0; par[2] = 1.1;
   par[3] = 4.0; par[4] = 50.0; par[5] = 1.1;
   par[6] = 6.0; par[7] = 55.0; par[8] = 1.1;
   {
      int       i, j;

      for (i = 0; i < n; i++) {
         y[i] = 0.0;
         for (j = 0; j < np; j += 3) {
            if (par[j] != 0.0) {
               double    a = par[j];
               double    c = (par[j+1] - (double) i);
               double    s = par[j+2];

               y[i] += a * exp( -0.5 * c / s * c / s );
            }
         }
      }
   }
   r = smf_gauest( y, n, par, np, rms, cutoff, minsig, q );
   printf( "gauest = %d\n", r );
   if (r > 0) {
      int       i;

      for (i = 0; i < (3*r); i += 3) {
         printf( "A = %f, C = %f, S = %f\n", par[i], par[i+1], par[i+2] );
      }
   }
}
#endif
