/////////////////////////////////////////////////////////////////////////////////
//
//  Levenberg - Marquardt non-linear minimization algorithm
//  Copyright (C) 2004  Manolis Lourakis (lourakis at ics forth gr)
//  Institute of Computer Science, Foundation for Research & Technology - Hellas
//  Heraklion, Crete, Greece.
//
//  This program is free software; you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation; either version 2 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
/////////////////////////////////////////////////////////////////////////////////

/* define astCLASS so that the AST error reporting module does not
   consider this file to be an external file, and so report line numbers
   within levmar.c when issuing error reports. */
#define astCLASS levmar

#include <float.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "memory.h"
#include "error.h"
#include "ast_err.h"
#include "levmar.h"

static int dlevmar_LUinverse_noLapack(double *A,double *B, int m);
static double dlevmar_L2nrmxmy(double *e, double *x, double *y, int n);
static void dlevmar_trans_mat_mat_mult(double *a, double *b, int n, int m);
static int dAx_eq_b_LU_noLapack(double *A, double *B, double *x, int n);
static int dlevmar_covar(double *JtJ, double *C, double sumsq, int m, int n);
static int dAx_eq_b_LU_noLapack(double *A, double *B, double *x, int n);


/*
 * This function seeks the parameter vector p that best describes the measurements vector x.
 * More precisely, given a vector function  func : R^m --> R^n with n>=m,
 * it finds p s.t. func(p) ~= x, i.e. the squared second order (i.e. L2) norm of
 * e=x-func(p) is minimized.
 *
 * This function requires an analytic Jacobian. In case the latter is unavailable,
 * use LEVMAR_DIF() bellow
 *
 * Returns the number of iterations (>=0) if successful, LM_ERROR if failed
 *
 * For more details, see K. Madsen, H.B. Nielsen and O. Tingleff's lecture notes on
 * non-linear least squares at http://www.imm.dtu.dk/pubdb/views/edoc_download.php/3215/pdf/imm3215.pdf
 */
int dlevmar_der (
   void (*func) (double *p, double *hx, int m, int n, void *adata), /* functional relation describing measurements. A p \in R^m yields a \hat{x} \in R^n */
   void (*jacf) (double *p, double *j, int m, int n, void *adata),  /* function to evaluate the Jacobian \part x / \part p */
   double *p,       /* I/O: initial parameter estimates. On output has the estimated solution */
   double *x,       /* I: measurement vector. NULL implies a zero vector */
   int m,           /* I: parameter vector dimension (i.e. #unknowns) */
   int n,           /* I: measurement vector dimension */
   int itmax,       /* I: maximum number of iterations */
   double opts[4],  /* I: minim. options [\mu, \epsilon1, \epsilon2, \epsilon3]. Respectively the scale factor for initial \mu,
                     * stopping thresholds for ||J^T e||_inf, ||Dp||_2 and ||e||_2. Set to NULL for defaults to be used
                     */
   double info[10], /* O: information regarding the minimization. Set to NULL if don't care
                     * info[0]= ||e||_2 at initial p.
                     * info[1-4]=[ ||e||_2, ||J^T e||_inf,  ||Dp||_2, mu/max[J^T J]_ii ], all computed at estimated p.
                     * info[5]= # iterations,
                     * info[6]=reason for terminating: 1 - stopped by small gradient J^T e
                     *                                 2 - stopped by small Dp
                     *                                 3 - stopped by itmax
                     *                                 4 - singular matrix. Restart from current p with increased mu
                     *                                 5 - no further error reduction is possible. Restart with increased mu
                     *                                 6 - stopped by small ||e||_2
                     *                                 7 - stopped by invalid (i.e. NaN or Inf) "func" values. This is a user error
                     * info[7]= # function evaluations
                     * info[8]= # Jacobian evaluations
                     * info[9]= # linear systems solved, i.e. # attempts for reducing error
                     */
   double *work,    /* working memory at least LM_DER_WORKSZ() reals large, allocated if NULL */
   double *covar,   /* O: Covariance matrix corresponding to LS solution; mxm. Set to NULL if not needed. */
   void *adata      /* pointer to possibly additional data, passed uninterpreted to func & jacf.
                     * Set to NULL if not needed */
) {
   register int i, j, k, l;
   int worksz, freework = 0, issolved;

/* temp work arrays */
   double *e,            /* nx1 */
          *hx,           /* \hat{x}_i, nx1 */
          *jacTe,        /* J^T e_i mx1 */
          *jac,          /* nxm */
          *jacTjac,      /* mxm */
          *Dp,           /* mx1 */
          *diag_jacTjac, /* diagonal of J^T J, mx1 */
          *pDp;          /* p + Dp, mx1 */
   register double mu,   /* damping constant */
                   tmp;  /* mainly used in matrix & vector multiplications */
   double p_eL2, jacTe_inf, pDp_eL2;  /* ||e(p)||_2, ||J^T e||_inf, ||e(p+Dp)||_2 */
   double p_L2, Dp_L2 = DBL_MAX, dF, dL;
   double tau, eps1, eps2, eps2_sq, eps3;
   double init_p_eL2;
   int nu = 2, nu2, stop = 0, nfev, njev = 0, nlss = 0;
   int *status = astGetStatusPtr;
   const int nm = n * m;
   int (*linsolver) (double *A, double *B, double *x, int m) = NULL;
   mu = jacTe_inf = 0.0; /* -Wall */

   if (n < m) {
      astError( AST__LEVMAR, "dlevmar_der: cannot solve a problem with "
                "fewer measurements [%d] than unknowns [%d]", status, n, m );
      return -1;
   }

   if (!jacf) {
      astError( AST__LEVMAR, "dlevmar_der: No function specified for "
                "computing the Jacobian in dlevmar_der().\nIf no such "
                "function is available, use dlevmar_dif() rather than "
                "dlevmar_der()", status );
      return -1;
   }

   if (opts) {
      tau = opts[0];
      eps1 = opts[1];
      eps2 = opts[2];
      eps2_sq = opts[2] * opts[2];
      eps3 = opts[3];
   } else {   // use default values
      tau = (1E-03);
      eps1 = (1E-17);
      eps2 = (1E-17);
      eps2_sq = (1E-17) * (1E-17);
      eps3 = (1E-17);
   }

   if (!work) {
      worksz = (2 * (n) + 4 * (m) + (n) * (m) + (m) * (m)); //2*n+4*m + n*m + m*m;
      work = (double *) astMalloc(worksz * sizeof(double));

/* allocate a big chunk in one step */
      if (!work) {
         astError( AST__LEVMAR, "dlevmar_der: memory allocation request failed.", status );
         return -1;
      }
      freework = 1;
   }

/* set up work arrays */
   e = work;
   hx = e + n;
   jacTe = hx + n;
   jac = jacTe + m;
   jacTjac = jac + nm;
   Dp = jacTjac + m * m;
   diag_jacTjac = Dp + m;
   pDp = diag_jacTjac + m;

/* compute e=x - f(p) and its L2 norm */
   (*func) (p, hx, m, n, adata);
   nfev = 1;

/* ### e=x-hx, p_eL2=||e|| */
   p_eL2 = dlevmar_L2nrmxmy(e, x, hx, n);
   init_p_eL2 = p_eL2;
   if (!finite(p_eL2)) stop = 7;

   for (k = 0; k < itmax && !stop; ++k) {

/* Note that p and e have been updated at a previous iteration */
      if (p_eL2 <= eps3) {

/* error is small */
         stop = 6;
         break;
      }

/* Compute the Jacobian J at p,  J^T J,  J^T e,  ||J^T e||_inf and ||p||^2.
 * Since J^T J is symmetric, its computation can be sped up by computing
 * only its upper triangular part and copying it to the lower part
 */
      (*jacf) (p, jac, m, n, adata);
      ++njev;

/* J^T J, J^T e */

/* "32" is the block size for cache-friendly matrix-matrix multiply. It should be
   such that __BLOCKSZ__^2*sizeof(LM_REAL) is smaller than the CPU (L1)
   data cache size. Notice that a value of 32 when LM_REAL=double assumes
   an 8Kb L1 data cache (32*32*8=8K). This is a concervative choice since
   newer Pentium 4s have a L1 data cache of size 16K, capable of holding
   up to 45x45 double blocks. */
      if (nm < (32*32) ) { // this is a small problem

/* J^T*J_ij = \sum_l J^T_il * J_lj = \sum_l J_li * J_lj.
 * Thus, the product J^T J can be computed using an outer loop for
 * l that adds J_li*J_lj to each element ij of the result. Note that
 * with this scheme, the accesses to J and JtJ are always along rows,
 * therefore induces less cache misses compared to the straightforward
 * algorithm for computing the product (i.e., l loop is innermost one).
 * A similar scheme applies to the computation of J^T e.
 * However, for large minimization problems (i.e., involving a large number
 * of unknowns and measurements) for which J/J^T J rows are too large to
 * fit in the L1 cache, even this scheme incures many cache misses. In
 * such cases, a cache-efficient blocking scheme is preferable.
 *
 * Thanks to John Nitao of Lawrence Livermore Lab for pointing out this
 * performance problem.
 *
 * Note that the non-blocking algorithm is faster on small
 * problems since in this case it avoids the overheads of blocking.
 */

/* looping downwards saves a few computations */
         register int l, im;
         register double alpha, *jaclm;
         for (i = m * m; i-- > 0;) jacTjac[i] = 0.0;
         for (i = m; i-- > 0;) jacTe[i] = 0.0;

         for (l = n; l-- > 0;) {
            jaclm = jac + l * m;

            for (i = m; i-- > 0;) {
               im = i * m;
               alpha = jaclm[i]; //jac[l*m+i];

/* j<=i computes lower triangular part only */
               for (j = i + 1; j-- > 0;)
               jacTjac[im + j] += jaclm[j] * alpha; //jac[l*m+j]

/* J^T e */
               jacTe[i] += alpha * e[l];
            }
         }
         for (i = m; i-- > 0;)

/* copy to upper part */
            for (j = i + 1; j < m; ++j)
               jacTjac[i * m + j] = jacTjac[j * m + i];

      } else {  // this is a large problem

/* Cache efficient computation of J^T J based on blocking */
         dlevmar_trans_mat_mat_mult(jac, jacTjac, n, m);

/* cache efficient computation of J^T e */
         for (i = 0; i < m; ++i) jacTe[i] = 0.0;

         for (i = 0; i < n; ++i) {
            register double *jacrow;
            for (l = 0, jacrow = jac + i * m, tmp = e[i]; l < m; ++l) jacTe[l] += jacrow[l] * tmp;
         }
      }

/* Compute ||J^T e||_inf and ||p||^2 */
      for (i = 0, p_L2 = jacTe_inf = 0.0; i < m; ++i) {
         if (jacTe_inf < (tmp = (((jacTe[i]) >= 0.0) ? (jacTe[i]) : -(jacTe[i])))) jacTe_inf = tmp;
         diag_jacTjac[i] = jacTjac[i * m + i];

/* save diagonal entries so that augmentation can be later canceled */
         p_L2 += p[i] * p[i];
      }  //p_L2=sqrt(p_L2);

/* check for convergence */
      if ((jacTe_inf <= eps1)) {
         Dp_L2 = 0.0;

/* no increment for p in this case */
         stop = 1;
         break;
      }

/* compute initial damping factor */
      if (k == 0) {
         for (i = 0, tmp = -DBL_MAX; i < m; ++i)
            if (diag_jacTjac[i] > tmp) tmp = diag_jacTjac[i];

/* find max diagonal element */
         mu = tau * tmp;
      }

/* determine increment using adaptive damping */
      while (1) {

/* augment normal equations */
         for (i = 0; i < m; ++i) jacTjac[i * m + i] += mu;

/* solve augmented equations */
/* use the LU included with levmar */
         issolved = dAx_eq_b_LU_noLapack(jacTjac,jacTe, Dp, m);
         ++nlss;
         linsolver = dAx_eq_b_LU_noLapack;
         if (issolved) {

/* compute p's new estimate and ||Dp||^2 */
            for (i = 0, Dp_L2 = 0.0; i < m; ++i) {
               pDp[i] = p[i] + (tmp = Dp[i]);
               Dp_L2 += tmp * tmp;
            }

            if (Dp_L2 <= eps2_sq * p_L2) {

/* relative change in p is small, stop */
               stop = 2;
               break;
            }

            if (Dp_L2 >= (p_L2 + eps2) / ((1E-12) * (1E-12))) {

/* almost singular */
               stop = 4;
               break;
            }
            (*func) (pDp, hx, m, n, adata);
            ++nfev;

/* evaluate function at p + Dp */

/* compute ||e(pDp)||_2 */

/* ### hx=x-hx, pDp_eL2=||hx|| */
            pDp_eL2 = dlevmar_L2nrmxmy(hx, x, hx, n);
            if (!finite(pDp_eL2)) {

/* sum of squares is not finite, most probably due to a user error.
 * This check makes sure that the inner loop does not run indefinitely.
 * Thanks to Steve Danauskas for reporting such cases */
               stop = 7;
               break;
            }

            for (i = 0, dL = 0.0; i < m; ++i) dL += Dp[i] * (mu * Dp[i] + jacTe[i]);
            dF = p_eL2 - pDp_eL2;

            if (dL > 0.0 && dF > 0.0) {

/* reduction in error, increment is accepted */
               tmp = ((2.0) * dF / dL - (1.0));
               tmp = (1.0) - tmp * tmp * tmp;
               mu = mu * ((tmp >= 0.3333333334)? tmp : 0.3333333334);
               nu = 2;

/* update p's estimate */
               for (i = 0; i < m; ++i) p[i] = pDp[i];

/* update e and ||e||_2 */
               for (i = 0; i < n; ++i) e[i] = hx[i];
               p_eL2 = pDp_eL2;
               break;
            }
         }

/* if this point is reached, either the linear system could not be solved or
 * the error did not reduce; in any case, the increment must be rejected
 */
         mu *= nu;
         nu2 = nu << 1; // 2*nu;
         if (nu2 <= nu) {

/* nu has wrapped around (overflown). Thanks to Frank Jordan for
   spotting this case */
            stop = 5;
            break;
         }
         nu = nu2;
         for (i = 0; i < m; ++i)

/* restore diagonal J^T J entries */
         jacTjac[i * m + i] = diag_jacTjac[i];
      }

/* inner loop */
   }

   if (k >= itmax) stop = 3;

/* restore diagonal J^T J entries */
   for (i = 0; i < m; ++i) jacTjac[i * m + i] = diag_jacTjac[i];

   if (info) {
      info[0] = init_p_eL2;
      info[1] = p_eL2;
      info[2] = jacTe_inf;
      info[3] = Dp_L2;
      for (i = 0, tmp = -DBL_MAX; i < m; ++i)
         if (tmp < jacTjac[i * m + i]) tmp = jacTjac[i * m + i];
      info[4] = mu / tmp;
      info[5] = (double) k;
      info[6] = (double) stop;
      info[7] = (double) nfev;
      info[8] = (double) njev;
      info[9] = (double) nlss;
   }

/* covariance matrix */
   if (covar) {
      dlevmar_covar(jacTjac, covar, p_eL2, m, n);
   }
   if (freework) work = astFree(work);
   if (linsolver) (*linsolver) (NULL, NULL, NULL, 0);

   return (stop != 4 && stop != 7) ? k : -1;
}







/* Compute e=x-y for two n-vectors x and y and return the squared L2 norm of e.
 * e can coincide with either x or y; x can be NULL, in which case it is assumed
 * to be equal to the zero vector.
 * Uses loop unrolling and blocking to reduce bookkeeping overhead & pipeline
 * stalls and increase instruction-level parallelism; see http://www.abarnett.demon.co.uk/tutorial.html
 */
static double dlevmar_L2nrmxmy( double *e, double *x, double *y, int n) {
   const int blocksize = 8, bpwr = 3;

/* 8=2^3 */
   register int i;
   int j1, j2, j3, j4, j5, j6, j7;
   int blockn;
   register double sum0 = 0.0, sum1 = 0.0, sum2 = 0.0, sum3 = 0.0;

/* n may not be divisible by blocksize,
 * go as near as we can first, then tidy up.
 */
   blockn = (n >> bpwr) << bpwr;

/* (n / blocksize) * blocksize; */

/* unroll the loop in blocks of `blocksize'; looping downwards gains
   some more speed */
   if (x) {
      for (i = blockn - 1; i > 0; i -= blocksize) {
         e[i] = x[i] - y[i];
         sum0 += e[i] * e[i];
         j1 = i - 1;
         e[j1] = x[j1] - y[j1];
         sum1 += e[j1] * e[j1];
         j2 = i - 2;
         e[j2] = x[j2] - y[j2];
         sum2 += e[j2] * e[j2];
         j3 = i - 3;
         e[j3] = x[j3] - y[j3];
         sum3 += e[j3] * e[j3];
         j4 = i - 4;
         e[j4] = x[j4] - y[j4];
         sum0 += e[j4] * e[j4];
         j5 = i - 5;
         e[j5] = x[j5] - y[j5];
         sum1 += e[j5] * e[j5];
         j6 = i - 6;
         e[j6] = x[j6] - y[j6];
         sum2 += e[j6] * e[j6];
         j7 = i - 7;
         e[j7] = x[j7] - y[j7];
         sum3 += e[j7] * e[j7];
      }

/*
 * There may be some left to do.
 * This could be done as a simple for() loop,
 * but a switch is faster (and more interesting)
 */
      i = blockn;
      if (i < n) {

/* Jump into the case at the place that will allow
 * us to finish off the appropriate number of items.
 */
         switch (n - i) {
            case 7:
            e[i] = x[i] - y[i];
            sum0 += e[i] * e[i];
            ++i;

            case 6:
            e[i] = x[i] - y[i];
            sum1 += e[i] * e[i];
            ++i;

            case 5:
            e[i] = x[i] - y[i];
            sum2 += e[i] * e[i];
            ++i;

            case 4:
            e[i] = x[i] - y[i];
            sum3 += e[i] * e[i];
            ++i;

            case 3:
            e[i] = x[i] - y[i];
            sum0 += e[i] * e[i];
            ++i;

            case 2:
            e[i] = x[i] - y[i];
            sum1 += e[i] * e[i];
            ++i;

            case 1:
            e[i] = x[i] - y[i];
            sum2 += e[i] * e[i]; //++i;
         }
      }

   } else {

/* x==0 */
      for (i = blockn - 1; i > 0; i -= blocksize) {
         e[i] = -y[i];
         sum0 += e[i] * e[i];
         j1 = i - 1;
         e[j1] = -y[j1];
         sum1 += e[j1] * e[j1];
         j2 = i - 2;
         e[j2] = -y[j2];
         sum2 += e[j2] * e[j2];
         j3 = i - 3;
         e[j3] = -y[j3];
         sum3 += e[j3] * e[j3];
         j4 = i - 4;
         e[j4] = -y[j4];
         sum0 += e[j4] * e[j4];
         j5 = i - 5;
         e[j5] = -y[j5];
         sum1 += e[j5] * e[j5];
         j6 = i - 6;
         e[j6] = -y[j6];
         sum2 += e[j6] * e[j6];
         j7 = i - 7;
         e[j7] = -y[j7];
         sum3 += e[j7] * e[j7];
      }

/*
 * There may be some left to do.
 * This could be done as a simple for() loop,
 * but a switch is faster (and more interesting)
 */
      i = blockn;
      if (i < n) {

/* Jump into the case at the place that will allow
 * us to finish off the appropriate number of items.
 */
         switch (n - i) {

            case 7:
            e[i] = -y[i];
            sum0 += e[i] * e[i];
            ++i;

            case 6:
            e[i] = -y[i];
            sum1 += e[i] * e[i];
            ++i;

            case 5:
            e[i] = -y[i];
            sum2 += e[i] * e[i];
            ++i;

            case 4:
            e[i] = -y[i];
            sum3 += e[i] * e[i];
            ++i;

            case 3:
            e[i] = -y[i];
            sum0 += e[i] * e[i];
            ++i;

            case 2:
            e[i] = -y[i];
            sum1 += e[i] * e[i];
            ++i;

            case 1:
            e[i] = -y[i];
            sum2 += e[i] * e[i]; //++i;
         }
      }
   }
   return sum0 + sum1 + sum2 + sum3;
}





/* blocked multiplication of the transpose of the nxm matrix a with itself (i.e. a^T a)
 * using a block size of bsize. The product is returned in b.
 * Since a^T a is symmetric, its computation can be sped up by computing only its
 * upper triangular part and copying it to the lower part.
 *
 * More details on blocking can be found at
 * http://www-2.cs.cmu.edu/afs/cs/academic/class/15213-f02/www/R07/section_a/Recitation07-SectionA.pdf
 */
static void dlevmar_trans_mat_mat_mult(double *a,double *b, int n, int m) {
   register int i, j, k, jj, kk;
   register double sum, *bim, *akm;

/* block size for cache-friendly matrix-matrix multiply. It should be
 * such that __BLOCKSZ__^2*sizeof(LM_REAL) is smaller than the CPU (L1)
 * data cache size. Notice that a value of 32 when LM_REAL=double assumes
 * an 8Kb L1 data cache (32*32*8=8K). This is a concervative choice since
 * newer Pentium 4s have a L1 data cache of size 16K, capable of holding
 * up to 45x45 double blocks.
 */
   const int bsize = 32;

/* compute upper triangular part using blocking */
   for (jj = 0; jj < m; jj += bsize) {
      for (i = 0; i < m; ++i) {
         bim = b + i * m;
         for (j = (((jj) >= (i)) ? (jj) : (i));
              j < (((jj + bsize) <= (m)) ? (jj + bsize) : (m)); ++j)
            bim[j] = 0.0; //b[i*m+j]=0.0;
      }

      for (kk = 0; kk < n; kk += bsize) {
         for (i = 0; i < m; ++i) {
            bim = b + i * m;
            for (j = (((jj) >= (i)) ? (jj) : (i));
                 j < (((jj + bsize) <= (m)) ? (jj + bsize) : (m));
                 ++j) {
               sum = 0.0;
               for (k = kk;
                    k < (((kk + bsize) <= (n)) ? (kk + bsize) : (n));
                    ++k) {
                  akm = a + k * m;
                  sum += akm[i] * akm[j]; //a[k*m+i]*a[k*m+j];
               }
               bim[j] += sum; //b[i*m+j]+=sum;
            }
         }
      }
   }

/* copy upper triangular part to the lower one */
   for (i = 0; i < m; ++i)
      for (j = 0; j < i; ++j) b[i * m + j] = b[j * m + i];
}





/*
 * This function computes in C the covariance matrix corresponding to a least
 * squares fit. JtJ is the approximate Hessian at the solution (i.e. J^T*J, where
 * J is the Jacobian at the solution), sumsq is the sum of squared residuals
 * (i.e. goodnes of fit) at the solution, m is the number of parameters (variables)
 * and n the number of observations. JtJ can coincide with C.
 *
 * if JtJ is of full rank, C is computed as sumsq/(n-m)*(JtJ)^-1
 * otherwise and if LAPACK is available, C=sumsq/(n-r)*(JtJ)^+
 * where r is JtJ's rank and ^+ denotes the pseudoinverse
 * The diagonal of C is made up from the estimates of the variances
 * of the estimated regression coefficients.
 * See the documentation of routine E04YCF from the NAG fortran lib
 *
 * The function returns the rank of JtJ if successful, 0 on error
 *
 * A and C are mxm
 *
 */
static int dlevmar_covar(double *JtJ, double *C,double sumsq, int m, int n) {
   register int i;
   int rnk;
   double fact;

   rnk = dlevmar_LUinverse_noLapack(JtJ, C, m);
   if (!rnk) return 0;
   rnk = m;

/* assume full rank */
    fact = sumsq / (double) (n - rnk);
    for (i = 0; i < m * m; ++i) C[i] *= fact;
    return rnk;
}






/*
 * This function computes the inverse of A in B. A and B can coincide
 *
 * The function employs LAPACK-free LU decomposition of A to solve m linear
 * systems A*B_i=I_i, where B_i and I_i are the i-th columns of B and I.
 *
 * A and B are mxm
 *
 * The function returns 0 in case of error, 1 if successful
 *
 */
static int dlevmar_LUinverse_noLapack(double *A,double *B, int m) {
   void *buf = NULL;
   int buf_sz = 0;
   register int i, j, k, l;
   int *idx, maxi = -1, idx_sz, a_sz, x_sz, work_sz, tot_sz;
   double *a, *x, *work, max, sum, tmp;
   int *status = astGetStatusPtr;

/* calculate required memory size */
   idx_sz = m;
   a_sz = m * m;
   x_sz = m;
   work_sz = m;
   tot_sz = (a_sz + x_sz + work_sz) * sizeof(double) + idx_sz * sizeof(int);

/* should be arranged in that order for proper doubles alignment */
   buf_sz = tot_sz;
   buf = (void *) astMalloc(tot_sz);
   if (!buf) {
      astError( AST__LEVMAR, "memory allocation in dlevmar_LUinverse_noLapack() failed!", status );
      return 0;
   }
   a = buf;
   x = a + a_sz;
   work = x + x_sz;
   idx = (int *) (work + work_sz);

/* avoid destroying A by copying it to a */
   for (i = 0; i < a_sz; ++i) a[i] = A[i];

/* compute the LU decomposition of a row permutation of matrix a; the
   permutation itself is saved in idx[] */
   for (i = 0; i < m; ++i) {
      max = 0.0;
      for (j = 0; j < m; ++j)
         if ((tmp = (((a[i * m + j]) >= 0.0) ? (a[i * m + j]) : -(a[i * m + j]))) > max) max = tmp;

      if (max == 0.0) {
         astError( AST__LEVMAR, "Singular matrix A in dlevmar_LUinverse_noLapack()!", status );
         buf = astFree(buf);
        return 0;
      }
      work[i] = (1.0) / max;
   }

   for (j = 0; j < m; ++j) {
      for (i = 0; i < j; ++i) {
         sum = a[i * m + j];
         for (k = 0; k < i; ++k) sum -= a[i * m + k] * a[k * m + j];
         a[i * m + j] = sum;
      }

      max = 0.0;
      for (i = j; i < m; ++i) {
         sum = a[i * m + j];
         for (k = 0; k < j; ++k) sum -= a[i * m + k] * a[k * m + j];
         a[i * m + j] = sum;
         if ((tmp = work[i] * (((sum) >= 0.0) ? (sum) : -(sum))) >= max) {
            max = tmp;
            maxi = i;
         }
      }

      if (j != maxi) {
         for (k = 0; k < m; ++k) {
            tmp = a[maxi * m + k];
            a[maxi * m + k] = a[j * m + k];
            a[j * m + k] = tmp;
         }
         work[maxi] = work[j];
      }

      idx[j] = maxi;
      if (a[j * m + j] == 0.0) a[j * m + j] = DBL_EPSILON;
      if (j != m - 1) {
         tmp = (1.0) / (a[j * m + j]);
         for (i = j + 1; i < m; ++i) a[i * m + j] *= tmp;
      }
   }

/* The decomposition has now replaced a. Solve the m linear systems using
 * forward and back substitution
 */
   for (l = 0; l < m; ++l) {
      for (i = 0; i < m; ++i) x[i] = 0.0;
      x[l] = (1.0);
      for (i = k = 0; i < m; ++i) {
         j = idx[i];
         sum = x[j];
         x[j] = x[i];
         if (k != 0)  for (j = k - 1; j < i; ++j) sum -= a[i * m + j] * x[j];
         else if (sum != 0.0) k = i + 1;
         x[i] = sum;
      }

      for (i = m - 1; i >= 0; --i) {
         sum = x[i];
         for (j = i + 1; j < m; ++j) sum -= a[i * m + j] * x[j];
         x[i] = sum / a[i * m + i];
      }

      for (i = 0; i < m; ++i) B[i * m + l] = x[i];
   }
   buf = astFree(buf);
   return 1;
}






/*
 * This function returns the solution of Ax = b
 *
 * The function employs LU decomposition followed by forward/back substitution (see
 * also the LAPACK-based LU solver above)
 *
 * A is mxm, b is mx1
 *
 * The function returns 0 in case of error, 1 if successful
 *
 * This function is often called repetitively to solve problems of identical
 * dimensions. To avoid repetitive malloc's and free's, allocated memory is
 * retained between calls and free'd-malloc'ed when not of the appropriate size.
 * A call with NULL as the first argument forces this memory to be released.
 */
static int dAx_eq_b_LU_noLapack(double *A, double *B,double *x, int m) {
   static void *buf = NULL;
   static int buf_sz = 0;
   register int i, j, k;
   int *idx, maxi = -1, idx_sz, a_sz, work_sz, tot_sz;
   double *a, *work, max, sum, tmp;
   int *status = astGetStatusPtr;

   if (!A) {
      buf = astFree(buf);
      buf_sz = 0;
      return 1;
   }

/* calculate required memory size */
   idx_sz = m;
   a_sz = m * m;
   work_sz = m;
   tot_sz = (a_sz + work_sz) * sizeof(double) + idx_sz * sizeof(int);

/* should be arranged in that order for proper doubles alignment */
   if (tot_sz > buf_sz) {

/* insufficient memory, allocate a "big" memory chunk at once */
      buf = astFree(buf);

/* free previously allocated memory */
      buf_sz = tot_sz;
      buf = (void *) astMalloc(tot_sz);
      if (!buf) {
         astError( AST__LEVMAR, "memory allocation in dAx_eq_b_LU_noLapack() failed!", status );
         return 0;
      }
   }

   a = buf;
   work = a + a_sz;
   idx = (int *) (work + work_sz);

/* avoid destroying A, B by copying them to a, x resp. */
   for (i = 0; i < m; ++i) {// B & 1st row of A
      a[i] = A[i];
      x[i] = B[i];
   }

   for (; i < a_sz; ++i) a[i] = A[i];// copy A's remaining rows

/****
   for(i=0; i<m; ++i){
     for(j=0; j<m; ++j) a[i*m+j]=A[i*m+j];
     x[i]=B[i];
   }
  ****/


/* compute the LU decomposition of a row permutation of matrix a; the
   permutation itself is saved in idx[] */
   for (i = 0; i < m; ++i) {
      max = 0.0;
      for (j = 0; j < m; ++j) if ((tmp = (((a[i * m + j]) >= 0.0) ? (a[i * m + j]) : -(a[i * m + j]))) > max) max = tmp;
      if (max == 0.0) {
         astError( AST__LEVMAR, "Singular matrix A in dAx_eq_b_LU_noLapack()!", status );
         return 0;
      }
      work[i] = (1.0) / max;
   }

   for (j = 0; j < m; ++j) {

      for (i = 0; i < j; ++i) {
         sum = a[i * m + j];
         for (k = 0; k < i; ++k) sum -= a[i * m + k] * a[k * m + j];
         a[i * m + j] = sum;
      }
      max = 0.0;

      for (i = j; i < m; ++i) {
         sum = a[i * m + j];
         for (k = 0; k < j; ++k) sum -= a[i * m + k] * a[k * m + j];
         a[i * m + j] = sum;
         if ((tmp = work[i] * (((sum) >= 0.0) ? (sum) : -(sum))) >= max) {
            max = tmp;
            maxi = i;
         }
      }

      if (j != maxi) {
         for (k = 0; k < m; ++k) {
            tmp = a[maxi * m + k];
            a[maxi * m + k] = a[j * m + k];
            a[j * m + k] = tmp;
         }
         work[maxi] = work[j];
      }

      idx[j] = maxi;
      if (a[j * m + j] == 0.0) a[j * m + j] = DBL_EPSILON;
      if (j != m - 1) {
         tmp = (1.0) / (a[j * m + j]);
         for (i = j + 1; i < m; ++i) a[i * m + j] *= tmp;
      }
   }

/* The decomposition has now replaced a. Solve the linear system using
 * forward and back substitution
 */
   for (i = k = 0; i < m; ++i) {
      j = idx[i];
      sum = x[j];
      x[j] = x[i];
      if (k != 0) for (j = k - 1; j < i; ++j) sum -= a[i * m + j] * x[j];
         else if (sum != 0.0) k = i + 1;
      x[i] = sum;
   }

   for (i = m - 1; i >= 0; --i) {
      sum = x[i];
      for (j = i + 1; j < m; ++j) sum -= a[i * m + j] * x[j];
      x[i] = sum / a[i * m + i];
   }
   return 1;
}








