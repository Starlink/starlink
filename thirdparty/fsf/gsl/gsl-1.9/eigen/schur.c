/* eigen/schur.c
 * 
 * Copyright (C) 2006 Patrick Alken
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include <config.h>
#include <stdlib.h>
#include <math.h>
#include <gsl/gsl_eigen.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>
#include <gsl/gsl_linalg.h>
#include <gsl/gsl_cblas.h>

#include "schur.h"

/*
 * This module contains some routines related to manipulating the
 * Schur form of a matrix which are needed by the eigenvalue solvers
 *
 * This file contains routines based on original code from LAPACK
 * which is distributed under the modified BSD license. The LAPACK
 * routine used is DLANV2.
 */

static inline void schur_standard_form(gsl_matrix *A, gsl_complex *eval1,
                                       gsl_complex *eval2, double *cs,
                                       double *sn);

/*
gsl_schur_standardize()
  Wrapper function for schur_standard_form - convert a 2-by-2 eigenvalue
block to standard form and then update the Schur form and
Schur vectors.

Inputs: T        - Schur form
        row      - row of T of 2-by-2 block to be updated
        eval1    - where to store eigenvalue 1
        eval2    - where to store eigenvalue 2
        update_t - 1 = update the entire matrix T with the transformation
                   0 = do not update rest of T
        Z        - (optional) if non-null, accumulate transformation
*/

void
gsl_schur_standardize(gsl_matrix *T, size_t row, gsl_complex *eval1,
                      gsl_complex *eval2, int update_t, gsl_matrix *Z)
{
  const size_t N = T->size1;
  gsl_matrix_view m;
  double cs, sn;

  m = gsl_matrix_submatrix(T, row, row, 2, 2);
  schur_standard_form(&m.matrix, eval1, eval2, &cs, &sn);

  if (update_t)
    {
      gsl_vector_view xv, yv, v;

      /*
       * The above call to schur_standard_form transformed a 2-by-2 block
       * of T into upper triangular form via the transformation
       *
       * U = [ CS -SN ]
       *     [ SN  CS ]
       *
       * The original matrix T was
       *
       * T = [ T_{11} | T_{12} | T_{13} ]
       *     [   0*   |   A    | T_{23} ]
       *     [   0    |   0*   | T_{33} ]
       *
       * where 0* indicates all zeros except for possibly
       * one subdiagonal element next to A.
       *
       * After schur_standard_form, T looks like this:
       *
       * T = [ T_{11} | T_{12}  | T_{13} ]
       *     [   0*   | U^t A U | T_{23} ]
       *     [   0    |    0*   | T_{33} ]
       *
       * since only the 2-by-2 block of A was changed. However,
       * in order to be able to back transform T at the end,
       * we need to apply the U transformation to the rest
       * of the matrix T since there is no way to apply a
       * similarity transformation to T and change only the
       * middle 2-by-2 block. In other words, let
       *
       * M = [ I 0 0 ]
       *     [ 0 U 0 ]
       *     [ 0 0 I ]
       *
       * and compute
       *
       * M^t T M = [ T_{11} | T_{12} U |   T_{13}   ]
       *           [ U^t 0* | U^t A U  | U^t T_{23} ]
       *           [   0    |   0* U   |   T_{33}   ]
       *
       * So basically we need to apply the transformation U
       * to the i x 2 matrix T_{12} and the 2 x (n - i + 2)
       * matrix T_{23}, where i is the index of the top of A
       * in T.
       *
       * The BLAS routine drot() is suited for this.
       */

      if (row < (N - 2))
        {
          /* transform the 2 rows of T_{23} */

          v = gsl_matrix_row(T, row);
          xv = gsl_vector_subvector(&v.vector,
                                    row + 2,
                                    N - row - 2);

          v = gsl_matrix_row(T, row + 1);
          yv = gsl_vector_subvector(&v.vector,
                                    row + 2,
                                    N - row - 2);

          gsl_blas_drot(&xv.vector, &yv.vector, cs, sn);
        }

      if (row > 0)
        {
          /* transform the 2 columns of T_{12} */

          v = gsl_matrix_column(T, row);
          xv = gsl_vector_subvector(&v.vector,
                                    0,
                                    row);

          v = gsl_matrix_column(T, row + 1);
          yv = gsl_vector_subvector(&v.vector,
                                    0,
                                    row);

          gsl_blas_drot(&xv.vector, &yv.vector, cs, sn);
        }
    } /* if (update_t) */

  if (Z)
    {
      gsl_vector_view xv, yv;

      /*
       * Accumulate the transformation in Z. Here, Z -> Z * M
       *
       * So:
       *
       * Z -> [ Z_{11} | Z_{12} U | Z_{13} ]
       *      [ Z_{21} | Z_{22} U | Z_{23} ]
       *      [ Z_{31} | Z_{32} U | Z_{33} ]
       *
       * So we just need to apply drot() to the 2 columns
       * starting at index 'row'
       */

      xv = gsl_matrix_column(Z, row);
      yv = gsl_matrix_column(Z, row + 1);

      gsl_blas_drot(&xv.vector, &yv.vector, cs, sn);
    } /* if (Z) */
} /* gsl_schur_standardize() */

/*******************************************************
 *            INTERNAL ROUTINES                        *
 *******************************************************/

/*
schur_standard_form()
  Compute the Schur factorization of a real 2-by-2 matrix in
standard form:

[ A B ] = [ CS -SN ] [ T11 T12 ] [ CS SN ]
[ C D ]   [ SN  CS ] [ T21 T22 ] [-SN CS ]

where either:
1) T21 = 0 so that T11 and T22 are real eigenvalues of the matrix, or
2) T11 = T22 and T21*T12 < 0, so that T11 +/- sqrt(|T21*T12|) are
   complex conjugate eigenvalues

Inputs: A     - 2-by-2 matrix
        eval1 - where to store eigenvalue 1
        eval2 - where to store eigenvalue 2
        cs    - where to store cosine parameter of rotation matrix
        sn    - where to store sine parameter of rotation matrix

Notes: based on LAPACK routine DLANV2
*/

static inline void
schur_standard_form(gsl_matrix *A, gsl_complex *eval1, gsl_complex *eval2,
                    double *cs, double *sn)
{
  double a, b, c, d; /* input matrix values */
  double tmp;
  double p, z;
  double bcmax, bcmis, scale;
  double tau, sigma;
  double cs1, sn1;
  double aa, bb, cc, dd;
  double sab, sac;

  a = gsl_matrix_get(A, 0, 0);
  b = gsl_matrix_get(A, 0, 1);
  c = gsl_matrix_get(A, 1, 0);
  d = gsl_matrix_get(A, 1, 1);

  if (c == 0.0)
    {
      /*
       * matrix is already upper triangular - set rotation matrix
       * to the identity
       */
      *cs = 1.0;
      *sn = 0.0;
    }
  else if (b == 0.0)
    {
      /* swap rows and columns to make it upper triangular */

      *cs = 0.0;
      *sn = 1.0;

      tmp = d;
      d = a;
      a = tmp;
      b = -c;
      c = 0.0;
    }
  else if (((a - d) == 0.0) && (GSL_SIGN(b) != GSL_SIGN(c)))
    {
      /* the matrix has complex eigenvalues with a == d */
      *cs = 1.0;
      *sn = 0.0;
    }
  else
    {
      tmp = a - d;
      p = 0.5 * tmp;
      bcmax = GSL_MAX(fabs(b), fabs(c));
      bcmis = GSL_MIN(fabs(b), fabs(c)) * GSL_SIGN(b) * GSL_SIGN(c);
      scale = GSL_MAX(fabs(p), bcmax);
      z = (p / scale) * p + (bcmax / scale) * bcmis;

      if (z >= 4.0 * GSL_DBL_EPSILON)
        {
          /* real eigenvalues, compute a and d */

          z = p + GSL_SIGN(p) * fabs(sqrt(scale) * sqrt(z));
          a = d + z;
          d -= (bcmax / z) * bcmis;

          /* compute b and the rotation matrix */

          tau = gsl_hypot(c, z);
          *cs = z / tau;
          *sn = c / tau;
          b -= c;
          c = 0.0;
        }
      else
        {
          /*
           * complex eigenvalues, or real (almost) equal eigenvalues -
           * make diagonal elements equal
           */

          sigma = b + c;
          tau = gsl_hypot(sigma, tmp);
          *cs = sqrt(0.5 * (1.0 + fabs(sigma) / tau));
          *sn = -(p / (tau * (*cs))) * GSL_SIGN(sigma);

          /*
           * Compute [ AA BB ] = [ A B ] [ CS -SN ]
           *         [ CC DD ]   [ C D ] [ SN  CS ]
           */
          aa = a * (*cs) + b * (*sn);
          bb = -a * (*sn) + b * (*cs);
          cc = c * (*cs) + d * (*sn);
          dd = -c * (*sn) + d * (*cs);

          /*
           * Compute [ A B ] = [ CS SN ] [ AA BB ]
           *         [ C D ]   [-SN CS ] [ CC DD ]
           */
          a = aa * (*cs) + cc * (*sn);
          b = bb * (*cs) + dd * (*sn);
          c = -aa * (*sn) + cc * (*cs);
          d = -bb * (*sn) + dd * (*cs);

          tmp = 0.5 * (a + d);
          a = d = tmp;

          if (c != 0.0)
            {
              if (b != 0.0)
                {
                  if (GSL_SIGN(b) == GSL_SIGN(c))
                    {
                      /*
                       * real eigenvalues: reduce to upper triangular
                       * form
                       */
                      sab = sqrt(fabs(b));
                      sac = sqrt(fabs(c));
                      p = GSL_SIGN(c) * fabs(sab * sac);
                      tau = 1.0 / sqrt(fabs(b + c));
                      a = tmp + p;
                      d = tmp - p;
                      b -= c;
                      c = 0.0;

                      cs1 = sab * tau;
                      sn1 = sac * tau;
                      tmp = (*cs) * cs1 - (*sn) * sn1;
                      *sn = (*cs) * sn1 + (*sn) * cs1;
                      *cs = tmp;
                    }
                }
              else
                {
                  b = -c;
                  c = 0.0;
                  tmp = *cs;
                  *cs = -(*sn);
                  *sn = tmp;
                }
            }
        }
    }

  /* set eigenvalues */

  GSL_SET_REAL(eval1, a);
  GSL_SET_REAL(eval2, d);
  if (c == 0.0)
    {
      GSL_SET_IMAG(eval1, 0.0);
      GSL_SET_IMAG(eval2, 0.0);
    }
  else
    {
      tmp = sqrt(fabs(b) * fabs(c));
      GSL_SET_IMAG(eval1, tmp);
      GSL_SET_IMAG(eval2, -tmp);
    }

  /* set new matrix elements */

  gsl_matrix_set(A, 0, 0, a);
  gsl_matrix_set(A, 0, 1, b);
  gsl_matrix_set(A, 1, 0, c);
  gsl_matrix_set(A, 1, 1, d);
} /* schur_standard_form() */
