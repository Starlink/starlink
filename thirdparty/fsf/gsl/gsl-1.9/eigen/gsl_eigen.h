/* eigen/gsl_eigen.h
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 Gerard Jungman, Brian Gough
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

#ifndef __GSL_EIGEN_H__
#define __GSL_EIGEN_H__

#include <gsl/gsl_vector.h>
#include <gsl/gsl_matrix.h>

#undef __BEGIN_DECLS
#undef __END_DECLS
#ifdef __cplusplus
# define __BEGIN_DECLS extern "C" {
# define __END_DECLS }
#else
# define __BEGIN_DECLS /* empty */
# define __END_DECLS /* empty */
#endif

__BEGIN_DECLS

typedef struct {
  size_t size;
  double * d;
  double * sd;
} gsl_eigen_symm_workspace;

gsl_eigen_symm_workspace * gsl_eigen_symm_alloc (const size_t n);
void gsl_eigen_symm_free (gsl_eigen_symm_workspace * w);
int gsl_eigen_symm (gsl_matrix * A, gsl_vector * eval, gsl_eigen_symm_workspace * w);

typedef struct {
  size_t size;
  double * d;
  double * sd;
  double * gc;
  double * gs;
} gsl_eigen_symmv_workspace;

gsl_eigen_symmv_workspace * gsl_eigen_symmv_alloc (const size_t n);
void gsl_eigen_symmv_free (gsl_eigen_symmv_workspace * w);
int gsl_eigen_symmv (gsl_matrix * A, gsl_vector * eval, gsl_matrix * evec, gsl_eigen_symmv_workspace * w);

typedef struct {
  size_t size;           /* matrix size */
  size_t max_iterations; /* max iterations since last eigenvalue found */
  size_t n_iter;         /* number of iterations since last eigenvalue found */
  size_t n_evals;        /* number of eigenvalues found so far */

  gsl_vector *hv2;       /* temporary 2-by-1 householder vector */
  gsl_vector *hv3;       /* temporary 3-by-1 householder vector */

  int compute_t;         /* compute Schur form T = Z^t A Z */

  gsl_matrix *H;         /* pointer to Hessenberg matrix */

  gsl_matrix *Z;         /* pointer to Schur vector matrix */
} gsl_eigen_francis_workspace;

gsl_eigen_francis_workspace * gsl_eigen_francis_alloc (void);
void gsl_eigen_francis_free (gsl_eigen_francis_workspace * w);
void gsl_eigen_francis_T (const int compute_t,
                          gsl_eigen_francis_workspace * w);
int gsl_eigen_francis (gsl_matrix * H, gsl_vector_complex * eval,
                       gsl_eigen_francis_workspace * w);
int gsl_eigen_francis_Z (gsl_matrix * H, gsl_vector_complex * eval,
                         gsl_matrix * Z,
                         gsl_eigen_francis_workspace * w);

typedef struct {
  size_t size;                 /* size of matrices */
  gsl_vector *diag;            /* diagonal matrix elements from balancing */
  gsl_vector *tau;             /* Householder coefficients */
  gsl_matrix *Z;               /* pointer to Z matrix */
  int do_balance;              /* perform balancing transformation? */
  size_t n_evals;              /* number of eigenvalues found */

  gsl_eigen_francis_workspace *francis_workspace_p;
} gsl_eigen_nonsymm_workspace;

gsl_eigen_nonsymm_workspace * gsl_eigen_nonsymm_alloc (const size_t n);
void gsl_eigen_nonsymm_free (gsl_eigen_nonsymm_workspace * w);
void gsl_eigen_nonsymm_params (const int compute_t, const int balance,
                               gsl_eigen_nonsymm_workspace *w);
int gsl_eigen_nonsymm (gsl_matrix * A, gsl_vector_complex * eval,
                       gsl_eigen_nonsymm_workspace * w);
int gsl_eigen_nonsymm_Z (gsl_matrix * A, gsl_vector_complex * eval,
                         gsl_matrix * Z, gsl_eigen_nonsymm_workspace * w);

typedef struct {
  size_t size;                 /* size of matrices */
  gsl_vector *work;            /* scratch workspace */
  gsl_vector *work2;           /* scratch workspace */
  gsl_vector *work3;           /* scratch workspace */

  gsl_matrix *Z;               /* pointer to Schur vectors */

  gsl_eigen_nonsymm_workspace *nonsymm_workspace_p;
} gsl_eigen_nonsymmv_workspace;

gsl_eigen_nonsymmv_workspace * gsl_eigen_nonsymmv_alloc (const size_t n);
void gsl_eigen_nonsymmv_free (gsl_eigen_nonsymmv_workspace * w);
int gsl_eigen_nonsymmv (gsl_matrix * A, gsl_vector_complex * eval,
                        gsl_matrix_complex * evec,
                        gsl_eigen_nonsymmv_workspace * w);
int gsl_eigen_nonsymmv_Z (gsl_matrix * A, gsl_vector_complex * eval,
                          gsl_matrix_complex * evec, gsl_matrix * Z,
                          gsl_eigen_nonsymmv_workspace * w);

typedef struct {
  size_t size;
  double * d;
  double * sd;
  double * tau;
} gsl_eigen_herm_workspace;

gsl_eigen_herm_workspace * gsl_eigen_herm_alloc (const size_t n);
void gsl_eigen_herm_free (gsl_eigen_herm_workspace * w);
int gsl_eigen_herm (gsl_matrix_complex * A, gsl_vector * eval,
                         gsl_eigen_herm_workspace * w);

typedef struct {
  size_t size;
  double * d;
  double * sd;
  double * tau;
  double * gc;
  double * gs;
} gsl_eigen_hermv_workspace;

gsl_eigen_hermv_workspace * gsl_eigen_hermv_alloc (const size_t n);
void gsl_eigen_hermv_free (gsl_eigen_hermv_workspace * w);
int gsl_eigen_hermv (gsl_matrix_complex * A, gsl_vector * eval, 
                           gsl_matrix_complex * evec,
                           gsl_eigen_hermv_workspace * w);



typedef enum {
  GSL_EIGEN_SORT_VAL_ASC,
  GSL_EIGEN_SORT_VAL_DESC,
  GSL_EIGEN_SORT_ABS_ASC,
  GSL_EIGEN_SORT_ABS_DESC
}
gsl_eigen_sort_t;

/* Sort eigensystem results based on eigenvalues.
 * Sorts in order of increasing value or increasing
 * absolute value.
 *
 * exceptions: GSL_EBADLEN
 */

int gsl_eigen_symmv_sort(gsl_vector * eval, gsl_matrix * evec,
                         gsl_eigen_sort_t sort_type);

int gsl_eigen_hermv_sort(gsl_vector * eval, gsl_matrix_complex * evec,
                         gsl_eigen_sort_t sort_type);

int gsl_eigen_nonsymmv_sort(gsl_vector_complex * eval,
                            gsl_matrix_complex * evec,
                            gsl_eigen_sort_t sort_type);


/* The following functions are obsolete: */

/* Eigensolve by Jacobi Method
 *
 * The data in the matrix input is destroyed.
 *
 * exceptions: 
 */
int
gsl_eigen_jacobi(gsl_matrix * matrix,
                      gsl_vector * eval,
                      gsl_matrix * evec,
                      unsigned int max_rot, 
                      unsigned int * nrot);


/* Invert by Jacobi Method
 *
 * exceptions: 
 */
int
gsl_eigen_invert_jacobi(const gsl_matrix * matrix,
                             gsl_matrix * ainv,
                             unsigned int max_rot);



__END_DECLS

#endif /* __GSL_EIGEN_H__ */
