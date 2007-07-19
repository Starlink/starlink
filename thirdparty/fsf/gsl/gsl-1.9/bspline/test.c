/* bspline/test.c
 * 
 * Copyright (C) 2006 Brian Gough
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
#include <gsl/gsl_test.h>
#include <gsl/gsl_errno.h>
#include <gsl/gsl_bspline.h>
#include <gsl/gsl_ieee_utils.h>

void
test_bspline (gsl_bspline_workspace * bw)
{
  gsl_vector *B;
  size_t i, j;
  size_t n = 100;
  size_t ncoeffs = gsl_bspline_ncoeffs (bw);
  size_t order = gsl_bspline_order (bw);
  size_t nbreak = gsl_bspline_nbreak (bw);
  double a = gsl_bspline_breakpoint (0, bw);
  double b = gsl_bspline_breakpoint (nbreak - 1, bw);

  B = gsl_vector_alloc (ncoeffs);

  for (i = 0; i < n; i++)
    {
      double xi = a + (b - a) * (i / (n - 1.0));
      double sum = 0;
      gsl_bspline_eval (xi, B, bw);

      for (j = 0; j < ncoeffs; j++)
        {
          double Bj = gsl_vector_get (B, j);
          int s = (Bj < 0 || Bj > 1);
          gsl_test (s,
                    "basis-spline coefficient %u is in range [0,1] for x=%g",
                    j, xi);
          sum += Bj;
        }

      gsl_test_rel (sum, 1.0, order * GSL_DBL_EPSILON,
                    "basis-spline order %u is normalized for x=%g", order,
                    xi);
    }

  gsl_vector_free (B);
}



int
main (int argc, char **argv)
{
  int status = 0;
  size_t order, breakpoints, i;

  gsl_ieee_env_setup ();

  argc = 0;                     /* prevent warnings about unused parameters */
  argv = 0;

  for (order = 1; order < 10; order++)
    {
      for (breakpoints = 2; breakpoints < 100; breakpoints++)
        {
          double a = -1.23 * order, b = 45.6 * order;
          gsl_bspline_workspace *bw = gsl_bspline_alloc (order, breakpoints);
          gsl_bspline_knots_uniform (a, b, bw);
          test_bspline (bw);
          gsl_bspline_free (bw);
        }
    }


  for (order = 1; order < 10; order++)
    {
      for (breakpoints = 2; breakpoints < 100; breakpoints++)
        {
          double a = -1.23 * order, b = 45.6 * order;
          gsl_bspline_workspace *bw = gsl_bspline_alloc (order, breakpoints);
          gsl_vector *k = gsl_vector_alloc (breakpoints);
          for (i = 0; i < breakpoints; i++)
            {
              double f, x;
              f = sqrt (i / (breakpoints - 1.0));
              x = (1 - f) * a + f * b;
              gsl_vector_set (k, i, x);
            };
          gsl_bspline_knots (k, bw);
          test_bspline (bw);
          gsl_vector_free (k);
          gsl_bspline_free (bw);
        }
    }

  exit (gsl_test_summary ());
}
