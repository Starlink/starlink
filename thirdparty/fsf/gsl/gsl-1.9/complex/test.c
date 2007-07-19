/* complex/test.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 Brian Gough
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
#include <stdio.h>
#include <gsl/gsl_ieee_utils.h>
#include <gsl/gsl_test.h>
#include <gsl/gsl_math.h>
#include <gsl/gsl_complex.h>
#include <gsl/gsl_complex_math.h>

struct f
{
  char *name;
  double (*f) (gsl_complex z);
  double x;
  double y;
  double fx;
  double fy;
};

struct fz
{
  char *name;
  gsl_complex (*f) (gsl_complex z);
  double x;
  double y;
  double fx;
  double fy;
};

struct freal
{
  char *name;
  gsl_complex (*f) (double x);
  double x;
  double fx;
  double fy;
};


#define FN(x) "gsl_complex_" #x, gsl_complex_ ## x
#define ARG(x,y) x, y
#define RES(x,y) x, y

struct f list[] =
{
#include "results1.h"
  {"", 0, 0, 0, 0, 0}
};


struct fz listz[] =
{
#include "results.h"
  {"", 0, 0, 0, 0, 0}
};

struct freal listreal[] =
{
#include "results_real.h"
  {"", 0, 0, 0, 0}
};


int
main (void)
{
  size_t i = 0;

  gsl_ieee_env_setup();


  for (i = 0 ; i < 10; i++) 
    {
      double r = (i - 5.0) * 0.3 ;
      double t = 2.0 * M_PI * i / 5 ;
      double x = r * cos(t), y = r * sin(t) ;
      gsl_complex z = gsl_complex_polar (r, t) ;
      gsl_test_rel (GSL_REAL(z), x, 10 * GSL_DBL_EPSILON, "gsl_complex_polar real part at (r=%g,t=%g)", r, t);
      
      gsl_test_rel (GSL_IMAG(z), y, 10 * GSL_DBL_EPSILON, "gsl_complex_polar imag part at (r=%g,t=%g)", r, t);
    }
    
    i = 0;

  while (list[i].f)
    {
      struct f t = list[i];
      gsl_complex z = gsl_complex_rect (t.x, t.y);
      double f = (t.f) (z);
      gsl_test_rel (f, t.fx, 10 * GSL_DBL_EPSILON, "%s at (%g,%g)", t.name, t.x, t.y);
      i++;
    }

  i = 0;

  while (listz[i].f)
    {
      struct fz t = listz[i];
      gsl_complex z = gsl_complex_rect (t.x, t.y);
      gsl_complex fz = (t.f) (z);
      double fx = GSL_REAL (fz), fy = GSL_IMAG (fz);

#ifdef DEBUG
      printf("x = "); gsl_ieee_fprintf_double (stdout, &t.x); printf("\n");
      printf("y = "); gsl_ieee_fprintf_double (stdout, &t.y); printf("\n");
      printf("fx = "); gsl_ieee_fprintf_double (stdout, &fx); printf("\n");
      printf("ex = "); gsl_ieee_fprintf_double (stdout, &t.fx); printf("\n");
      printf("fy = "); gsl_ieee_fprintf_double (stdout, &fy); printf("\n");
      printf("ey = "); gsl_ieee_fprintf_double (stdout, &t.fy); printf("\n");
#endif

      gsl_test_rel (fx, t.fx, 10 * GSL_DBL_EPSILON, "%s real part at (%g,%g)", t.name, t.x, t.y);
      gsl_test_rel (fy, t.fy, 10 * GSL_DBL_EPSILON, "%s imag part at (%g,%g)", t.name, t.x, t.y);
      i++;
    }


  i = 0;

  while (listreal[i].f)
    {
      struct freal t = listreal[i];
      gsl_complex fz = (t.f) (t.x);
      double fx = GSL_REAL (fz), fy = GSL_IMAG (fz);

#ifdef DEBUG
      printf("x = "); gsl_ieee_fprintf_double (stdout, &t.x); printf("\n");
      printf("fx = "); gsl_ieee_fprintf_double (stdout, &fx); printf("\n");
      printf("ex = "); gsl_ieee_fprintf_double (stdout, &t.fx); printf("\n");
      printf("fy = "); gsl_ieee_fprintf_double (stdout, &fy); printf("\n");
      printf("ey = "); gsl_ieee_fprintf_double (stdout, &t.fy); printf("\n");
#endif

      gsl_test_rel (fx, t.fx, 10 * GSL_DBL_EPSILON, "%s real part at (%g,0)", t.name, t.x);
      gsl_test_rel (fy, t.fy, 10 * GSL_DBL_EPSILON, "%s imag part at (%g,0)", t.name, t.x);
      i++;
    }

  exit (gsl_test_summary ());
}
