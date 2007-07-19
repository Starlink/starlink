/* statistics/covar_source.c
 * 
 * Copyright (C) 1996, 1997, 1998, 1999, 2000 Jim Davies, Brian Gough
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

static double
FUNCTION(compute,covariance) (const BASE data1[], const size_t stride1,
                              const BASE data2[], const size_t stride2,
                              const size_t n, 
                              const double mean1, const double mean2);

static double
FUNCTION(compute,covariance) (const BASE data1[], const size_t stride1,
                              const BASE data2[], const size_t stride2,
                              const size_t n, 
                              const double mean1, const double mean2)
{
  /* takes a dataset and finds the covariance */

  long double covariance = 0 ;

  size_t i;

  /* find the sum of the squares */
  for (i = 0; i < n; i++)
    {
      const long double delta1 = (data1[i * stride1] - mean1);
      const long double delta2 = (data2[i * stride2] - mean2);
      covariance += (delta1 * delta2 - covariance) / (i + 1);
    }

  return covariance ;
}

double 
FUNCTION(gsl_stats,covariance_m) (const BASE data1[], const size_t stride1, 
                                  const BASE data2[], const size_t stride2, 
                                  const size_t n, 
                                  const double mean1, const double mean2)
{
  const double covariance = FUNCTION(compute,covariance) (data1, stride1,
                                                          data2, stride2,
                                                          n, 
                                                          mean1, mean2);
  
  return covariance * ((double)n / (double)(n - 1));
}

double 
FUNCTION(gsl_stats,covariance) (const BASE data1[], const size_t stride1,
                                const BASE data2[], const size_t stride2,
                                const size_t n)
{
  const double mean1 = FUNCTION(gsl_stats,mean) (data1, stride1, n);
  const double mean2 = FUNCTION(gsl_stats,mean) (data2, stride2, n);

  return FUNCTION(gsl_stats,covariance_m)(data1, stride1, 
                                          data2, stride2, 
                                          n, 
                                          mean1, mean2);
}


