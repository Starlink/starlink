/* Author: G. Jungman
 */
#include <config.h>
#include <stdlib.h>
#include <gsl/gsl_ieee_utils.h>

#include <gsl/gsl_qrng.h>
#include <gsl/gsl_test.h>

void test_sobol(void)
{
  int status = 0;
  double v[3];
  /* int i; */

  /* test in dimension 2 */
  gsl_qrng * g = gsl_qrng_alloc(gsl_qrng_sobol, 2);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.375 || v[1] != 0.375 );
  gsl_qrng_free(g);
  
  gsl_test (status, "Sobol d=2");

  status = 0;
  /* test in dimension 3 */
  g = gsl_qrng_alloc(gsl_qrng_sobol, 3);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 || v[2] != 0.25 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.375 || v[1] != 0.375 || v[2] != 0.625 );

  gsl_test (status, "Sobol d=3");

  status = 0;
  gsl_qrng_init(g);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 || v[2] != 0.25 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.375 || v[1] != 0.375 || v[2] != 0.625 );
  gsl_qrng_free(g);

  gsl_test (status, "Sobol d=3 (reinitialized)");
}


void test_nied2(void)
{
  int status = 0;
  double v[3];
  /* int i; */

  /* test in dimension 2 */
  gsl_qrng * g = gsl_qrng_alloc(gsl_qrng_niederreiter_2, 2);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.75 || v[1] != 0.25 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 );
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.625 || v[1] != 0.125 );
  gsl_qrng_free(g);

  gsl_test (status, "Niederreiter d=2");

  status = 0;

  /* test in dimension 3 */
  g = gsl_qrng_alloc(gsl_qrng_niederreiter_2, 3);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.75 || v[1] != 0.25 || v[2] != 0.3125 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 || v[2] != 0.5625 );
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.625 || v[1] != 0.125 || v[2] != 0.6875 );

  gsl_test (status, "Niederreiter d=3");

  status = 0;

  gsl_qrng_init(g);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.75 || v[1] != 0.25 || v[2] != 0.3125 );
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.25 || v[1] != 0.75 || v[2] != 0.5625 );
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  gsl_qrng_get(g, v);
  status += ( v[0] != 0.625 || v[1] != 0.125 || v[2] != 0.6875 );
  gsl_qrng_free(g);


  gsl_test (status, "Niederreiter d=3 (reinitialized)");
}


int main()
{

  gsl_ieee_env_setup ();

  test_sobol();
  test_nied2();

  exit (gsl_test_summary ());
}
