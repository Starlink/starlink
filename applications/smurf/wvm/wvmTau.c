/* 
  File name: 
   wvmTau.c

  Description:
	Provides utility to convert millimeters of precipitable water
	vapor content into a value representing the optical depth seen
	at 225GHz relative to zenith. Otherwise known as 225Tau.
         Also available is a function to convert optical depth seen at
         225GHz relative to the zenith into a value representing
         millimeters of water vapor.

  Functions:
	double pwv2tau(double airMass, double mmH2O);
         double tau2pwv(double tau);

  History:

	$Log: wvmTau.c,v $
	Revision 1.9  2011/12/02 21:54:14  timj
	New simpler pwv to tau polynomial
	
	Remove the airmass argument as we no longer need it.
	
	Revision 1.8  2008/06/11 03:33:27  rkackley
	Added tau2pwv function and tau_table lookup table (both supplied by J.Balfour).
	
	Revision 1.7  2006/07/14 19:05:28  rkackley
	Corrected a typo in the calculation of const_c (code was using coefs_m1 when it should have been using coefs_c). Note that this bug did not affect the final tau value since const_c is arithmetically eliminated when the correction value is computed.
	
	Revision 1.6  2003/07/02 22:20:47  berndw
	Just more tests
	
	Revision 1.5  2003/07/02 22:14:33  berndw
	This is a test of cvs commit
	
	Revision 1.4  2003/05/13 23:36:16  mrippa
	Fixed array index out of bounds exception
	
	Revision 1.3  2003/05/12 03:19:08  mrippa
	Added another printf.
	
	Revision 1.2  2003/05/09 21:56:56  mrippa
	We now compute the empirical tau model on the C side.
	
	Revision 1.1  2003/05/06 03:13:41  mrippa
	Added wvmTau to convert pwv into tau value


*/


/* C Includes */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>

/* WVM includes */
#include "wvmTau.h"
#include "wvmCal.h"

/* Define the coefficients */

/* PWV to CSO TAU */
#define WVM_COEFF_SIZE 2
static const double wvm_cso_coefs[WVM_COEFF_SIZE] = {
  0.018,
  1./25.317
};

/* CSO TAU to PWV */
/* This is a simple inverse of the linear fit */
#define TAU2PWV_COEFF_SIZE 2
static const double tau2pwv_coefs[TAU2PWV_COEFF_SIZE] = {
  -0.45567,
  25.317
};


/* 
   Function:
	pwv2tau converts millimeters of water vapor at zenith into a value
	representing the optical depth seen at 225GHz relative to
	zenith. This is otherwise known as 225Tau.

   Support:
	Jessica Dempsey <j.dempsey@jach.hawaii.edu>

   Author:
	Tim Jenness <t.jenness@jach.hawaii.edu>
 */
double pwv2tau(double mmH2O_z) {
  int j;
  double tau = 0.0;

  if (TAU_DEBUG > 4)
    printf("You gave me a zenith pwv of %f\n", mmH2O_z);

  /* APPLY THE CSO TAU CONVERSION */
  for (j=0; j < WVM_COEFF_SIZE; j++) {
    tau += wvm_cso_coefs[j] * pow(mmH2O_z, j);
  }

  if (TAU_DEBUG > 3) {
    printf("Final Tau is: %f\n", tau);
  }
  return tau;
}

/*
  Function:
        tau2pwv converts the optical depth seen at 225GHz relative to
       the zenith into a value representing millimeters of water vapor.

  Author:
        Tim Jenness <t.jenness@jach.hawaii.edu>
*/

double tau2pwv(double tau) {

 int j;
 double pwv = 0.0;

 /* Polynomial */
  for (j=0; j < WVM_COEFF_SIZE; j++) {
    pwv += tau2pwv_coefs[j] * pow(tau, j);
  }
  return pwv;
}
