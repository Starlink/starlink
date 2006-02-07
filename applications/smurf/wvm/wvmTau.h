/* 		
 *  Module name:
      wvmTau.h

 *  Function:
      Include file for wvmTau

 *  Description:
      This file provides prototypes for the functions used in the
      wvmTau.c

 *  Language:
      C

 *  Support: Matt Rippa

 
 *  History:
	$Log$
	Revision 0.1  2006/02/07 22:29:51  agibb
	WVM files necessary for calculating tau from the WVM temperature measurements
	
	Revision 0.1  2006/01/25 20:43:37  echapin
	Initial version
	
	Revision 1.8  2003/07/03 01:26:27  berndw
	Reset reasonable values.
	
	Revision 1.7  2003/07/03 01:13:49  berndw
	Fixed comma
	
	Revision 1.6  2003/07/03 01:08:17  berndw
	Committed corny values
	
	Revision 1.5  2003/05/15 22:10:52  berndw
	Test commit for demonstration.
	
	Revision 1.4  2003/05/12 23:53:09  mrippa
	Turned off debugging
	
	Revision 1.3  2003/05/10 21:26:39  mrippa
	Initialized coefficient arrays set to static const
	
	Revision 1.2  2003/05/10 21:24:34  mrippa
	wvm_cso_coefs now static const since we
	initialize in the header file.
	
	Revision 1.1  2003/05/09 21:58:29  mrippa
	Header file for wcmTau.c. The function pwv2tau() is defined
	in wvmCal.h.
	
 */
#define WVM_COEFF_SIZE 11	/* Size of Coeffs array */
#define MMH2O_MAX 7.747		/* Saturation level of calculation */
#define TAU_DEBUG 0		/* Debug level: 6 is verbose, 0 is off*/


/* 
   Descibe the TAU model coefficients here. Added a comment to show
   Bernd how to commit.
 */
static const double wvm_cso_coefs[WVM_COEFF_SIZE] = {
  -0.37325545, 38.769126, -13.05137, -25.278241, 
  31.155206, -16.233469, 4.8036578, -0.86140855, 
  0.092759443, -0.0055237545, 0.00013989644
};

/* Describe the AIRMASS correction coefficients here. */
static const double coefs_m1[3] = {0.038863637, -0.18578918, 0.034048884};
static const double coefs_m2[3] = {-0.0073803229, 0.027694585, -0.010215906};
static const double coefs_c[3] = {-0.026203715, 1.1069635, 0.073422855};
