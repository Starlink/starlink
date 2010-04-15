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
	Revision 0.3  2006/07/04 17:09:55  jbalfour
	Commenting on tau2pwv lookup table

	Revision 0.2  2006/06/30 18:22:29  jbalfour
	added tau<->pwv lookup table.

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

/* tau2pwv lookup table : These values represent the tau values for pwvs
   ranging from 0.0 to 10.0 (incrementing by 0.1).  The values were
   calculated using pwv2tau and an airmass of 1.0. */

static const double tau_table[101] = { 0.0,
    0.0298435, 0.0298455, 0.0311965, 0.0331185, 0.0354685,
    0.0382015, 0.0412965, 0.0447385, 0.0485095, 0.0525835,
    0.0569275, 0.0614995, 0.0662485, 0.0711195, 0.0760545,
    0.0810005, 0.0859085, 0.0907415, 0.0954745, 0.1000945,
    0.1046035, 0.1090125, 0.1133415, 0.1176155, 0.1218635,
    0.1261125, 0.1303885, 0.1347135, 0.1391045, 0.1435725,
    0.1481215, 0.1527495, 0.1574495, 0.1622065, 0.1670055,
    0.1718235, 0.1766405, 0.1814345, 0.1861885, 0.1908875,
    0.1955205, 0.2000875, 0.2045915, 0.2090455, 0.2134665,
    0.2178805, 0.2223155, 0.2268015, 0.2313705, 0.2360515,
    0.2408685, 0.2458405, 0.2509765, 0.2562735, 0.2617185,
    0.2672835, 0.2729275, 0.2785975, 0.2842285, 0.2897525,
    0.2951015, 0.3002145, 0.3050505, 0.3095895, 0.3138505,
    0.3178865, 0.3217955, 0.3257135, 0.3298095, 0.3342695,
    0.3392755, 0.3449695, 0.3514005, 0.3584375, 0.3656425,
    0.3721015, 0.3762215, 0.3714295, 0.3761905, 0.3809525,
    0.3857145, 0.3904765, 0.3952385, 0.4000005, 0.4047625,
    0.4095245, 0.4142865, 0.4190485, 0.4238105, 0.4285715,
    0.4333335, 0.4380955, 0.4428575, 0.4476195, 0.4523815,
    0.4571435, 0.4619055, 0.4666675, 0.4714295, 0.4761905 };
