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
	$Log: wvmTau.h,v $
	Revision 1.10  2011/12/02 21:54:14  timj
	New simpler pwv to tau polynomial
	
	Remove the airmass argument as we no longer need it.
	
	Revision 1.9  2008/06/11 03:33:27  rkackley
	Added tau2pwv function and tau_table lookup table (both supplied by J.Balfour).
	
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

#define TAU_DEBUG 0		/* Debug level: 6 is verbose, 0 is off*/

