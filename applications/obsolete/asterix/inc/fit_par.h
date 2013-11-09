/*
*+  FIT_PAR.H - Global constants for fitting software
*
*    Description :
*
*     Global constants for FIT_ routines.
*
*    WARNING :
*
*     If you update this file please make equivalent changes to fit_par.h
*
*    Authors :
*
*     Trevor Ponman  (BHVAD::TJP)
*
*    History :
*
*     24 Jan 94 : Original C version. Please ensure that if you update this
*                 file you do the same to fit_par.for (DJA)
*     18 May 94 : Added MAXTIE (DJA)
*     22 Nov 94 : Increased MAXIMP to 64 (DJA)
*-
*/

#define  NDSMAX	    	32 		/* Max # datasets */
#define  NDSCMAX    	32 		/* Max # container files */
#define  NDETMAX    	8 		/* Max # detector spectra in 
					   a single spectral set */
#define  MAXCOMP    	30 		/* Max # model components */
#define  NPAMAX	    	60 		/* Max # model parameters */
#define  MAXIMP	    	64 		/* Max # implemented pmodels */
#define  MAXSTACK   	8 		/* Max # pmodels which can be stacked 
				           during computation of a model  */
#define  MAXTIE         16              /* Max # parameter ties */

#define  PARRANGE   	20.0 		/* Range for scaled parameters */
#define  MAXKEYLEN  	5 		/* Maximum length of a model key */
#define  FIT__CHISQ 	1 		/* Chi-squared statistic identifier */
#define  FIT__LOGL  	2 		/* log(likelihood) statistic 
					   identifier */
