/* static char sccsid[] = "@(#) ST-ECF tc/h/trigo.h	4.1	12/6/91"; */
/*+++++++++
.Header		trigo.h
.LANGUAGE	C
.AUTHOR		Francois Ochsenbein [ESO-IPG]
.KEYWORDS	Trigonometric mathematical functions
.COMMENTS	This header contains declarations for mathematical functions 
		and macros for spherical/hyperbolic transformations.

.VERSION 1.0	21-Oct-1985: Creation
.VERSION 1.1	05-Dec-1988: Transformed some macros to functions
----------------*/

#ifndef  TRIGO_DEF
#define  TRIGO_DEF	0

#include <math.h>	/* Use system Math Library */
#include "compiler.h"	/* If function templates are allowed */

              					/* constants */
#undef PI
#define PI   	3.14159265358979325e0
#define DEG  	(180.e0/PI)  			/* radians to degrees 	*/
			/* =57.295779513082320e0     			*/

/*=========================================================================
		1. Trigonometric functions
 *=========================================================================*/

/* Standard:  	sin(x) cos(x) tan(x) sinh(x) cosh(x) tanh(x)	*/

	/* Argument in degrees: */

#if _TEMPLATES_

double cosd (double x); 
double sind (double x); 
double tand (double x); 
double acosd(double x);
double asind(double x);
double atand(double x);
double atan2d(double x, double y);    
#else
double acosd(), asind(), atand(), atan2d(); 
double cosd(), sind(), tand();
#endif

/*=========================================================================
		2. Spherical functions
 *=========================================================================*/

#if _TEMPLATES_
int 	tr_ou 	(double o[2] , double u[3]);
int 	tr_uo 	(double u[3] , double o[2]);
int     tr_uu   (double u1[3], double u2[3], double R[3][3]);
int     tr_uu1  (double u1[3], double u2[3], double R[3][3]);
int     tr_Euler(double Euler_angles[3], double R[3][3] );
int     prej_u  (double u0[3], double u1[3], double eq0, double eq1 );

	/* Surface of a `rectangle' on the sphere */
double	surf_o	(double o1[2], double o2[2]);		/* Pos. in degrees */
double	surf_p	(double p1[2], double p2[2]);		/* On gnomonic proj. */
	/* Distances between two points on the sphere (degrees) */
double	s2d_u	(double u1[3], double u2[3]);
double	s2d_o	(double o1[2], double o2[2]);
double	s2d_p	(double p1[2], double p2[2]);
double	dist_u	(double u1[3], double u2[3]);
double	dist_o	(double o1[2], double o2[2]);
double	dist_p	(double p1[2], double p2[2]);
#else
double	surf_o(), surf_p();
double	s2d_o(),   s2d_p(),   s2d_u();
double	dist_o(),  dist_p(), dist_u();
#endif

/*=========================================================================
		3. Other standard Math Functions
 *=========================================================================*/

/* Standard: log(x) log10(x) exp(x) sqrt(x) 	*/
/* Standard: hypoth(x,y)= sqrt(x*x + y*y)	*/
/* Standard: pow(x,y) 	= x**y			*/

#if _TEMPLATES_
double sinc (double x);		/* sin(x)/x */
double asinc(double x);		/* asin(x)/x */
double acosh(double x);
double asinh(double x);
double atanh(double x);
#else
double sinc(), asinc();
double acosh(), asinh(), atanh();
#endif



/*=========================================================================
		4. Arithmetic operations
 *=========================================================================*/

/* Standard: ceil(x) floor(x) fabs(x) 	*/
/* Standard: fmod(x,y) = x%y, with same sign as x */

					/* trigo functions in degrees*/
/*=========================================================================
		5. Mantissa / Exponent Conversions
 *=========================================================================*/

/* Standard: m = frexp (x, &e)	Returns m and e such that x = m * 2**e 	
					with |m| in range [0.5, 1[	*/
/* Standard: y = ldexp (m,  e) 	Returns y = m * 2**e 			*/
/* Standard: f = modf  (x, &E) 	Returns f and E (double) with x = E + f
					f = fractional part with same
						sign as x		*/
						
#endif
                                                

                                                               
                                        
