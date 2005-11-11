/* static char sccsid[] = "@(#) ST-ECF tc/src/trigod.c	4.1	12/6/91"; */
/*+++	Trigonometric Function in degrees
.TYPE		Module
.IDENTIFICATION	trigod.c
.LANGUAGE	C
.AUTHOR		Francois Ochsenbein [ESO-IPG]
.CATEGORY	Trigonometric Functions
.COMMENTS	Arguments in degrees

.VERSION 1.0	21-Oct-1985: Creation
.VERSION 1.1	05-Dec-1988: Added inverse functions.
________________________________________*/

#ifndef __hpux__ 

#include "trigo.h"
#include "osdefos.h"

STATIC double int_part;

#define DOUBLE_MAX	1.7e38		/* max floating value 	*/

/*=========================================================================
 *			cosd
 *=========================================================================*/
double cosd (x)
/*+++
.DES Computation of cosine (argument in degrees)
.RET cosine (double)
.REM 
---*/
  	double x;      /* argument in degrees */
{ 
	double argument;
	char	sign;
	
  argument = modf (fabs(x)/360.e0, &int_part);
  sign = 0;
  if (argument > .5e0) 	argument = 1.e0 - argument;
  if (argument > .25e0) argument = .5e0 - argument, sign = 1;
  if (argument > .125e0) 
  	argument = sin( (PI*2) * (.25e0 - argument));
  else 	argument = cos( (PI*2) * argument);
  if (sign)	argument = -argument;
  return (argument);
}

/*=========================================================================
 *			sind
 *=========================================================================*/
double sind(x)
/*+++
.DES Computes the sine (argument in degrees)
.RET sine of argument (double)
.REM No tracing
---*/
  	double x;      /* argument in degrees */
{ 
	double argument;
	char	sign;
	
  sign = (x >= 0.e0 ? 0 : 1);
  argument = modf (fabs(x)/360.e0, &int_part);
  if (argument > .5e0) 	argument = 1.e0 - argument, sign ^= 1;
  if (argument > .25e0) argument = .5e0 - argument;
  if (argument > .125e0) 
  	argument = cos( (PI*2) * (.25e0 - argument));
  else 	argument = sin( (PI*2) * argument);
  if (sign)	argument = -argument;
  return (argument);
}

/*=========================================================================
 *			tand
 *=========================================================================*/
double tand(x)
/*+++
.DES Computes the tangent (argument in degrees)
.RET tangent of argument (double)
.REM For +90 degrees, DOUBLE_MAX is returned; 
	For -90 degrees, -DOUBLE_MAX is returned
.REM No tracing
---*/
  	double x;      /* argument in degrees */
{ 
	double argument;

  argument = modf (fabs(x)/180.e0, &int_part);
  if (argument == .5e0)
  	argument = DOUBLE_MAX;
  else	argument = tan (PI*argument);
  return (x>0.e0 ? argument: -argument);
}

/*=========================================================================
 *			Inverse functions
 *=========================================================================*/
double atand(x)
/*+++
.DES Computes the Arc tan in degrees
.RET Arc tangent of argument (double), in range [-90, 90]
---*/
  	double x;      /* argument in degrees */
{ 
  return(DEG*atan(x));
}

/*=========================================================================*/
double atan2d(x, y)
/*+++
.DES Cartesian to polar
.RET Angle in range ]-180, 180]
---*/
  	double x;      /* argument in degrees */
  	double y;      /* argument in degrees */
{ 
  return(DEG*atan2(x, y));
}

/*=========================================================================*/
double acosd(x)
/*+++
.DES Computes the Arc cos in degrees
.RET Arc cosine of argument (double), in range [0, 180]
.REM Range of argument [-1, +1]
---*/
  	double x;      /* argument in degrees */
{ 
  return(DEG*acos(x));
}

/*=========================================================================*/
double asind(x)
/*+++
.DES Computes the Arc sine in degrees
.RET Arc tangent of argument (double) in range [-90, 90]
.REM Range of argument [-1, +1]
---*/
  	double x;      /* argument in degrees */
{ 
  return(DEG*asin(x));
}

/*=========================================================================*/

                                                           
#endif /* HP_UX */                                                               
                                             
                                        
