
/* System definitions */
# include <math.h>

/******************************************************************************/

double rint( x )

/*
*  Name :
*     rint
*
*  Purpose :
*     VAX/VMS version of UNIX rint function
*
*  Invocation :
*     rint( x )
*
*  Description :
*     Return an integer ( represented as a double ) nearest x
*     in the direction of the prevailing rounding mode
*
*  Arguments :
*     x = double
*        Input argument
*
*  Algorithm :
*     Return an integer ( represented as a double ) nearest x
*     in the direction of the prevailing rounding mode
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*
*  History :
*     30-NOV-1990 (NE):
*        Orignal version
*/

/* Arguments Given */
double x;

{
if ( x >= 0 )
   return ( x - floor(x) < 0.5 ? floor(x) : ceil(x) );
else
   return ( ceil(x) - x < 0.5 ? ceil(x) : floor(x) );
}

