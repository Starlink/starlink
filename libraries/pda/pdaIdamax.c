/*Translated by FOR_C, v3.4.2 (-), on 07/09/115 at 08:30:07 */
/*FOR_C Options SET: ftn=u io=c no=p op=aimnv s=dbov str=l x=f - prototypes */
#include <math.h>
#include "fcrt.h"
#include "pda.h"
#include <stdlib.h>
long /*FUNCTION*/ pdaIdamax(
long n,
double x[],
long incx)
{
	long int _d_l, _d_m, _do0, _do1, i, idamax_v, ii, ns;
	double xmag, xmax;
		/* OFFSET Vectors w/subscript range: 1 to dimension */
	double *const X = &x[0] - 1;
		/* end of OFFSET VECTORS */

	/* Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
	 * ALL RIGHTS RESERVED.
	 * Based on Government Sponsored Research NAS7-03001.
	 *>> 1994-11-11 IDAMAX  Krogh   Declared all vars.
	 *>> 1994-10-20 IDAMAX Krogh  Changes to use M77CON
	 *>> 1994-04-19 IDAMAX Krogh   Conveted to use generic intrinsics.
	 *>> 1987-12-09 IDAMAX Lawson  Initial code.
	 *--D replaces "?": I?AMAX
	 *
	 *     FIND SMALLEST INDEX OF MAXIMUM MAGNITUDE OF X.
	 *     IDAMAX =  FIRST I, I = 1 TO N, TO MINIMIZE  ABS(X(1-INCX+I*INCX))
	 * */
	idamax_v = 0;
	if (n <= 0)
		return( idamax_v );
	idamax_v = 1;
	if (n <= 1)
		return( idamax_v );
	if (incx == 1)
		goto L_20;

	/*        CODE FOR INCREMENTS NOT EQUAL TO 1.
	 * */
	xmax = fabs( X[1] );
	ns = n*incx;
	ii = 1;
	for (i = 1, _do0=DOCNT(i,ns,_do1 = incx); _do0 > 0; i += _do1, _do0--)
	{
		xmag = fabs( X[i] );
		if (xmag <= xmax)
			goto L_5;
		idamax_v = ii;
		xmax = xmag;
L_5:
		ii += 1;
	}
	return( idamax_v );

	/*        CODE FOR INCREMENTS EQUAL TO 1.
	 * */
L_20:
	xmax = fabs( X[1] );
	for (i = 2; i <= n; i++)
	{
		xmag = fabs( X[i] );
		if (xmag <= xmax)
			goto L_30;
		idamax_v = i;
		xmax = xmag;
L_30:
		;
	}
	return( idamax_v );
} /* end of function */

