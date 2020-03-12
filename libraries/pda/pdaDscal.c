/*Translated by FOR_C, v3.4.2 (-), on 07/09/115 at 08:30:06 */
/*FOR_C Options SET: ftn=u io=c no=p op=aimnv s=dbov str=l x=f - prototypes */
#include <math.h>
#include "fcrt.h"
#include "pda.h"
#include <stdlib.h>
void /*FUNCTION*/ pdaDscal(
long n,
double a,
double x[],
long incx)
{
	long int _d_l, _d_m, _do0, _do1, i, m, mp1, ns;
		/* OFFSET Vectors w/subscript range: 1 to dimension */
	double *const X = &x[0] - 1;
		/* end of OFFSET VECTORS */

	/* Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
	 * ALL RIGHTS RESERVED.
	 * Based on Government Sponsored Research NAS7-03001.
	 *>> 1994-11-11 DSCAL  Krogh   Declared all vars.
	 *>> 1994-10-20 DSCAL  Krogh  Changes to use M77CON
	 *>> 1985-08-02 DSCAL  Lawson  Initial code.
	 *--D replaces "?": ?SCAL
	 *
	 *     REPLACE X BY  A*X.
	 *     FOR I = 0 TO N-1, REPLACE X(1+I*INCX) WITH  A * X(1+I*INCX)
	 * */
	if (n <= 0)
		return;
	if (incx == 1)
		goto L_20;

	/*        CODE FOR INCREMENTS NOT EQUAL TO 1.
	 * */
	ns = n*incx;
	for (i = 1, _do0=DOCNT(i,ns,_do1 = incx); _do0 > 0; i += _do1, _do0--)
	{
		X[i] *= a;
	}
	return;

	/*        CODE FOR INCREMENTS EQUAL TO 1.
	 *
	 *
	 *        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5.
	 * */
L_20:
	m = n%5;
	if (m == 0)
		goto L_40;
	for (i = 1; i <= m; i++)
	{
		X[i] *= a;
	}
	if (n < 5)
		return;
L_40:
	mp1 = m + 1;
	for (i = mp1; i <= n; i += 5)
	{
		X[i] *= a;
		X[i + 1] *= a;
		X[i + 2] *= a;
		X[i + 3] *= a;
		X[i + 4] *= a;
	}
	return;
} /* end of function */

