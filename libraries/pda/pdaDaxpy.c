/*Translated by FOR_C, v3.4.2 (-), on 07/09/115 at 08:30:04 */
/*FOR_C Options SET: ftn=u io=c no=p op=aimnv s=dbov str=l x=f - prototypes */
#include <math.h>
#include "fcrt.h"
#include "pda.h"
#include <stdlib.h>
void /*FUNCTION*/ pdaDaxpy(
long n,
double a,
double x[],
long incx,
double y[],
long incy)
{
	long int _d_l, _d_m, _do0, _do1, i, ix, iy, m, mp1, ns;
		/* OFFSET Vectors w/subscript range: 1 to dimension */
	double *const X = &x[0] - 1;
	double *const Y = &y[0] - 1;
		/* end of OFFSET VECTORS */

	/* Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
	 * ALL RIGHTS RESERVED.
	 * Based on Government Sponsored Research NAS7-03001.
	 *>> 2006-06-07 DAXPY  Krogh  Removed arithmetic if
	 *>> 1994-11-11 DAXPY  Krogh  Declared all vars.
	 *>> 1994-10-20 DAXPY  Krogh  Changes to use M77CON
	 *>> 1985-08-02 DAXPY  Lawson Initial code.
	 *--D replaces "?": ?AXPY
	 *
	 *     OVERWRITE Y WITH A*X + Y.
	 *     FOR I = 0 TO N-1, REPLACE  Y(LY+I*INCY) WITH A*X(LX+I*INCX) +
	 *       Y(LY+I*INCY), WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N,
	 *       AND LY IS DEFINED IN A SIMILAR WAY USING INCY.
	 * */
	if (n <= 0 || a == 0.e0)
		return;
	if ((incx != incy) || (incx < 0))
	{
		/*        CODE FOR NONEQUAL OR NONPOSITIVE INCREMENTS. */
		ix = 1;
		iy = 1;
		if (incx < 0)
			ix = (-n + 1)*incx + 1;
		if (incy < 0)
			iy = (-n + 1)*incy + 1;
		for (i = 1; i <= n; i++)
		{
			Y[iy] += a*X[ix];
			ix += incx;
			iy += incy;
		}
	}
	else if (incx == 1)
	{
		/*        CODE FOR BOTH INCREMENTS EQUAL TO 1
		 *        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 4. */
		m = n%4;
		if (m == 0)
			goto L_40;
		for (i = 1; i <= m; i++)
		{
			Y[i] += a*X[i];
		}
		if (n < 4)
			return;
L_40:
		mp1 = m + 1;
		for (i = mp1; i <= n; i += 4)
		{
			Y[i] += a*X[i];
			Y[i + 1] += a*X[i + 1];
			Y[i + 2] += a*X[i + 2];
			Y[i + 3] += a*X[i + 3];
		}
	}
	else
	{
		/*        CODE FOR EQUAL, POSITIVE, NONUNIT INCREMENTS. */
		ns = n*incx;
		for (i = 1, _do0=DOCNT(i,ns,_do1 = incx); _do0 > 0; i += _do1, _do0--)
		{
			Y[i] += a*X[i];
		}
	}
	return;
} /* end of function */

