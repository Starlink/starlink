/*Translated by FOR_C, v3.4.2 (-), on 07/09/115 at 08:30:04 */
/*FOR_C Options SET: ftn=u io=c no=p op=aimnv s=dbov str=l x=f - prototypes */
#include <math.h>
#include "fcrt.h"
#include "pda.h"
#include <stdlib.h>
double /*FUNCTION*/ pdaDdot(
long n,
double x[],
long incx,
double y[],
long incy)
{
	long int _d_l, _d_m, _do0, _do1, i, ix, iy, m, mp1, ns;
	double ddot_v;
		/* OFFSET Vectors w/subscript range: 1 to dimension */
	double *const X = &x[0] - 1;
	double *const Y = &y[0] - 1;
		/* end of OFFSET VECTORS */

	/* Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
	 * ALL RIGHTS RESERVED.
	 * Based on Government Sponsored Research NAS7-03001.
	 *>> 2006-06-07 DDOT   Krogh  Removed arithmetic if
	 *>> 1994-11-11 DDOT   Krogh  Declared all vars.
	 *>> 1994-10-20 DDOT   Krogh  Changes to use M77CON
	 *>> 1994-04-19 DDOT   Krogh  Minor -- Made code versions line up.
	 *>> 1985-08-02 DDOT   Lawson Initial code.
	 *--D replaces "?": ?DOT
	 *
	 *     RETURNS THE DOT PRODUCT OF X AND Y.
	 *     DDOT = SUM FOR I = 0 TO N-1 OF  X(LX+I*INCX) * Y(LY+I*INCY),
	 *     WHERE LX = 1 IF INCX .GE. 0, ELSE LX = (-INCX)*N, AND LY IS
	 *     DEFINED IN A SIMILAR WAY USING INCY.
	 * */
	ddot_v = 0.0e0;
	if (n <= 0)
		return( ddot_v );
	if ((incx != incy) || (incx < 0))
	{
		/*         CODE FOR UNEQUAL OR NONPOSITIVE INCREMENTS. */
		ix = 1;
		iy = 1;
		if (incx < 0)
			ix = (-n + 1)*incx + 1;
		if (incy < 0)
			iy = (-n + 1)*incy + 1;
		for (i = 1; i <= n; i++)
		{
			ddot_v += X[ix]*Y[iy];
			ix += incx;
			iy += incy;
		}
	}
	else if (incx == 1)
	{
		/*        CODE FOR BOTH INCREMENTS EQUAL TO 1.
		 *        CLEAN-UP LOOP SO REMAINING VECTOR LENGTH IS A MULTIPLE OF 5. */
		m = n%5;
		if (m == 0)
			goto L_40;
		for (i = 1; i <= m; i++)
		{
			ddot_v += X[i]*Y[i];
		}
		if (n < 5)
			return( ddot_v );
L_40:
		mp1 = m + 1;
		for (i = mp1; i <= n; i += 5)
		{
			ddot_v += X[i]*Y[i] + X[i + 1]*Y[i + 1] + X[i + 2]*Y[i + 2] +
			 X[i + 3]*Y[i + 3] + X[i + 4]*Y[i + 4];
		}
	}
	else
	{
		/*         CODE FOR POSITIVE EQUAL INCREMENTS .NE.1. */
		ns = n*incx;
		for (i = 1, _do0=DOCNT(i,ns,_do1 = incx); _do0 > 0; i += _do1, _do0--)
		{
			ddot_v += X[i]*Y[i];
		}
	}
	return( ddot_v );
} /* end of function */

