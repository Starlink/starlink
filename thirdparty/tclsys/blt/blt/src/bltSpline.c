#include "bltInt.h"

/*
 * Quadratic spline parameters
 */
#define E1	param[0]
#define E2	param[1]
#define V1	param[2]
#define V2	param[3]
#define W1	param[4]
#define W2	param[5]
#define Z1	param[6]
#define Z2	param[7]
#define Y1	param[8]
#define Y2	param[9]

#ifdef __STDC__
static Tcl_CmdProc SplineCmd;
#endif

static INLINE double
Fabs(x)
    double x;
{
    return ((x < 0.0) ? -x : x);
}

/*
 * -----------------------------------------------------------------------
 *
 * Search --
 *
 *	Conducts a binary search for a value.  This routine is called
 *	only if key is between x(0) and x(len - 1).
 *
 * Results:
 *	Returns the index of the largest value in xtab for which
 *	x[i] < key.
 *
 * -----------------------------------------------------------------------
 */
static int
Search(x, len, key, foundPtr)
    double x[];			/* Contains the abscissas of the data
				 * points of interpolation. */
    int len;			/* Dimension of x. */
    double key;			/* Value whose relative position in
				 * x is to be located. */
    int *foundPtr;		/* (out) Returns 1 if s is found in
				 * x and 0 otherwise. */
{
    int high, low, middle;

    low = 0;
    high = len - 1;

    while (high >= low) {
	middle = (high + low) / 2;
	if (key > x[middle]) {
	    low = middle + 1;
	} else if (key < x[middle]) {
	    high = middle - 1;
	} else {
	    *foundPtr = 1;
	    return (middle);
	}
    }
    *foundPtr = 0;
    return (low);
}

/*
 *-----------------------------------------------------------------------
 *
 * QuadChoose --
 *
 *	Determines the case needed for the computation of the parame-
 *	ters of the quadratic spline.
 *
 * Results:
 * 	Returns a case number (1-4) which controls how the parameters
 * 	of the quadratic spline are evaluated.
 *
 *-----------------------------------------------------------------------
 */
static int
QuadChoose(x1, y1, m1, x2, y2, m2, epsilon)
    double x1, y1;		/* Coordinates of one of the points of
				 * interpolation */
    double m1;			/* Derivative condition at point x1, y1 */
    double x2, y2;		/* Coordinates of one of the points of
				 * interpolation */
    double m2;			/* Derivative condition at point x2, y2 */
    double epsilon;		/* Error tolerance used to distinguish
				 * cases when m1 or m2 is relatively
				 * close to the slope or twice the
				 * slope of the line segment joining
				 * the points x1, y1 and x2, y2.  If
				 * epsilon is not 0.0, then epsilon
				 * should be greater than or equal to
				 * machine epsilon.  */
{
    double slope;

    /* Calculate the slope of the line joining x1,y1 and x2,y2. */
    slope = (y2 - y1) / (x2 - x1);

    if (slope != 0.0) {
	double relerr;
	double mref, mref1, mref2, prod1, prod2;

	prod1 = slope * m1;
	prod2 = slope * m2;

	/* Find the absolute values of the slopes slope, m1, and m2. */
	mref = Fabs(slope);
	mref1 = Fabs(m1);
	mref2 = Fabs(m2);

	/*
	 * If the relative deviation of m1 or m2 from slope is less than
	 * epsilon, then choose case 2 or case 3.
	 */
	relerr = epsilon * mref;
	if ((Fabs(slope - m1) > relerr) && (Fabs(slope - m2) > relerr) &&
	    (prod1 >= 0.0) && (prod2 >= 0.0)) {
	    double prod;

	    prod = (mref - mref1) * (mref - mref2);
	    if (prod < 0.0) {
		/*
		 * l1, the line through (x1,y1) with slope m1, and l2,
		 * the line through (x2,y2) with slope m2, intersect
		 * at a point whose abscissa is between x1 and x2.
		 * The abscissa becomes a knot of the spline.
		 */
		return 1;
	    }
	    if (mref1 > (mref * 2.0)) {
		if (mref2 <= ((2.0 - epsilon) * mref)) {
		    return 3;
		}
	    } else if (mref2 <= (mref * 2.0)) {
		/*
		 * Both l1 and l2 cross the line through (x1+x2)/2.0,y1
		 * and (x1+x2)/2.0,y2, which is the midline of the
		 * rectangle formed by (x1,y1),(x2,y1), (x2,y2), and
		 * (x1,y2), or both m1 and m2 have signs different
		 * than the sign of slope, or one of m1 and m2 has
		 * opposite sign from slope and l1 and l2 intersect to
		 * the left of x1 or to the right of x2.  The point
		 * (x1+x2)/2. is a knot of the spline.
		 */
		return 2;
	    } else if (mref1 <= ((2.0 - epsilon) * mref)) {
		/*
		 * In cases 3 and 4, sign(m1)=sign(m2)=sign(slope).
		 * Either l1 or l2 crosses the midline, but not both.
		 * Choose case 4 if mref1 is greater than
		 * (2.-epsilon)*mref; otherwise, choose case 3.
		 */
		return 3;
	    }
	    /*
	     * If neither l1 nor l2 crosses the midline, the spline
	     * requires two knots between x1 and x2.
	     */
	    return 4;
	} else {
	    /*
	     * The sign of at least one of the slopes m1 or m2 does not
	     * agree with the sign of *slope*.
	     */
	    if ((prod1 < 0.0) && (prod2 < 0.0)) {
		return 2;
	    } else if (prod1 < 0.0) {
		if (mref2 > ((epsilon + 1.0) * mref)) {
		    return 1;
		} else {
		    return 2;
		}
	    } else if (mref1 > ((epsilon + 1.0) * mref)) {
		return 1;
	    } else {
		return 2;
	    }
	}
    } else if ((m1 * m2) >= 0.0) {
	return 2;
    } else {
	return 1;
    }
}

/*
 * -----------------------------------------------------------------------
 *
 * QuadCases --
 *
 *	Computes the knots and other parameters of the spline on the
 *	interval (p1,q1).
 *
 *
 * On input--
 *
 *	(p1,y1) and (q1,q2) are the coordinates of the points of
 *	interpolation.
 *
 *	m1 is the slope at (p1,p2).
 *
 *	m2 is the slope at (q1,q2)
 *
 *	ncase controls the number and location of the knots.
 *
 *
 * On output--
 *
 *	(v1,v2),(w1,w2),(z1,z2), and (e1,e2) are the coordinates of
 *	the knots and other parameters of the spline on (p1,q1).
 *	(e1,e2) and (y1,y2) are used only if ncase=4.
 *
 * -----------------------------------------------------------------------
 */
static void
QuadCases(p1, p2, m1, m2, q1, q2, param, which)
    double p1, p2, m1, m2, q1, q2;
    double param[];
    int which;
{
    if ((which == 3) || (which == 4)) {	/* Parameters used in both 3 and 4 */
	double mbar1, mbar2, mbar3, c1, d1, h1, j1, k1;

	c1 = p1 + (q2 - p2) / m1;
	d1 = q1 + (p2 - q2) / m2;
	h1 = c1 * 2.0 - p1;
	j1 = d1 * 2.0 - q1;
	mbar1 = (q2 - p2) / (h1 - p1);
	mbar2 = (p2 - q2) / (j1 - q1);

	if (which == 4) {	/* Case 4. */
	    Y1 = (p1 + c1) / 2.0;
	    V1 = (p1 + Y1) / 2.0;
	    V2 = m1 * (V1 - p1) + p2;
	    Z1 = (d1 + q1) / 2.0;
	    W1 = (q1 + Z1) / 2.0;
	    W2 = m2 * (W1 - q1) + q2;
	    mbar3 = (W2 - V2) / (W1 - V1);
	    Y2 = mbar3 * (Y1 - V1) + V2;
	    Z2 = mbar3 * (Z1 - V1) + V2;
	    E1 = (Y1 + Z1) / 2.0;
	    E2 = mbar3 * (E1 - V1) + V2;
	} else {		/* Case 3. */
	    k1 = (p2 - q2 + q1 * mbar2 - p1 * mbar1) / (mbar2 - mbar1);
	    if (Fabs(m1) > Fabs(m2)) {
		Z1 = (k1 + p1) / 2.0;
	    } else {
		Z1 = (k1 + q1) / 2.0;
	    }
	    V1 = (p1 + Z1) / 2.0;
	    V2 = p2 + m1 * (V1 - p1);
	    W1 = (q1 + Z1) / 2.0;
	    W2 = q2 + m2 * (W1 - q1);
	    Z2 = V2 + (W2 - V2) / (W1 - V1) * (Z1 - V1);
	}
    } else if (which == 2) {	/* Case 2. */
	Z1 = (p1 + q1) / 2.0;
	V1 = (p1 + Z1) / 2.0;
	V2 = p2 + m1 * (V1 - p1);
	W1 = (Z1 + q1) / 2.0;
	W2 = q2 + m2 * (W1 - q1);
	Z2 = (V2 + W2) / 2.0;
    } else {			/* Case 1. */
	double ztwo;

	Z1 = (p2 - q2 + m2 * q1 - m1 * p1) / (m2 - m1);
	ztwo = p2 + m1 * (Z1 - p1);
	V1 = (p1 + Z1) / 2.0;
	V2 = (p2 + ztwo) / 2.0;
	W1 = (Z1 + q1) / 2.0;
	W2 = (ztwo + q2) / 2.0;
	Z2 = V2 + (W2 - V2) / (W1 - V1) * (Z1 - V1);
    }
}

static int
QuadSelect(x1, y1, x2, y2, m1, m2, epsilon, param)
    double x1, y1, x2, y2;
    double m1, m2;
    double epsilon;
    double param[];
{
    int ncase;

    ncase = QuadChoose(x1, y1, m1, x2, y2, m2, epsilon);
    QuadCases(x1, y1, m1, m2, x2, y2, param, ncase);
    return (ncase);
}

/*
 * -----------------------------------------------------------------------
 *
 * QuadGetImage --
 *
 * -----------------------------------------------------------------------
 */
INLINE static double
QuadGetImage(p1, p2, p3, x1, x2, x3)
    double p1, p2, p3;
    double x1, x2, x3;
{
    double A, B, C;
    double y;

    A = x1 - x2;
    B = x2 - x3;
    C = x1 - x3;

    y = (p1 * (A * A) + p2 * 2.0 * B * A + p3 * (B * B)) / (C * C);
    return (y);
}

/*
 * -----------------------------------------------------------------------
 *
 * QuadSpline --
 *
 *	Finds the image of a point in x.
 *
 *	On input
 *
 *	x	Contains the value at which the spline is evaluated.
 *	leftX, leftY
 *		Coordinates of the left-hand data point used in the
 *		evaluation of xvals.
 *	rightX, rightY
 *		Coordinates of the right-hand data point used in the
 *		evaluation of xvals.
 *	Z1, Z2, Y1, Y2, E2, W2, V2
 *		Parameters of the spline.
 *	ncase	Controls the evaluation of the spline by indicating
 *		whether one or two knots were placed in the interval
 *		(xtabs,xtabs1).
 *
 * Results:
 *	The image of the spline at x.
 *
 * -----------------------------------------------------------------------
 */
static double
QuadSpline(x, leftX, leftY, rightX, rightY, param, ncase)
    double x;			/* Value at which spline is evaluated */
    double leftX, leftY;	/* Point to the left of the data point to
				 * be evaluated */
    double rightX, rightY;	/* Point to the right of the data point to
				 * be evaluated */
    double param[];		/* Parameters of the spline */
    int ncase;			/* Controls the evaluation of the
				 * spline by indicating whether one or
				 * two knots were placed in the
				 * interval (leftX,rightX) */
{
    if (ncase == 4) {
	/*
	 * Case 4:  More than one knot was placed in the interval.
	 */

	/*
	 * Determine the location of data point relative to the 1st knot.
	 */
	if (Y1 > x) {
	    return QuadGetImage(leftY, V2, Y2, Y1, x, leftX);
	} else if (Y1 < x) {
	    /*
	     * Determine the location of the data point relative to
	     * the 2nd knot.
	     */
	    if (Z1 > x) {
		return QuadGetImage(Y2, E2, Z2, Z1, x, Y1);
	    } else if (Z1 < x) {
		return QuadGetImage(Z2, W2, rightY, rightX, x, Z1);
	    } else {
		return (Z2);
	    }
	} else {
	    return (Y2);
	}
    } else {

	/*
	 * Cases 1, 2, or 3:
	 *
	 * Determine the location of the data point relative to the
	 * knot.
	 */
	if (Z1 < x) {
	    return QuadGetImage(Z2, W2, rightY, rightX, x, Z1);
	} else if (Z1 > x) {
	    return QuadGetImage(leftY, V2, Z2, Z1, x, leftX);
	} else {
	    return (Z2);
	}
    }
}

/*
 * -----------------------------------------------------------------------
 *
 * QuadSlopes --
 *
 * 	Calculates the derivative at each of the data points.  The
 * 	slopes computed will insure that an osculatory quadratic
 * 	spline will have one additional knot between two adjacent
 * 	points of interpolation.  Convexity and monotonicity are
 * 	preserved wherever these conditions are compatible with the
 * 	data.
 *
 * Results:
 *	The output array "m" is filled with the derivates at each
 *	data point.
 *
 * -----------------------------------------------------------------------
 */
static void
QuadSlopes(x, y, m, len)
    double x[];			/* Abscissas of the data points. */
    double y[];			/* Ordinates of the data points. */
    double m[];			/* (out) To be filled with the first
				 * derivative at each data point. */
    int len;			/* Number of data points (dimension of
				 * x, y, and m). */
{
    double xbar, xmid, xhat, ydif1, ydif2;
    double yxmid;
    double m1, m2;
    double m1s, m2s;
    register int i, next, last;

    m1s = m2s = m1 = m2 = 0;
    for (i = 1, next = 2, last = 0; i < (len - 1); i++, next++, last++) {
	/*
	 * Calculate the slopes of the two lines joining three
	 * consecutive data points.
	 */
	ydif1 = y[i] - y[last];
	ydif2 = y[next] - y[i];
	m1 = ydif1 / (x[i] - x[last]);
	m2 = ydif2 / (x[next] - x[i]);
	if (i == 1) {
	    m1s = m1, m2s = m2;	/* Save slopes of starting point */
	}
	/*
	 * If one of the preceding slopes is zero or if they have opposite
	 * sign, assign the value zero to the derivative at the middle
	 * point.
	 */
	if ((m1 == 0.0) || (m2 == 0.0) || ((m1 * m2) <= 0.0)) {
	    m[i] = 0.0;
	} else if (Fabs(m1) > Fabs(m2)) {
	    /*
	     * Calculate the slope by extending the line with slope m1.
	     */
	    xbar = ydif2 / m1 + x[i];
	    xhat = (xbar + x[next]) / 2.0;
	    m[i] = ydif2 / (xhat - x[i]);
	} else {
	    /*
	     * Calculate the slope by extending the line with slope m2.
	     */
	    xbar = -ydif1 / m2 + x[i];
	    xhat = (x[last] + xbar) / 2.0;
	    m[i] = ydif1 / (x[i] - xhat);
	}
    }

    /* Calculate the slope at the last point, x(n). */
    i = len - 2;
    next = len - 1;
    if ((m1 * m2) < 0.0) {
	m[next] = m2 * 2.0;
    } else {
	xmid = (x[i] + x[next]) / 2.0;
	yxmid = m[i] * (xmid - x[i]) + y[i];
	m[next] = (y[next] - yxmid) / (x[next] - xmid);
	if ((m[next] * m2) < 0.0) {
	    m[next] = 0.0;
	}
    }

    /* Calculate the slope at the first point, x(0). */
    if ((m1s * m2s) < 0.0) {
	m[0] = m1s * 2.0;
    } else {
	xmid = (x[0] + x[1]) / 2.0;
	yxmid = m[1] * (xmid - x[1]) + y[1];
	m[0] = (yxmid - y[0]) / (xmid - x[0]);
	if ((m[0] * m1s) < 0.0) {
	    m[0] = 0.0;
	}
    }

}

/*
 * -----------------------------------------------------------------------
 *
 * QuadEval --
 *
 * 	QuadEval controls the evaluation of an osculatory quadratic
 * 	spline.  The user may provide his own slopes at the points of
 * 	interpolation or use the subroutine 'QuadSlopes' to calculate
 * 	slopes which are consistent with the shape of the data.
 *
 * ON INPUT--
 *   	splX 	must be a nondecreasing vector of points at which the
 *		spline will be evaluated.
 *   	x 	contains the abscissas of the data points to be
 *		interpolated. xtab must be increasing.
 *   	y	contains the ordinates of the data points to be
 *		interpolated.
 *   	m 	contains the slope of the spline at each point of
 *		interpolation.
 *   	len	number of data points (dimension of xtab and y).
 *   	numEval is the number of points of evaluation (dimension of
 *		xval and yval).
 *   	epsilon 	is a relative error tolerance used in subroutine
 *		'QuadChoose' to distinguish the situation m(i) or
 *		m(i+1) is relatively close to the slope or twice
 *		the slope of the linear segment between xtab(i) and
 *		xtab(i+1).  If this situation occurs, roundoff may
 *		cause a change in convexity or monotonicity of the
 *   		resulting spline and a change in the case number
 *		provided by 'QuadChoose'.  If epsilon is not equal to zero,
 *		then epsilon should be greater than or equal to machine
 *		epsilon.
 * ON OUTPUT--
 * 	splY 	contains the images of the points in xval.
 *   	err 	is one of the following error codes:
 *      	0 - QuadEval ran normally.
 *      	1 - xval(i) is less than xtab(1) for at least one
 *		    i or xval(i) is greater than xtab(num) for at
 *		    least one i. QuadEval will extrapolate to provide
 *		    function values for these abscissas.
 *      	2 - xval(i+1) < xval(i) for some i.
 *
 *
 *  QuadEval calls the following subroutines or functions:
 *      Search
 *      QuadCases
 *      QuadChoose
 *      QuadSpline
 * -----------------------------------------------------------------------
 */
static int
QuadEval(splX, splY, x, y, m, len, splLen, epsilon)
    double splX[];		/* Must be a nondecreasing vector of
				 * points at which the spline will be
				 * evaluated. */
    double splY[];		/* (out) To be filled with the images
				 * of the points in splX. */
    double x[];			/* Abscissas of the data points to
				 * be interpolated. X must be increasing. */
    double y[];			/* Ordinates of the data points to be
				 * interpolated. */
    double m[];			/* Slope of the spline at each point
				 * of interpolation. */
    int len;			/* Number of data points (dimension of
				 * X and y). */
    int splLen;			/* Number of points of evaluation
				 * (length of vectors of splX and splY). */
    double epsilon;		/* Relative error tolerance (see choose) */
{
    int error;
    register int i;
    double param[10];
    int ncase;
    int splLast;
    int start, last, prev;

    /* Initialize indices and set error result */
    start = 0;
    splLast = splLen - 1;
    error = 0;
    last = len - 1;
    prev = last - 1;
    ncase = 1;

    /*
     * Determine if abscissas of new vector are non-decreasing.
     */
    for (i = 1; i < splLen; i++) {
	if (splX[i] < splX[i - 1]) {
	    return 2;
	}
    }
    /*
     * Determine if any of the points in splX are LESS than the
     * abscissa of the first data point.
     */
    for (i = 0; i < splLen; i++) {
	if (splX[i] >= x[0]) {
	    break;
	}
	start = i + 1;
    }
    /*
     * Determine if any of the points in splX are GREATER than the
     * abscissa of the last data point.
     */
    for (i = splLen - 1; i >= 0; i--) {
	if (splX[i] <= x[last]) {
	    break;
	}
	splLast = i - 1;
    }

    if (start > 0) {
	error = 1;		/* Set error value to indicate that
				 * extrapolation has occurred. */
	/*
	 * Calculate the images of points of evaluation whose abscissas
	 * are less than the abscissa of the first data point.
	 */
	ncase = QuadSelect(x[0], y[0], x[1], y[1], m[0], m[1], epsilon, param);
	for (i = 0; i < (start - 1); i++) {
	    splY[i] = QuadSpline(splX[i], x[0], y[0], x[1], y[1], param, ncase);
	}
	if (splLen == 1) {
	    return (error);
	}
    }
    if ((splLen > 1) || (splLast == (splLen - 1))) {
	register int next, loc;
	int found;

	/*
	 * Search locates the interval in which the first in-range
	 * point of evaluation lies.
	 */
	loc = Search(x, len, splX[start], &found);

	next = loc + 1;

	/*
	 * If the first in-range point of evaluation is equal to one
	 * of the data points, assign the appropriate value from y.
	 * Continue until a point of evaluation is found which is not
	 * equal to a data point.
	 */
	if (found) {
	    do {
		splY[start] = y[loc];
		start++;
		if (start >= splLen) {
		    return (error);
		}
	    } while (splX[start - 1] == splX[start]);

	    for (;;) {
		if (splX[start] < x[next]) {
		    break;	/* Break out of for-loop */
		}
		if (splX[start] == x[next]) {
		    do {
			splY[start] = y[next];
			start++;
			if (start >= splLen) {
			    return (error);
			}
		    } while (splX[start] == splX[start - 1]);
		}
		loc++;
		next++;
	    }
	}
	/*
	 * Calculate the images of all the points which lie within
	 * range of the data.
	 */
	if ((loc > 0) || (error != 1)) {
	    ncase = QuadSelect(x[loc], y[loc], x[next], y[next], m[loc],
		m[next], epsilon, param);
	}
	for (i = start; i <= splLast; i++) {
	    /*
	     * If splX(i) - x(next) is negative, do not recalculate
	     * the parameters for this section of the spline since
	     * they are already known.
	     */
	    if (splX[i] == x[next]) {
		splY[i] = y[next];
		continue;
	    } else if (splX[i] > x[next]) {
		double delta;

		/* Determine that the routine is in the correct part of
		   the spline. */
		do {
		    loc++, next++;
		    delta = splX[i] - x[next];
		} while (delta > 0.0);

		if (delta < 0.0) {
		    ncase = QuadSelect(x[loc], y[loc], x[next], y[next], m[loc],
			m[next], epsilon, param);
		} else if (delta == 0.0) {
		    splY[i] = y[next];
		    continue;
		}
	    }
	    splY[i] = QuadSpline(splX[i], x[loc], y[loc], x[next], y[next],
		param, ncase);
	}

	if (splLast == (splLen - 1)) {
	    return (error);
	}
	if ((next == last) && (splX[splLast] != x[last])) {
	    goto noExtrapolation;
	}
    }
    error = 1;			/* Set error value to indicate that
				 * extrapolation has occurred. */
    ncase = QuadSelect(x[prev], y[prev], x[last], y[last], m[prev], m[last],
	epsilon, param);

  noExtrapolation:
    /*
     * Calculate the images of the points of evaluation whose
     * abscissas are greater than the abscissa of the last data point.
     */
    for (i = (splLast + 1); i < splLen; i++) {
	splY[i] = QuadSpline(splX[i], x[prev], y[prev], x[last], y[last],
	    param, ncase);
    }
    return (error);
}

/*
 * -----------------------------------------------------------------------
 *
 *		  Shape preserving quadratic splines
 *		   by D.F.Mcallister & J.A.Roulier
 *		    Coded by S.L.Dodd & M.Roulier
 *			 N.C.State University
 *
 * -----------------------------------------------------------------------
 */
/*
 * Driver routine for quadratic spline package
 * On input--
 *   X,Y    Contain n-long arrays of data (x is increasing)
 *   XM     Contains m-long array of x values (increasing)
 *   eps    Relative error tolerance
 *   n      Number of input data points
 *   m      Number of output data points
 * On output--
 *   work   Contains the value of the first derivative at each data point
 *   ym     Contains the interpolated spline value at each data point
 */

static int
QuadraticSpline(x, y, len, splX, splY, splLen, work, epsilon)
    double x[], y[];
    int len;
    double splX[], splY[];
    int splLen;
    double work[];
    double epsilon;
{
    QuadSlopes(x, y, work, len);
    return QuadEval(splX, splY, x, y, work, len, splLen, epsilon);
}

/*
 * ------------------------------------------------------------------------
 *
 * Reference:
 *	Numerical Analysis, R. Burden, J. Faires and A. Reynolds.
 *	Prindle, Weber & Schmidt 1981 pp 112
 *
 * Parameters:
 *	x    - vector of points, assumed to be sorted.
 *	y    - vector of corresponding function values.
 *	splX - vector of new points.
 *	splY - vector of new function values.
 *
 * ------------------------------------------------------------------------
 */
static int
NaturalSpline(x, y, len, splX, splY, splLen, work)
    double x[];			/* Vector of points in ascending order */
    double y[];			/* Vector of function values f(x)  */
    int len;
    double splX[];		/* New mapping of points  */
    double splY[];		/* (out) Function values f(nx) */
    int splLen;
    double work[];		/* Working storage */
{
    int end;
    int loc, found;
    register int i, j, n;
    double *h;			/* vector of deltas in x */
    double *alpha;
    double *l, *mu, *z, *a, *b, *c, *d, v;

    end = len - 1;

    a = work;
    b = a + len;
    c = b + len;
    d = c + len;
    h = d + len;
    l = h + len;
    z = l + len;
    mu = z + len;
    alpha = mu + len;

    for (i = 0; i < len; i++) {
	a[i] = y[i];
    }

    /* Calculate vector of differences */
    for (i = 0; i < end; i++) {
	h[i] = x[i + 1] - x[i];
	if (h[i] < 0.0) {
	    return -1;
	}
    }

    /* Calculate alpha vector */
    for (n = 0, i = 1; i < end; i++, n++) {
	/* n = i - 1 */
	alpha[i] = 3.0 * ((a[i + 1] / h[i]) - (a[i] / h[n]) - (a[i] / h[i]) +
	    (a[n] / h[n]));
    }

    /* Vectors to solve the tridiagonal matrix */
    l[0] = l[end] = 1.0;
    mu[0] = mu[end] = 0.0;
    z[0] = z[end] = 0.0;
    c[0] = c[end] = 0.0;

    /* Calculate the intermediate results */
    for (n = 0, i = 1; i < end; i++, n++) {
	/* n = i - 1 */
	l[i] = 2 * (h[i] + h[n]) - h[n] * mu[n];
	mu[i] = h[i] / l[i];
	z[i] = (alpha[i] - h[n] * z[n]) / l[i];
    }
    for (n = end, j = end - 1; j >= 0; j--, n--) {
	/* n = j + 1 */
	c[j] = z[j] - mu[j] * c[n];
	b[j] = (a[n] - a[j]) / h[j] - h[j] * (c[n] + 2.0 * c[j]) / 3.0;
	d[j] = (c[n] - c[j]) / (3.0 * h[j]);
    }

    /* Now calculate the new values */
    for (j = 0; j < splLen; j++) {
	v = splX[j];
	splY[j] = 0.0;

	/* Is it outside the interval? */
	if ((v < x[0]) || (v > x[end])) {
	    continue;
	}
	/* Search for the interval containing v in the x vector */
	loc = Search(x, len, v, &found);
	if (found) {
	    splY[j] = y[loc];
	} else {
	    loc--;
	    v -= x[loc];
	    splY[j] = a[loc] + v * (b[loc] + v * (c[loc] + v * d[loc]));
	}
    }
    return 0;
}

int
Blt_NaturalSpline(x, y, length, splX, splY, splLen)
    double x[], y[];
    int length;
    double splX[], splY[];
    int splLen;
{
    double *work;
    int result;

    work = (double *)malloc(sizeof(double) * length * 9);
    assert(work);
    result = NaturalSpline(x, y, length, splX, splY, splLen, work);
    free((char *)work);
    return (result);
}

int
Blt_QuadraticSpline(x, y, length, splX, splY, splLen, epsilon)
    double x[], y[];
    int length;
    double splX[], splY[];
    int splLen;
    double epsilon;
{
    double *work;
    int result;

    /* allocate space for vectors used in calculation */
    work = (double *)malloc(length * sizeof(double));
    assert(work);
    result = QuadraticSpline(x, y, length, splX, splY, splLen, work, epsilon);
    free((char *)work);
    return (result);
}

/*ARGSUSED*/
static int
NaturalOp(tkwin, interp, x, y, splX, splY, argc, argv)
    Tk_Window tkwin;		/* Main window of the interpreter. Used to
				 * process options using Tk_ConfigureWidget */
    Tcl_Interp *interp;
    Blt_Vector *x, *y, *splX, *splY;
    int argc;			/* Not used */
    char **argv;		/* Not used */
{
    if (Blt_NaturalSpline(Blt_VecData(x), Blt_VecData(y), Blt_VecLength(x),
	    Blt_VecData(splX), Blt_VecData(splY), Blt_VecLength(splX)) != 0) {
	Tcl_AppendResult(interp, "x vector \"", argv[2],
	    "\" must be sorted in ascending order", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}


typedef struct {
    double epsilon;		/* Error setting for calculating the spline */
} QuadInfo;

#define DEF_QUAD_ERROR	"0.0"

static Tk_ConfigSpec quadConfigSpecs[] =
{
    {TK_CONFIG_DOUBLE, "-error", (char *)NULL, (char *)NULL,
	DEF_QUAD_ERROR, Tk_Offset(QuadInfo, epsilon),
	TK_CONFIG_DONT_SET_DEFAULT},
    {TK_CONFIG_END, (char *)NULL, (char *)NULL, (char *)NULL,
	(char *)NULL, 0, 0}
};

/* ARGSUSED */
static int
QuadraticOp(tkwin, interp, x, y, splX, splY, argc, argv)
    Tk_Window tkwin;		/* Main window of the interpreter. Used to
				 * process options using Tk_ConfigureWidget */
    Tcl_Interp *interp;
    Blt_Vector *x, *y, *splX, *splY;
    int argc;			/* Not used */
    char **argv;
{
    double epsilon;
    int result;

    epsilon = 0.0;		/* TBA: adjust error via command-line option */
    if (argc > 6) {
	QuadInfo info;

	info.epsilon = 0.0;
	if (Tk_ConfigureWidget(interp, tkwin, quadConfigSpecs, argc - 6,
		argv + 6, (char *)&info, 0) != TCL_OK) {
	    return TCL_ERROR;
	}
#ifdef notdef
	if (info.epsilon < 0.0) {
	    info.epsilon = 0.0;
	}
#endif
	epsilon = info.epsilon;
    }
    result = Blt_QuadraticSpline(Blt_VecData(x), Blt_VecData(y),
	Blt_VecLength(x), Blt_VecData(splX), Blt_VecData(splY),
	Blt_VecLength(splX), epsilon);
    if (result != 0) {
	Tcl_AppendResult(interp, "error generating spline for \"", argv[2],
	    "\"", (char *)NULL);
	return TCL_ERROR;
    }
    return TCL_OK;
}

static Blt_OpSpec operSpecs[] =
{
    {"natural", 1, (Blt_Operation)NaturalOp, 6, 0,
	"x y splx sply ?option value?...",},
    {"quadratic", 1, (Blt_Operation)QuadraticOp, 6, 0,
	"x y splx sply ?option value?...",},
};
static int numSpecs = sizeof(operSpecs) / sizeof(Blt_OpSpec);

static int
SplineCmd(clientData, interp, argc, argv)
    ClientData clientData;
    Tcl_Interp *interp;
    int argc;
    char **argv;
{
    Tk_Window tkwin = (Tk_Window)clientData;
    Blt_Operation proc;
    Blt_Vector *x, *y, *splX, *splY;
    register int i;
    int result;

    proc = Blt_GetOperation(interp, numSpecs, operSpecs, BLT_OPER_ARG1,
	argc, argv);
    if (proc == NULL) {
	return TCL_ERROR;
    }
    if ((Blt_GetVector(interp, argv[2], &x) != TCL_OK) ||
	(Blt_GetVector(interp, argv[3], &y) != TCL_OK) ||
	(Blt_GetVector(interp, argv[4], &splX) != TCL_OK)) {
	return TCL_ERROR;
    }
    if (Blt_VecLength(x) < 3) {
	Tcl_AppendResult(interp, "length of vector \"", argv[2], "\" is < 3",
	    (char *)NULL);
	return TCL_ERROR;
    }
    for (i = 1; i < Blt_VecLength(x); i++) {
	if (Blt_VecData(x)[i] <= Blt_VecData(x)[i - 1]) {
	    Tcl_AppendResult(interp, "x vector \"", argv[2],
		"\" must be monotonically increasing", (char *)NULL);
	    return TCL_ERROR;
	}
    }
    if (Blt_VecLength(x) != Blt_VecLength(y)) {
	Tcl_AppendResult(interp, "vectors \"", argv[2], "\" and \"", argv[3],
	    " have different lengths", (char *)NULL);
	return TCL_ERROR;
    }
    if (Blt_GetVector(interp, argv[5], &splY) != TCL_OK) {
	/*
	 * If the named vector to hold the ordinates of the spline
	 * doesn't exist, create one the same size as the vector
	 * containing the abscissas.
	 */
	if (Blt_CreateVector(interp, argv[5], Blt_VecLength(splX),
		&splY) != TCL_OK) {
	    return TCL_ERROR;
	}
    } else if (Blt_VecLength(splX) != Blt_VecLength(splY)) {
	/*
	 * The x and y vectors differ in size. Make the number of ordinates
	 * the same as the number of abscissas.
	 */
	if (Blt_ResizeVector(splY, Blt_VecLength(splX)) != TCL_OK) {
	    return TCL_ERROR;
	}
    }
    result = (*proc) (tkwin, interp, x, y, splX, splY, argc, argv);
    if (result != TCL_OK) {
	return TCL_ERROR;
    }
    /*
     * Update the vector. In this case, we're merely notifying the
     * vector management routines that the values have changed (the
     * memory is still the same).  The vector does not need to be
     * reallocated (TCL_STATIC is ignored).
     */
    if (Blt_ResetVector(splY, Blt_VecData(splY), Blt_VecLength(splY),
	    Blt_VecSize(splY), TCL_STATIC) != TCL_OK) {
	return TCL_ERROR;
    }
    return TCL_OK;
}

int
Blt_SplineInit(interp)
    Tcl_Interp *interp;
{
    static Blt_CmdSpec cmdSpec =
    {"spline", SplineCmd,};

    if (Blt_InitCmd(interp, "blt", &cmdSpec) == NULL) {
	return TCL_ERROR;
    }
    return TCL_OK;
}
