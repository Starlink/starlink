/*
	Module:       chisq.c
	Purpose:      compute approximations to chisquare distribution probabilities
	Contents:     pochisq(), critchi()
	Uses:         poz() in z.c (Algorithm 209)
	Programmer:   Gary Perlman
	Organization: Wang Institute, Tyngsboro, MA 01879
	Tester:       compile with -DCHISQTEST to include main program
	Copyright:    none
	Tabstops:     4
*/

/*LINTLIBRARY*/
#include <math.h>
static char sccsfid[] = "@(#) chisq.c 5.2 (|stat) 12/08/86";

#define	CHI_EPSILON     0.000000001    /* accuracy of critchi approximation */
#define	CHI_MAX     99999.0         /* maximum chi square value */

#define	LOG_SQRT_PI     0.5723649429247000870717135 /* log (sqrt (pi)) */
#define	I_SQRT_PI       0.5641895835477562869480795 /* 1 / sqrt (pi) */
#define	BIGX           20.0         /* max value to represent exp (x) */
#define	ex(x)             (((x) < -BIGX) ? 0.0 : exp (x))

double	pochisq ();
double	critchi ();

#ifdef	CHISQTEST
double	Prob[] = { .10, .05, .01, .005, .001, -1.0 };
void main ()
	{
	int 	df;
	int 	p;
	printf ("%-4s ", "df");
	for (p = 0; Prob[p] > 0.0; p++)
		printf ("%8.3f ", Prob[p]);
	putchar ('\n');
	for (df = 1; df < 30; df++)
		{
		printf ("%4d ", df);
		for (p = 0; Prob[p] > 0.0; p++)
			printf ("%8.3f ", critchi (Prob[p], df));
		putchar ('\n');
		}
	}
#endif	CHISQTEST

/*FUNCTION pochisq: probability of chi sqaure value */
/*ALGORITHM Compute probability of chi square value.
	Adapted from:
		Hill, I. D. and Pike, M. C.  Algorithm 299
		Collected Algorithms for the CACM 1967 p. 243
	Updated for rounding errors based on remark in
		ACM TOMS June 1985, page 185
*/
double
pochisq (x, df)
double	x;       /* obtained chi-square value */
int 	df;      /* degrees of freedom */
	{
	double	a, y, s;
	double	e, c, z;
	double	poz ();   /* computes probability of normal z score */
	int 	even;     /* true if df is an even number */

	if (x <= 0.0 || df < 1)
		return (1.0);

	a = 0.5 * x;
	even = (2*(df/2)) == df;
	if (df > 1)
		y = ex (-a);
	s = (even ? y : (2.0 * poz (-sqrt (x))));
	if (df > 2)
		{
		x = 0.5 * (df - 1.0);
		z = (even ? 1.0 : 0.5);
		if (a > BIGX)
			{
			e = (even ? 0.0 : LOG_SQRT_PI);
			c = log (a);
			while (z <= x)
				{
				e = log (z) + e;
				s += ex (c*z-a-e);
				z += 1.0;
				}
			return (s);
			}
		else
			{
			e = (even ? 1.0 : (I_SQRT_PI / sqrt (a)));
			c = 0.0;
			while (z <= x)
				{
				e = e * (a / z);
				c = c + e;
				z += 1.0;
				}
			return (c * y + s);
			}
		}
	else
		return (s);
	}

/*FUNCTION critchi: compute critical chi square value to produce given p */
double
critchi (p, df)
double	p;
int 	df;
	{
	double	minchisq = 0.0;
	double	maxchisq = CHI_MAX;
	double	chisqval;

	if (p <= 0.0)
		return (maxchisq);
	else if (p >= 1.0)
		return (0.0);

	chisqval = df / sqrt (p);    /* fair first value */
	while (maxchisq - minchisq > CHI_EPSILON)
		{
		if (pochisq (chisqval, df) < p)
			maxchisq = chisqval;
		else
			minchisq = chisqval;
		chisqval = (maxchisq + minchisq) * 0.5;
		}
	return (chisqval);
	}
