/*
	Module:       f.c
	Purpose:      compute approximations to F distribution probabilities
	Contents:     pof(), critf()
	Programmer:   Gary Perlman
	Organization: Wang Institute, Tyngsboro, MA 01879
	Tester:       compile with -DFTEST to include main program
	Copyright:    none
	Tabstops:     4
*/

/*LINTLIBRARY*/
#include <math.h>
static char sccsfid[] = "@(#) f.c 5.2 (|stat) 12/26/85";

#ifndef	I_PI        /* 1 / pi */
#define	I_PI        0.3183098861837906715377675
#endif
#define	F_EPSILON     0.000001       /* accuracy of critf approximation */
#define	F_MAX      9999.0            /* maximum F ratio */

double	pof ();
double	critf ();

#ifdef	FTEST

int 	DFs[] = { 1, 2, 5, 10, 20, 40, 60, 120, -1 };
double	Prob[] = { .10, .05, .01, .005, .001, -1.0 };

main ()
	{
	int 	dfnum;
	int 	dfdenom;
	int 	p;

	for (p = 0; Prob[p] > 0.0; p++)
		{
		printf ("alpha = %g                      df1\n", Prob[p]);
		printf ("%-4s\\", "df2");
		for (dfnum = 0; DFs[dfnum] > 0; dfnum++)
			printf ("%7d ", DFs[dfnum]);
		putchar ('\n');
		for (dfdenom = 0; DFs[dfdenom] > 0; dfdenom++)
			{
			printf ("%4d ", DFs[dfdenom]);
			for (dfnum = 0; DFs[dfnum] > 0; dfnum++)
				printf ("%7.2f ", critf (Prob[p], DFs[dfnum], DFs[dfdenom]));
			putchar ('\n');
			}
		putchar ('\n');
		}
	}
#endif	FTEST

/*FUNCTION pof: probability of F */
/*ALGORITHM Compute probability of F ratio.
	Adapted from Collected Algorithms of the CACM
	Algorithm 322
	Egon Dorrer
*/
double
pof (F, df1, df2)
double	F;
int 	df1, df2;
	{
	int	i, j;
	int	a, b;
	double	w, y, z, d, p;

	if (F < F_EPSILON || df1 < 1 || df2 < 1)
		return (1.0);
	a = df1%2 ? 1 : 2;
	b = df2%2 ? 1 : 2;
	w = (F * df1) / df2;
	z = 1.0 / (1.0 + w);
	if (a == 1)
		if (b == 1)
			{
			p = sqrt (w);
			y = I_PI; /* 1 / 3.14159 */
			d = y * z / p;
			p = 2.0 * y * atan (p);
			}
		else
			{
			p = sqrt (w * z);
			d = 0.5 * p * z / w;
			}
	else if (b == 1)
		{
		p = sqrt (z);
		d = 0.5 * z * p;
		p = 1.0 - p;
		}
	else
		{
		d = z * z;
		p = w * z;
		}
	y = 2.0 * w / z;
#ifdef	REMARK /* speedup modification suggested by Tolman (wrong answer!) */
	if (a == 1)
		for (j = b + 2; j <= df2; j += 2)
			{
			d *= (1.0 + a / (j - 2.0)) * z;
			p += d * y / (j - 1.0);
			}
	else
		{
		double	zk = 1.0;
		for (j = (df2 - 1) / 2; j; j--)
			zk *= z;
		d *= zk * df2/b;
		p *= zk + w * z * (zk - 1.0)/(z-1.0);
		}
#else /* original version */
	for (j = b + 2; j <= df2; j += 2)
		{
		d *= (1.0 + a / (j - 2.0)) * z;
		p = (a == 1 ? p + d * y / (j - 1.0) : (p + w) * z);
		}
#endif	REMARK
	y = w * z;
	z = 2.0 / z;
	b = df2 - 2;
	for (i = a + 2; i <= df1; i += 2)
		{
		j = i + b;
		d *= y * j / (i - 2.0);
		p -= z * d / j;
		}
	/* correction for approximation errors suggested in certification */
	if (p < 0.0)
		p = 0.0;
	else if (p > 1.0)
		p = 1.0;
	return (1.0-p);
	}

/*FUNCTION critf: compute critical F value t produce given probability */
/*ALGORITHM
	Begin with upper and lower limits for F values (maxf and minf)
	set to extremes.  Choose an f value (fval) between the extremes.
	Compute the probability of the f value.  Set minf or maxf, based
	on whether the probability is less than or greater than the
	desired p.  Continue adjusting the extremes until they are
	within F_EPSILON of each other.
*/
double
critf (p, df1, df2)
double	p;
int 	df1;
int 	df2;
	{
	double	fval;
	double	fabs ();          /* floating absolute value */
	double	maxf = F_MAX;     /* maximum F ratio */
	double	minf = 0.0;       /* minimum F ratio */

	if (p <= 0.0 || p >= 1.0)
		return (0.0);

	fval = 1.0 / p;             /* the smaller the p, the larger the F */

	while (fabs (maxf - minf) > F_EPSILON)
		{
		if (pof (fval, df1, df2) < p)     /* F too large */
			maxf = fval;
		else                              /* F too small */
			minf = fval;
		fval = (maxf + minf) * 0.5;
		}

	return (fval);
	}
