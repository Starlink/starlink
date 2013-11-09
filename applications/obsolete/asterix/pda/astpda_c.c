/*
 *+
 * Description:
 *   Calling functions for Asterix PDA replacements.
 *   Designed to be as close to NAG calls as possible
 *   and wrapper to insulate the user from the actual
 *   algorithm called.

 * Authors:
 *   RB: Richard Beard (ROSAT, University of Birmingham)

 * History:
 *   20 Jun 1997: RB
 *     Original version
 *-
 */

#include "f77.h"

double pochisq(double x, int df);
double critchi(double p, int df);
int is_inf(double x);

/*
 *+
 * Description:
 *   Chi-squared distribution (integer degrees fo freedom only)
 *   Replacement for G01ECF
 *-
 */

F77_DOUBLE_FUNCTION(pda_chisqd)(CHARACTER(tail), DOUBLE(x), DOUBLE(df), INTEGER(ifail) TRAIL(tail)) {
	GENPTR_CHARACTER(tail)
	GENPTR_DOUBLE(x)
	GENPTR_DOUBLE(df)
	GENPTR_INTEGER(ifail)

	int ndf;

	ndf = (int) (*df + 0.5);

	if (tail[0] != 'L' && tail[0] != 'l' && tail[0] != 'U' && tail[0] != 'u') {
		*ifail = 1;
		return 0.0;
	} else if (*x < 0.0) {
		*ifail = 2;
		return 0.0;
	} else if (ndf <= 0) {
		*ifail = 3;
		return 0.0;
	} else {
		*ifail = 0;
		if (tail[0] == 'U' || tail[0] == 'u')
			return pochisq(*x, ndf);
		else
			return (1.0 - pochisq(*x, ndf));
	}
}

/*
 *+
 * Description:
 *   Deviate from lower-tail probability of chi-squared distribution (integer degrees fo freedom only)
 *   Replacement for G01FCF
 *-
 */

F77_DOUBLE_FUNCTION(pda_chidev)(DOUBLE(p), DOUBLE(df), INTEGER(ifail)) {
	GENPTR_DOUBLE(p)
	GENPTR_DOUBLE(df)
	GENPTR_INTEGER(ifail)

	int ndf;

	ndf = (int) (*df + 0.5);
	*p = 1.0 - *p;

	if (*p < 0.0 || *p >= 1.0) {
		*ifail = 1;
		return 0.0;
	} else if (ndf <= 0) {
		*ifail = 2;
		return 0.0;
	} else {
		*ifail = 0;
		return critchi(*p, ndf);
	}
}

/*
 *+
 * Description:
 *   Check the value of a number.
 *   Retruns 1 if +Inf or -Inf, 0 otherwise
 *-
 */

F77_LOGICAL_FUNCTION(is_inf)(DOUBLE(x)) {
	GENPTR_DOUBLE(x)

	return !finite(*x);
}
