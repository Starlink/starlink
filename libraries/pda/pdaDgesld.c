/*Translated by FOR_C, v3.4.2 (-), on 07/09/115 at 08:31:15 */
/*FOR_C Options SET: ftn=u io=c no=p op=aimnv s=dbov str=l x=f - prototypes */
#include <math.h>
#include "fcrt.h"
#include "pda.h"
#include <stdlib.h>
		/* PARAMETER translations */
#define	ZERO	0.e0
		/* end of PARAMETER translations */

void /*FUNCTION*/ pdaDgesld(
double *a,
long lda,
long n,
long ipvt[],
double b[])
{
#define A(I_,J_)	(*(a+(I_)*(lda)+(J_)))
	long int k, kb, l, nm1;
	double t;
		/* OFFSET Vectors w/subscript range: 1 to dimension */
	double *const B = &b[0] - 1;
	long *const Ipvt = &ipvt[0] - 1;
		/* end of OFFSET VECTORS */

	/* Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
	 * ALL RIGHTS RESERVED.
	 * Based on Government Sponsored Research NAS7-03001.
	 *>> 1994-10-20 DGESLD Krogh  Changes to use M77CON
	 *>> 1987-08-18 DGESLD Lawson  Initial code.
	 *--D replaces "?": ?GESLD, ?AXPY */
	/*     This subroutine solves the system of equations  A * X = B
	 *     using the LU factorization of A given in the array A().
	 *     ------------------------------------------------------------------
	 *           Subroutine arguments
	 *
	 *     A(,)  [in]  An array of size at least N x N.  On entry must
	 *         contain the LU factors of an N x N  matrix, A.  It is
	 *         expected that this factorization will have been computed by
	 *         use of _GEFA, either directly or indirectly via use of
	 *         _GEFS or _GEFSC.  This subr will not alter the contents of
	 *         A(,)
	 *
	 *     LDA  [in]  Leading dimensioning parameter for the array A(,).
	 *
	 *     N  [in]  The order of the original matrix, A.
	 *
	 *     IPVT()  [in]  An integer array of length at least N, containg a
	 *           record of the row interchanges made during factorization of
	 *           A.
	 *
	 *     B()  [inout]  On entry contains the right-side N-vector for the
	 *           problem, A * X = B.  On return contains the solution
	 *           N-vector, X.
	 *     ------------------------------------------------------------------
	 *     ERROR CONDITION
	 *        A DIVISION BY ZERO WILL OCCUR IF THE INPUT FACTOR CONTAINS A
	 *        ZERO ON THE DIAGONAL.  TECHNICALLY THIS INDICATES SINGULARITY
	 *        BUT IT IS OFTEN CAUSED BY IMPROPER ARGUMENTS OR IMPROPER
	 *        setting of LDA.  The user can avoid sending a singular matrix
	 *        to this subr by testing INFO (set by _GEFS or _GEFA) or
	 *        RCOND (set by _GEFSC or _GERC) before calling this subr.
	 *        Nonsingularity is indicated by INFO .eq. 0 or RCOND .ne. ZERO.
	 *     ------------------------------------------------------------------
	 *     LINPACK. THIS VERSION DATED 08/14/78 .
	 *     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
	 *     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
	 *     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
	 *     Math, Philadelphia, 1979.
	 *     Adapted from LINPACK for the JPL Math77 library by
	 *     C. L. Lawson, JPL, Aug 1987.
	 *     ------------------------------------------------------------------
	 *     Subprograms referenced: DAXPY
	 *     ------------------------------------------------------------------ */
	/*     ------------------------------------------------------------------ */
	nm1 = n - 1;

	/*        SOLVE  A * X = B
	 *        FIRST SOLVE  L*Y = B */

	for (k = 1; k <= nm1; k++)
	{
		l = Ipvt[k];
		t = B[l];
		if (l != k)
		{
			B[l] = B[k];
			B[k] = t;
		}
		pdaDaxpy( n - k, t, &A(k - 1,k), 1, &B[k + 1], 1 );
	}

	/*        NOW SOLVE  U*X = Y
	 * */
	for (kb = 1; kb <= n; kb++)
	{
		k = n + 1 - kb;
		if (A(k - 1,k - 1) != ZERO)
		{
			B[k] /= A(k - 1,k - 1);
		}
		else
		{
			pdaErmsg( "DGESLD", 1, 0, "A diagonal element is zero", '.' );
			return;
		}
		t = -B[k];
		pdaDaxpy( k - 1, t, &A(k - 1,0), 1, &B[1], 1 );
	}
	return;
#undef	A
} /* end of function */

