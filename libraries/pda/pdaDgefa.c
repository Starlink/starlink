/*Translated by FOR_C, v3.4.2 (-), on 07/09/115 at 08:30:52 */
/*FOR_C Options SET: ftn=u io=c no=p op=aimnv s=dbov str=l x=f - prototypes */
#include <math.h>
#include "fcrt.h"
#include "pda.h"
#include <stdlib.h>
		/* PARAMETER translations */
#define	ONE	1.e0
#define	ZERO	0.e0
		/* end of PARAMETER translations */

void /*FUNCTION*/ pdaDgefa(
double *a,
long lda,
long n,
long ipvt[],
long *info)
{
#define A(I_,J_)	(*(a+(I_)*(lda)+(J_)))
	long int j, k, kp1, l, nm1;
	double t;
		/* OFFSET Vectors w/subscript range: 1 to dimension */
	long *const Ipvt = &ipvt[0] - 1;
		/* end of OFFSET VECTORS */

	/* Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
	 * ALL RIGHTS RESERVED.
	 * Based on Government Sponsored Research NAS7-03001.
	 *>> 1996-03-30 DGEFA  Krogh  Added external statement.
	 *>> 1994-10-20 DGEFA  Krogh  Changes to use M77CON
	 *>> 1987-08-18 DGEFA  Lawson  Initial code.
	 *--D replaces "?": ?GEFA, ?SCAL, ?AXPY, I?AMAX
	 *
	 *     DGEFA computes the LU factorization of the N x N matrix A by
	 *     Gaussian elimination.  This produces matrices, L and U, that
	 *     satisfy L * U = A, where U is an upper triangular matrix and
	 *     L is a permutation of a lower triangular matrix. Use of this
	 *     subroutine would typically be followed by use of other
	 *     subroutines that would use this factorization to solve a
	 *     system of linear equations, or to compute the inverse matrix
	 *     or the determinant of A.
	 *
	 *     DGEFA may be referenced indirectly via _GECO, but it can be called
	 *     directly with a saving in time if the reciprocal condition number
	 *     RCOND is not needed.
	 *     (Time for _GECO) = (1 + 9/N)*(Time for DGEFA) .
	 *
	 *     ------------------------------------------------------------------
	 *                        Subroutine arguments
	 *
	 *     A(,)  [inout]  An array of size at least N x N.  On entry must
	 *            contain an N x N matrix, A, to be factored.  On return will
	 *            contain the LU factors of A.
	 *
	 *     LDA  [in]  Leading dimensioning parameter for the array A(,).
	 *
	 *     N  [in]  The order of the matrix, A.
	 *
	 *     IPVT()  [in]  An integer array of length at least N, containg a
	 *           record of the row interchanges made during factorization of
	 *           A.
	 *
	 *      INFO  [out]  Indicate status on return
	 *              = 0  NORMAL VALUE.
	 *              = K  IF  U(K,K) .EQ. 0.0 .  THIS IS NOT AN ERROR
	 *              CONDITION FOR THIS SUBROUTINE, but it does
	 *              indicate that the usual following steps to solve
	 *              equations or compute an inverse matrix cannot
	 *              be done, at least by the usual straightforward
	 *              algorithms.  Use RCOND in _GECO for a more reliable
	 *              indication of singularity.
	 *     ------------------------------------------------------------------
	 *     LINPACK. THIS VERSION DATED 08/14/78 .
	 *     CLEVE MOLER, UNIVERSITY OF NEW MEXICO, ARGONNE NATIONAL LAB.
	 *     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
	 *     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
	 *     Math, Philadelphia, 1979.
	 *     Adapted from LINPACK for the JPL Math77 library by
	 *     C. L. Lawson, JPL, Aug 1987.
	 *     ------------------------------------------------------------------
	 *     Subprograms referenced: DAXPY,DSCAL,IDAMAX
	 *     ------------------------------------------------------------------ */
	/*     ------------------------------------------------------------------
	 *     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING */
	*info = 0;
	nm1 = n - 1;
	for (k = 1; k <= nm1; k++)
	{
		kp1 = k + 1;

		/*        FIND L = PIVOT INDEX
		 * */
		l = pdaIdamax( n - k + 1, &A(k - 1,k - 1), 1 ) + k - 1;
		Ipvt[k] = l;

		/*        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
		 * */
		if (A(k - 1,l - 1) == ZERO)
		{
			*info = k;
		}
		else
		{
			/*           INTERCHANGE IF NECESSARY */
			if (l != k)
			{
				t = A(k - 1,l - 1);
				A(k - 1,l - 1) = A(k - 1,k - 1);
				A(k - 1,k - 1) = t;
			}

			/*           COMPUTE MULTIPLIERS
			 * */
			t = -ONE/A(k - 1,k - 1);
			pdaDscal( n - k, t, &A(k - 1,k), 1 );

			/*           ROW ELIMINATION WITH COLUMN INDEXING
			 * */
			for (j = kp1; j <= n; j++)
			{
				t = A(j - 1,l - 1);
				if (l != k)
				{
					A(j - 1,l - 1) = A(j - 1,k - 1);
					A(j - 1,k - 1) = t;
				}
				pdaDaxpy( n - k, t, &A(k - 1,k), 1, &A(j - 1,k), 1 );
			}
		}
	}
	Ipvt[n] = n;
	if (A(n - 1,n - 1) == ZERO)
		*info = n;
	return;
#undef	A
} /* end of function */

