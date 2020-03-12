/*Translated by FOR_C, v3.4.2 (-), on 07/09/115 at 08:30:52 */
/*FOR_C Options SET: ftn=u io=c no=p op=aimnv s=dbov str=l x=f - prototypes */
#include <math.h>
#include "fcrt.h"
#include "pda.h"
#include <stdlib.h>
void /*FUNCTION*/ pdaDgesl(
double *a,
long lda,
long n,
long ipvt[],
double b[],
long job)
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
	 *>> 2001-11-04 DGESL  Krogh  Fixes for F77 and conversion to single
	 *--D replaces "?": ?GESL, ?GECO, ?AXPY, ?DOT
	 ****BEGIN PROLOGUE  DGESL
	 ****PURPOSE  Solve the real system A*X=B or TRANS(A)*X=B using the
	 *            factors computed by DGECO or DGEFA.
	 ****CATEGORY  D2A1
	 ****TYPE      DOUBLE PRECISION (SGESL-S, DGESL-D, CGESL-C)
	 ****KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE
	 ****AUTHOR  Moler, C. B., (U. of New Mexico)
	 ****DESCRIPTION
	 *
	 *     DGESL solves the double precision system
	 *     A * X = B  or  TRANS(A) * X = B
	 *     using the factors computed by DGECO or DGEFA.
	 *
	 *     On Entry
	 *
	 *        A       DOUBLE PRECISION(LDA, N)
	 *                the output from DGECO or DGEFA.
	 *
	 *        LDA     INTEGER
	 *                the leading dimension of the array  A .
	 *
	 *        N       INTEGER
	 *                the order of the matrix  A .
	 *
	 *        IPVT    INTEGER(N)
	 *                the pivot vector from DGECO or DGEFA.
	 *
	 *        B       DOUBLE PRECISION(N)
	 *                the right hand side vector.
	 *
	 *        JOB     INTEGER
	 *                = 0         to solve  A*X = B ,
	 *                = nonzero   to solve  TRANS(A)*X = B  where
	 *                            TRANS(A)  is the transpose.
	 *
	 *     On Return
	 *
	 *        B       the solution vector  X .
	 *
	 *     Error Condition
	 *
	 *        A division by zero will occur if the input factor contains a
	 *        zero on the diagonal.  Technically this indicates singularity
	 *        but it is often caused by improper arguments or improper
	 *        setting of LDA .  It will not occur if the subroutines are
	 *        called correctly and if DGECO has set RCOND .GT. 0.0
	 *        or DGEFA has set INFO .EQ. 0 .
	 *
	 *     To compute  INVERSE(A) * C  where  C  is a matrix
	 *     with  P  columns
	 *           CALL DGECO(A,LDA,N,IPVT,RCOND,Z)
	 *           IF (RCOND is too small) GO TO ...
	 *           DO 10 J = 1, P
	 *              CALL DGESL(A,LDA,N,IPVT,C(1,J),0)
	 *        10 CONTINUE
	 *
	 ****REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
	 *                 Stewart, LINPACK Users' Guide, SIAM, 1979.
	 ****ROUTINES CALLED  DAXPY, DDOT
	 ****END PROLOGUE  DGESL */

	/****FIRST EXECUTABLE STATEMENT  DGESL */
	nm1 = n - 1;
	if (job != 0)
		goto L_50;

	/*        JOB = 0 , SOLVE  A * X = B
	 *        FIRST SOLVE  L*Y = B
	 * */
	if (nm1 < 1)
		goto L_30;
	for (k = 1; k <= nm1; k++)
	{
		l = Ipvt[k];
		t = B[l];
		if (l == k)
			goto L_10;
		B[l] = B[k];
		B[k] = t;
L_10:
		;
		pdaDaxpy( n - k, t, &A(k - 1,k), 1, &B[k + 1], 1 );
	}
L_30:
	;

	/*        NOW SOLVE  U*X = Y
	 * */
	for (kb = 1; kb <= n; kb++)
	{
		k = n + 1 - kb;
		B[k] /= A(k - 1,k - 1);
		t = -B[k];
		pdaDaxpy( k - 1, t, &A(k - 1,0), 1, &B[1], 1 );
	}
	goto L_100;
L_50:
	;

	/*        JOB = NONZERO, SOLVE  TRANS(A) * X = B
	 *        FIRST SOLVE  TRANS(U)*Y = B
	 * */
	for (k = 1; k <= n; k++)
	{
		t = pdaDdot( k - 1, &A(k - 1,0), 1, &B[1], 1 );
		B[k] = (B[k] - t)/A(k - 1,k - 1);
	}

	/*        NOW SOLVE TRANS(L)*X = Y
	 * */
	if (nm1 < 1)
		goto L_90;
	for (kb = 1; kb <= nm1; kb++)
	{
		k = n - kb;
		B[k] += pdaDdot( n - k, &A(k - 1,k), 1, &B[k + 1], 1 );
		l = Ipvt[k];
		if (l == k)
			goto L_70;
		t = B[l];
		B[l] = B[k];
		B[k] = t;
L_70:
		;
	}
L_90:
	;
L_100:
	;
	return;
#undef	A
} /* end of function */

