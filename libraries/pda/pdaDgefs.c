/*Translated by FOR_C, v3.4.2 (-), on 07/09/115 at 08:31:41 */
/*FOR_C Options SET: ftn=u io=c no=p op=aimnv s=dbov str=l x=f - prototypes */
#include <math.h>
#include "fcrt.h"
#include "pda.h"
#include <stdlib.h>
void /*FUNCTION*/ pdaDgefs(
double *a,
long lda,
long n,
double *b,
long ldb,
long nb,
long ipvt[],
long *info)
{
#define A(I_,J_)	(*(a+(I_)*(lda)+(J_)))
#define B(I_,J_)	(*(b+(I_)*(ldb)+(J_)))
	long int j;
		/* OFFSET Vectors w/subscript range: 1 to dimension */
	long *const Ipvt = &ipvt[0] - 1;
		/* end of OFFSET VECTORS */

	/* Copyright (c) 1996 California Institute of Technology, Pasadena, CA.
	 * ALL RIGHTS RESERVED.
	 * Based on Government Sponsored Research NAS7-03001.
	 *>> 1994-11-11 DGEFS  Krogh   Declared all vars.
	 *>> 1994-10-20 DGEFS  Krogh  Changes to use M77CON
	 *>> 1987-08-18 DGEFS  Lawson  Initial code.
	 *--D replaces "?": ?GEFS, ?GEFA, ?GESLD
	 *
	 *     Solves a system of linear equations,  A * X = B,
	 *     where A is a square nonsingular matrix of order N and B is an
	 *     N by NB matrix.  The solution is the N by NB matrix X that will
	 *     be stored on return in place of B in the array B().
	 *     ------------------------------------------------------------------
	 *     Uses subroutines derived from LINPACK.
	 *     Ref: LINPACK Users' Guide, by J. J. Dongarra, C. B. Moler,
	 *     J. R. Bunch, and G. W. Stewart, publ by Soc. for Indust. and Appl.
	 *     Math, Philadelphia, 1979.
	 *     Adapted for the JPL Math77 library by C. L. Lawson, JPL, Aug 1987.
	 *     ------------------------------------------------------------------
	 *                    Subroutine arguments
	 *
	 *     A(,)  [inout]  On entry contains the N by N matrix A.
	 *           On return contains the LU factorization of A as computed by
	 *           LINPACK subroutines.
	 *
	 *     LDA  [in]  Leading dimensioning parameter for the array A(,).
	 *           Require LDA .ge. N.
	 *
	 *     N  [in]  The order of the matrix A and number of rows in the
	 *           matrices B and X.
	 *
	 *     B(,)  [inout]  On entry contains the N by NB matrix, B.  On return
	 *           contains the N by NB solution matrix, X.  Could be a
	 *           Singly subscripted array if NB .eq. 1.
	 *
	 *     LDB  [in]  Leading dimensioning parameter for the array B(,).
	 *           Require LDB .ge. N.
	 *
	 *     NB  [in]  Number of columns in the matrices B and X.  If NB .lt. 1
	 *           the matrix, A, will be factored but no reference will be
	 *           made to the array B(,).
	 *
	 *     IPVT()  [out]  Integer array of length at least N.  On return will
	 *           contain a record of the row interchanges done during
	 *           factorization of A.
	 *
	 *     INFO  [out]  Set to zero if all diagonal elements in the U matrix
	 *           of the LU factorization are found to be nonzero.  If nonzero
	 *           it is the index of the first diagonal element of U that was
	 *           found to be zero.  In this latter case the solution X will
	 *           not be computed.
	 *     ------------------------------------------------------------------
	 *     Subprograms called: DGEFA, DGESLD
	 *     ------------------------------------------------------------------ */
	/*     ------------------------------------------------------------------ */
	pdaDgefa( a, lda, n, ipvt, info );
	if (*info != 0)
		return;
	for (j = 1; j <= nb; j++)
	{
		pdaDgesld( a, lda, n, ipvt, &B(j - 1,0) );
	}
	return;
#undef	B
#undef	A
} /* end of function */

