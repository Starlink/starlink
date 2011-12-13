      SUBROUTINE CCD1_FCOV( M, N, FSUMSQ, X, COVAR, LCOV, WRK1, WRK2,
     :                      WRK3, WRK4, IFAIL )

*+
*  Name:
*     CCD1_FCOV

*  Purpose:
*     Determines the covariances of a non-linear fit.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_FCOV( M, N, FSUMSQ, X, COVAR, LCOV,
*                     WRK1, WRK2, WRK3, WRK4, IFAIL )

*  Description:
*     This routine calculates the covariance matrix of the estimated
*     regression coefficients for a non-linear least squares problem.
*     It uses the estimate of the jacobian of f(x) at the solution.
*
*     The method used is based on the calculation of the inverse of the
*     cross-products of the jacobian scaled by the variance of the
*     residual at the solution - i.e.
*
*        vars (J^t J)^-1.
*
*     This is calculated using a single value decomposition (which
*     should be more stable to near singularities) using the fact that
*     the inverse cross product of the jacobian may be estimated as
*
*        V S^-2 V^t.
*
*     Where V is the right-hand side of the decomposition and S the
*     vector of singular values.
*
*     This routine is CCDPACK specific and is designed to be called from
*     CCD1_SZSLV after a call to DQEDS. CCD1_CALCJ is defined to use the
*     common block information in CCD1_SZSLV to calculate the jacobian.

*  Arguments:
*     M = INTEGER (Given)
*        The number of residuals in used in fit (observations).
*     N = INTEGER (Given)
*        The number of variables in fit.
*     FSUMSQ = DOUBLE PRECISION (Given)
*        The sum of squares of the residuals at the solution X
*     X( N ) = DOUBLE PRECISION (Given)
*        The solution vector.
*     COVAR( LCOV, N ) = DOUBLE PRECISION (Returned)
*        The calculated covariances. This is an NxN section on exit.
*     LCOV = INTEGER (Given)
*        The first dimension of COVAR as declared in the calling
*        routine. This must be at least M large.
*     WRK1( N )  = DOUBLE PRECISION (Returned)
*        Workspace.
*     WRK2( N )  = DOUBLE PRECISION (Returned)
*        Workspace.
*     WRK3( N, M )  = DOUBLE PRECISION (Returned)
*        Workspace.
*     WRK4( M )  = DOUBLE PRECISION (Returned)
*        Workspace.
*     IFAIL = INTEGER (Returned)
*        Indicates the success or failure of this routine. If 0 then
*        a covariance matrix has been sucessfully calculated. If 1 then
*        the calculation has failed. If greater than 1 then the matrix
*        contains linear or near linear dependencies. A matrix has still
*        been calculated and may still be a good solution.

*  Copyright:
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     2-OCT-1996 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'         ! Primitive data constants

*  Arguments Given:
      INTEGER M
      INTEGER N
      INTEGER LCOV
      DOUBLE PRECISION FSUMSQ
      DOUBLE PRECISION X( N )

*  Arguments Returned:
      DOUBLE PRECISION COVAR( LCOV, N )
      INTEGER IFAIL
      DOUBLE PRECISION WRK1( N )
      DOUBLE PRECISION WRK2( N )
      DOUBLE PRECISION WRK3( N, M )
      DOUBLE PRECISION WRK4( M )

*  Local Variables:
      DOUBLE PRECISION SNORM    ! Normalization factor for single values
      DOUBLE PRECISION TEMP     ! Summation temporary
      DOUBLE PRECISION VSMALL   ! Smallest value that isn't 0.0D0
      INTEGER I                 ! Loop counter
      INTEGER J                 ! Loop counter
      INTEGER JOB               ! Type of function
      INTEGER K                 ! Loop counter
      INTEGER NFAIL             ! Number of near singularities
      INTEGER UR( 1 )           ! Dummy
*.

*  Initialise status flag.
      IFAIL = 0

*  Calculate the jacobian.
      CALL CCD1_CALCJ( M, N, X, COVAR, LCOV )

*  Form the single value decomposition of this (we want the S and V
*  components for our analysis).
      JOB = 1                   ! Compute only right vectors (WRK3).
      CALL PDA_DSVDC( COVAR, LCOV, M, N, WRK1, WRK2, UR, 1, WRK3, N,
     :                WRK4, JOB, IFAIL )
      IF ( IFAIL .NE. 0 ) THEN
         IFAIL = 1
         GO TO 99
      END IF

*  Estimate a small just non-zero value (within rounding value created
*  by say about N operations).
      VSMALL = ABS( VAL__EPSD * WRK1( 1 ) * 2.0D0 * 10.0D0 ) ! Same as E04YCF?

*  Scale S so that smallest value won't blow things up too much (make
*  this stay about the same value when inverse squared). This means we
*  should be OKish as long as the product of any two elements of V do
*  not overflow.
      DO 1 I = N, 1, -1
         IF ( WRK1( I ) .GT. VSMALL ) THEN
            SNORM = WRK1( I )
            GO TO 5
         ELSE
            WRK1( I ) = 0.0D0
         END IF
 1    CONTINUE
 5    CONTINUE
      DO 6 I = 1, N
         IF ( WRK1( I ) .NE. 0.0D0 ) THEN
            WRK1( I ) = SNORM / ( WRK1( I ) * WRK1( I ) )
         END IF
 6    CONTINUE

*  Now derive the covariances (these are scaled by mean residual).
      NFAIL = 0
      DO 2 K = 1, N
         DO 3 J = 1, N
            TEMP = 0.0D0
            DO 4 I = 1, N
               IF ( WRK1( I ) .NE. 0.0D0 ) THEN
                  TEMP = TEMP + WRK3( J, I ) * WRK3( K, I ) * WRK1( I )
               ELSE
                  NFAIL = NFAIL + 1
               END IF
 4          CONTINUE
            COVAR( J, K ) = TEMP * FSUMSQ / SNORM
 3       CONTINUE
 2    CONTINUE
      IF ( NFAIL .NE. 0 ) THEN
         IFAIL = NFAIL + 1
      END IF

*  Exit in error label.
 99   CONTINUE

      END
