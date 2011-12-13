       SUBROUTINE PDA_LMERR( FCN, M, N, X, RESID, MXITER, DRESID, JACOB,
     :                       ALPHA, PIVOT, WORK, SIGMA, CORREL, COVAR,
     :                       FLAG, STATUS )

*  Name:
*     PDA_LMERR
*
*  Purpose:
*     Estimates the errors of least-squares fits to non-linear
*     functions

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PDA_LMERR( FCN, M, N, X, RESID, MXITER, DRESID, JACOB,
*                     ALPHA, PIVOT, WORK, SIGMA, CORREL, COVAR,
*                     FLAG, STATUS )

*  Description:
*     This routine complements the PDA_LMDIF1 routine that fits to
*     non-linear functions, by determining the errors in the fitted
*     parameters.  It assumes normally distributed errors.

*     This routine uses the same fitting function defined for
*     PDA_LMFUN1 with known best-fit values and residuals to
*     return the uncertainties in the best-fit values and
*     the correlations between the variables.

*  Arguments:
*     FCN = EXTERNAL (Given)
*        The name of the subroutine that generates the fitting function.
*        Its call statement is specified as
*                   CALL FCN( M, N, X, FVEC, IFLAG )
*        as used by the PDA_LMDIF1 fitting routine.
*     M = INTEGER (Given)
*        The number of functions (i.e. number of values to fit).
*        This must be greater than N.
*     N = INTEGER (Given)
*        The number of free parameters of the fit.
*     X( N ) = DOUBLE PRECISION (Given)
*         The best-fit parameters.
*     RESID( M ) = DOUBLE PRECISION (Given)
*        The residuals for the best fit.
*     MXITER = INTEGER (Given)
*        The maximum number of loops in error evaluation.  It is
*        constrained to be from one to ten.
*     DRESID( M ) = DOUBLE PRECISION (Returned)
*        Work array for the displaced-fit residuals.  On output, this
*        is equal to argument RESID.
*     JACOB( M, N ) = DOUBLE PRECISION (Returned)
*        Work array for the Jacobian derived by finite differences.
*     ALPHA( N, N ) = DOUBLE PRECISION (Returned)
*        Work array to hold the curvature matrix.
*     PIVOT( N ) = INTEGER (Returned)
*        Work array to store pivot indices used in matrix inversion.
*     WORK( N ) = DOUBLE PRECISION (Returned)
*        Work array for matrix inversion.
*     SIGMA( N ) = DOUBLE PRECISION (Returned)
*        The uncertainties for the fit parameters.
*     CORREL( N, N ) = DOUBLE PRECISION (Returned)
*        The two-variable correlations
*     COVAR( N, N )  = DOUBLE PRECISION (Returned)
*        The covariance matrix.
*     FLAG( N ) = LOGICAL (Returned)
*        An element is set .TRUE. if the corresponding parameter
*        likely caused the failure of the inversion of the
*        curvature matrix.
*     STATUS = INTEGER (Given & Returned)
*        Global inherited status.  It is set to PDA__FICMX if the
*        curvature matrix cannot be inverted.

*   Notes:
*     The errors are derived as follows.  First create the M * N
*     Jacobian matrix by differencing the residuals of the best-fit
*     and those derived from a small increment of each parameter
*     around the best-fit parameters, and normalised by the parameter
*     increment.

*     Next form the N-by-N curvature matrix (one half times the
*     Hessian matrix of second derivatives), by summing over the
*     elements of the product of the transposed Jacobian matrix and
*     the Jacobian itself for all functions.
*
*     Finally invert the curvature matrix to form the covariance matrix.
*     The diagonal elements give the errors and other elements, the
*     correlations between the parameters.

*     For more details see, for example, Chapter 15 of Press et al.'s
*     "Numerical Recipes" (Cambridge University Press).

*  Copyright:
*     Copyright (C) 2007 Particle Physics and Astronomy Research
*     Council.  All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     PWD: Peter W. Draper (JAC, Durham University)
*     {enter_new_authors_here}

*  History:
*     2007 February 13 (MJC):
*        Original version.
*     2007 May 21 (PWD):
*        Use EMS_ calls to replace ERR_ and MSG_.  PDA library depends
*        only on EMS.
*     2007 May 31 (MJC):
*        Replace external machine precision with internal PDA_D1MACH.
*     2009 December 5 (MJC):
*        Use explicit error code for the failure to invert the curvature
*        matrix.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PDA_ERR'          ! PDA error constants

*  Arguments Given:
      INTEGER M
      INTEGER N
      DOUBLE PRECISION X( N )
      DOUBLE PRECISION RESID( M )
      INTEGER MXITER

*  Arguments Returned:
      DOUBLE PRECISION DRESID( M )
      DOUBLE PRECISION JACOB( M, N )
      DOUBLE PRECISION ALPHA( N, N )
      INTEGER PIVOT( N )
      DOUBLE PRECISION WORK( N )
      DOUBLE PRECISION SIGMA( N )
      DOUBLE PRECISION CORREL( N, N )
      DOUBLE PRECISION COVAR( N, N )
      INTEGER IERROR
      LOGICAL FLAG( N )

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
       DOUBLE PRECISION PDA_D1MACH
       EXTERNAL PDA_D1MACH       ! Machine precision
       EXTERNAL FCN              ! Name of the fitting function

*  Local Constants:
       DOUBLE PRECISION PSINIT    ! Initial parameter shift fraction
       PARAMETER ( PSINIT = 1.D-2 )

*  Local Variables:
       DOUBLE PRECISION CHISQ    ! Chi-squared
       DOUBLE PRECISION DELTAX   ! Increment in a parameter
       DOUBLE PRECISION DUMDET( 2 ) ! Dummy determinant
       INTEGER IFAIL             ! Error indicator from PDA_DGEFA
       INTEGER IFLAG             ! Error indicator from FCN
       INTEGER IFUN              ! Loop counter for the functions
       INTEGER J                 ! Loop counter
       INTEGER K                 ! Loop counter
       INTEGER ITER              ! Iteration loop counter
       DOUBLE PRECISION MINPS    ! Minimum parameter shift fraction
       INTEGER MITER             ! Constrained number of iterations
       DOUBLE PRECISION PS       ! Parameter shift
       DOUBLE PRECISION SUM      ! Sum of Jacobian products
       DOUBLE PRECISION SUMRES   ! Sum of the residuals squared
       DOUBLE PRECISION XCOPY    ! Copy of current best-fit parameter

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Define a parameter.  Cannot do this with a PARAMETER statement
*  because of the PDA function.
      MINPS = 1.D2 * PDA_D1MACH( 4 )

*  Validate that there are sufficient degrees of freedom.
      IF ( M .LE. N ) THEN
         STATUS = SAI__ERROR
         CALL EMS_SETI( 'M', M )
         CALL EMS_SETI( 'N', N )
         CALL EMS_REP( 'PDA_LMERR_ERR1', 'Too few points (^M) to '/
     :                 /'determine ^N coefficients'' errors.', STATUS )
         GOTO 999
      END IF

*  Constrain the number of loops.
      MITER = MIN( 10, MAX( 1, MXITER ) )

*  Initialise some variables.
      IFLAG = 0
      ITER = 0

*  Form the Jacobian at the best-fit solution.
*  ===========================================

*  Start with an initial guess for the increment to the parameter value
*  and refine it iteratively.
      PS = PSINIT
      DO WHILE ( ITER .LT. MITER )
         ITER = ITER + 1

*  Look at each parameter in turn.
         DO J = 1, N
            XCOPY = X( J )

*  Use the previous estimate of the error to refine the parameter shift.
            IF ( ITER .GT. 1 ) THEN
               PS = SIGMA( J ) / MAX( MINPS, ABS( XCOPY ) )
            END IF

*  Find shifted parameter value, retaining a copy of the original
*  best-fit value.
            DELTAX = MAX( MINPS, PS * ABS( X( J ) ) )
            X( J ) = XCOPY + DELTAX

*  Evaluate the function at the shifted parameter.  ***Not sure what to
*  do should IFLAG come back negative.  It is not likely for a small
*  change around the best fit.
            CALL FCN( M, N, X, DRESID, IFLAG )

*  Assign the first differential matrix aka the Jacobian.  It uses the
*  best-possible guess for the relative error for the current parameter.
            DO IFUN = 1, M
               JACOB( IFUN, J ) = ( RESID( IFUN ) - DRESID( IFUN ) ) /
     :                            DELTAX
            END DO

*  Reset the best-fit value for the current parameter.
            X( J ) = XCOPY
         END DO

*  As most information communicatied to and from the evaluation
*  function is via COMMON blocks because of the fixed API, we recompute
*  the the best-fit solution to restore associated values in common.
*  As it worked before, it is safe not to test the IFLAG.
         CALL FCN( M, N, X, DRESID, IFLAG )

*  Sum the Jacobian J^T * J for all functions to form the curvature
*  matrix.
         DO J = 1, N
            DO K = 1, J
               SUM = 0.0D0
               DO IFUN = 1, M
                  SUM = SUM + JACOB( IFUN, J ) * JACOB( IFUN, K )
               END DO
               ALPHA( J, K ) = SUM

*  Curvature matrix is symmetric so assign the subdiagonal elements
*  too.
               IF ( K .NE. J ) ALPHA( K, J ) = SUM
            END DO
         END DO

*  Normalisation factor requires the sum of the squared deviations
*  and the number of degrees of freedom, estimating the the
*  statistical error from the scatter in the data.
         SUMRES = 0.0D0
         DO J = 1, M
            SUMRES = SUMRES + DRESID( J ) * DRESID( J )
         END DO
         CHISQ = SUMRES / DBLE( M - N )

*  In case the curvature matrix cannot be inverted, flag those
*  parameters with small diagonal components of curvature; these are
*  the likely null variables that caused the matrix inversion to fail.
         DO J = 1, N
            FLAG( J ) = ABS( ALPHA( J, J ) ) .LE. MINPS
         END DO

*  Invert the curvature matrix to give the covariance matrix.
*  ==========================================================

*  As the inversion is performed in situ, first copy the curvature array
*  to the desired covariance array.
         DO J = 1, N
            DO K = 1, N
               COVAR( K, J ) = ALPHA( K, J )
            END DO
         END DO

*  Perform a Gauss-Jordan elimination, first obtaining the pivot
*  indices, but testing for an error to prevent a divide-by-zero
*  failure.
         CALL PDA_DGEFA( COVAR, N, N, PIVOT, IFAIL )
         IF ( IFAIL .NE. 0 ) THEN
            STATUS = PDA__FICMX
            CALL EMS_REP( ' ', 'Failed to invert the curvature matrix.',
     :                    STATUS )
            GOTO 999
         ELSE

*  The final 1 asks for the inverse only.
            CALL PDA_DGEDI( COVAR, N, N, PIVOT, DUMDET, WORK, 1 )
         END IF

*  Extract the parameter errors from the covariance matrix.
          DO J = 1, N
             SIGMA( J ) = MAX( MINPS,
     :                         SQRT( ABS( COVAR( J, J ) * CHISQ )  ) )

*  Now normalise to form the symmetric correlation matrix.
             DO K = 1, J
                CORREL( K, J ) = COVAR( K, J ) / SIGMA( J ) / SIGMA( K )
                CORREL( J, K ) = CORREL( K, J )
             END DO
          END DO

*  Repeat with better estimates of the values of the shifts in
*  parameters for the derivatives and hence a better Jacobian matrix.
       END DO

  999  CONTINUE

       END
