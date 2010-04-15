      SUBROUTINE SCULIB_MRQMIN (X, Y, SIG, NDATA, A, IA, MA, COVAR,
     :  ALPHA, NCA, CHISQ, FUNCS, ALAMDA, STATUS)
*+
*  Name:
*     SCULIB_MRQMIN

*  Purpose:
*     Levenberg-Marquardt method non-linear least-squares
*     fit from Numerical Recipes in FORTRAN.

*  Description:
*     Levenberg-Marquardt method, attempting to reduce the value of
*     chi-squared of a fit between a set of data points X(1:NDATA),
*     Y(1:NDATA), and a non-linear function dependent on MA coefficients
*     A(1:MA). The input array IA(1:MA) indicates by non-zero components
*     those components of A that should be fitted for, and by 0 entries
*     those components that should be held fixed at their input values.
*     The program returns current best-fit values for the parameters
*     A(1:MA), and chi-squared = CHISQ. The arrays COVAR (1:NCA,1:NCA),
*     ALPHA (1:NCA,1:NCA) with physical dimension NCA (>= the number of
*     fitted parameters) are used as working space during most iterations.
*     Supply a subroutine FUNCS (X, A, YFIT, DYDA, MA) that evaluates
*     the fitting function YFIT, and its derivatives DYDA with respect to the
*     the fitting paramters A at X. On the first call provide an initial
*     guess for the parameters A, and set ALAMDA < 0 for initialisation
*     (which then sets ALAMDA = 0.001). If a step succeeds CHISQ becomes
*     smaller and ALAMDA decreases by a factor of 10. If a step fails
*     ALAMDA grows by a factor of 10. You must call this routine repeatedly
*     until convergence is achieved. Then, make one final call with
*     ALAMDA = 0, so that COVAR (1:MA,1:MA) returns the covariance matrix,
*     and ALPHA the curvature matrix. Parameters held fixed will return
*     zero covariance.
*       Copied from MRQMIN on p.680 of Numerical Recipes in Fortran, with
*     STATUS added.

*  Invocation:
*     CALL SCULIB_MRQMIN (X, Y, SIG, NDATA, A, IA, MA, COVAR,
*    :  ALPHA, NCA, CHISQ, FUNCS, ALAMDA, STATUS)

*  Arguments:
*     X( NDATA ) = REAL (Given)
*       X Data points
*     Y( NDATA ) = REAL (Given)
*       Y data points
*     SIG ( NDATA ) = REAL (Given)
*     NDATA = INTEGER (Given)
*       Number of data points in arrays
*     A( MA ) = REAL (Given & Returned)
*       The input parameter values. Contains the best-fit values on exit.
*     IA( MA ) = INTEGER (Given)
*       Array specifying which components should be fitted. A zero
*       indicates the parameter should not be fitted -- the parameters
*       are kept at their input values. Non-zero indicates the parameter
*       is free to be modified.
*     MA = INTEGER (Given)
*       Number of coefficients/parameters for function.
*     COVAR ( NCA, NCA ) = REAL (Returned)
*       The covariance matrix. Fixed parameters will return zero covariance.
*     ALPHA ( NCA, NCA ) = REAL (Returned)
*       The curvature matrix.
*     NCA = INTEGER (Given)
*       Size of output arrays. Must be greater than the number of fitted
*       parameters
*     CHISQ = REAL (Returned)
*       ChiSq of fit.
*     FUNCS = REAL FUNCTION (Given)
*       The function to be fitted. Must take arguments of X, A, YFIT,
*       DYDA and MA, where YFIT is the fitting function and DYDA the
*       the derivatives with respect to parameters A at X.
*     ALAMDA = REAL (Given & Returned)
*       The stepping factor for the iteration. Should be set to <0
*       initially.
*     STATUS = INTEGER (Given & Returned)
*       Global Status

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     13-SEP-93: Copied from Numerical Recipes in Fortran: 2nd Edition, p.680.
*                (REVAD::JFL)
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER MA
      INTEGER NCA
      INTEGER NDATA
      INTEGER IA( MA )
      REAL    SIG( NDATA )
      REAL    X( NDATA )
      REAL    Y( NDATA )

*  Arguments Given & Returned:
      REAL    ALAMDA
      REAL    A( MA )

*  Arguments Returned:
      REAL    CHISQ
      REAL    ALPHA ( NCA,NCA )
      REAL    COVAR ( NCA,NCA )

*  Status:
      INTEGER STATUS

*  External references:
      REAL    FUNCS

*  Global variables:

*  Local Constants:
      INTEGER MMAX
      PARAMETER (MMAX = 20)

*  Local variables:
      INTEGER J
      INTEGER K
      INTEGER L
      INTEGER M
      INTEGER MFIT
      REAL    OCHISQ
      REAL    ATRY( MMAX )
      REAL    BETA( MMAX )
      REAL    DA ( MMAX )
      SAVE OCHISQ, ATRY, BETA, DA, MFIT

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      IF (ALAMDA .LT. 0) THEN

*  initialisation

         MFIT = 0
         DO J =1, MA
            IF (IA(J) .NE. 0) MFIT = MFIT + 1
         END DO
         ALAMDA = 0.001
         CALL SCULIB_MRQCOF (X, Y, SIG, NDATA, A, IA, MA, ALPHA, BETA,
     :     NCA, CHISQ, FUNCS, STATUS)
         IF (STATUS .NE. SAI__OK) RETURN
         OCHISQ = CHISQ
         DO J = 1, MA
            ATRY (J) = A (J)
         END DO
      END IF

*  alter linearised fitting matrix, by augmenting diagonal elements

      J = 0
      DO L = 1, MA
         IF (IA(L) .NE. 0) THEN
            J = J + 1
            K = 0
            DO M = 1, MA
               IF (IA(M) .NE. 0) THEN
                  K = K + 1
                  COVAR (J,K) = ALPHA (J,K)
               END IF
            END DO
            COVAR (J,J) = ALPHA (J,J) * (1.0 + ALAMDA)
            DA (J) = BETA (J)
         END IF
      END DO

*  matrix solution

      CALL SCULIB_GAUSSJ (COVAR, MFIT, NCA, DA, 1, 1, STATUS)
      IF (STATUS .NE. SAI__OK) RETURN

*  once converged, evaluate covariance matrix

      IF (ALAMDA .EQ. 0) THEN
         CALL SCULIB_COVSRT (COVAR, NCA, MA, IA, MFIT, STATUS)
         RETURN
      END IF

*  did the trial succeed?

      J = 0
      DO L = 1, MA
         IF (IA(L) .NE. 0) THEN
            J = J + 1
            ATRY (L) = A(L) + DA (J)
         END IF
      END DO

      CALL SCULIB_MRQCOF (X, Y, SIG, NDATA, ATRY, IA, MA, COVAR, DA,
     :  NCA, CHISQ, FUNCS, STATUS)
      IF (STATUS .NE. SAI__OK) RETURN

      IF (CHISQ .LT. OCHISQ) THEN

*  success, accept the new solution

         ALAMDA = 0.1 * ALAMDA
         OCHISQ = CHISQ
         J = 0
         DO L = 1, MA
            IF (IA(L) .NE. 0) THEN
               J = J + 1
               K = 0
               DO M = 1, MA
                  IF (IA(M) .NE. 0) THEN
                     K = K + 1
                     ALPHA (J,K) = COVAR (J,K)
                  END IF
               END DO
               BETA (J) = DA (J)
               A (L) = ATRY (L)
            END IF
         END DO

      ELSE

*  failure, increase ALAMDA and return

         ALAMDA = 10.0 * ALAMDA
         CHISQ = OCHISQ

      END IF

      END
