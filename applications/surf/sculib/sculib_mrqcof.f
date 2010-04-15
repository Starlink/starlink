      SUBROUTINE SCULIB_MRQCOF (X, Y, SIG, NDATA, A, IA, MA, ALPHA,
     :  BETA, NALP, CHISQ, FUNCS, STATUS)
*+
*  Name:
*     SCULIB_MRQCOF

*  Purpose:
*     called by SCULIB_MRQMIN

*  Description:
*     Utility routine called by SCULIB_MRQMIN, copied from Numerical
*     Recipes in Fortran, p. 681. Used by SCULIB_MRQMIN to evaluate the
*     linearised fitting matrix ALPHA, and vector BETA, and calculate
*     chi-squared.
*        Copied from MRQCOF on p.681 of Numerical Recipes in Fortran, with
*     STATUS added.

*  Invocation:
*     CALL SCULIB_MRQCOF (X, Y, SIG, NDATA, A, IA, MA, ALPHA,
*    :  BETA, NALP, CHISQ, FUNCS, STATUS)

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
*     ALPHA( NALP, NALP ) = REAL (Returned)
*     BETA( MA ) = REAL (Returned)
*     NALP = INTEGER  (Given)
*       Size of ALPHA.
*     CHISQ = REAL (Returned)
*       ChiSq of fit.
*     FUNCS = REAL FUNCTION (Given)
*       The function to be fitted. Must take arguments of X, A, YFIT,
*       DYDA and MA, where YFIT is the fitting function and DYDA the
*       the derivatives with respect to parameters A at X.
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
*     13-SEP-1993: Original copy (REVAD::JFL)
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER MA
      INTEGER NALP
      INTEGER NDATA
      INTEGER IA( MA )
      REAL    A( MA )
      REAL    SIG( NDATA )
      REAL    X(NDATA)
      REAL    Y(NDATA)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL    CHISQ
      REAL    ALPHA( NALP, NALP )
      REAL    BETA( MA )

*  Status:
      INTEGER STATUS

*  External references:
      EXTERNAL FUNCS

*  Global variables:

*  Local Constants:
      INTEGER MMAX
      PARAMETER (MMAX = 20)

*  Local variables:
      INTEGER MFIT
      INTEGER I
      INTEGER J
      INTEGER K
      INTEGER L
      INTEGER M
      REAL    DY
      REAL    SIG2I
      REAL    WT
      REAL    YMOD
      REAL    DYDA( MMAX )

*  Internal References:

*  Local data:

*.

      MFIT = 0
      DO J = 1, MA
         IF (IA(J) .NE. 0) THEN
            MFIT = MFIT + 1
         END IF
      END DO

*  initialise (symmetric) alpha, beta

      DO J = 1, MFIT
         DO K = 1, J
            ALPHA (J,K) = 0.0
         END DO
         BETA (J) = 0.0
      END DO

*  summation loop over all data

      CHISQ = 0.0
      DO I = 1, NDATA
         CALL FUNCS (X(I), A, YMOD, DYDA, MA)
         IF (SIG(I) .NE. 0.0) THEN
            SIG2I = 1.0 / (SIG(I) * SIG (I))
         ELSE
            STATUS = SAI__ERROR
            CALL ERR_OUT (' ', 'SCULIB_MRQCOF: zero noise on point',
     :        STATUS)
         END IF
         DY = Y(I) - YMOD
         J = 0
         DO L = 1, MA
            IF (IA(L) .NE. 0) THEN
               J = J + 1
               WT = DYDA (1) * SIG2I
               K = 0
               DO M = 1, L
                  IF (IA(M) .NE. 0) THEN
                     K = K + 1
                     ALPHA (J,K) = ALPHA (J,K) + WT * DYDA (M)
                  END IF
               END DO
               BETA (J) = BETA (J) + DY * WT
            END IF
         END DO
         CHISQ = CHISQ + DY * DY * SIG2I
      END DO

*  fill in the symmetric side

      DO J = 2, MFIT
         DO K = 1, J - 1
            ALPHA (K,J) = ALPHA (J,K)
         END DO
      END DO

      END
