*+  SCULIB_MRQCOF - called by SCULIB_MRQMIN
      SUBROUTINE SCULIB_MRQCOF (X, Y, SIG, NDATA, A, IA, MA, ALPHA,
     :  BETA, NALP, CHISQ, FUNCS, STATUS)
*    Description :
*     Utility routine called by SCULIB_MRQMIN, copied from Numerical
*     Recipes in Fortran, p. 681. Used by SCULIB_MRQMIN to evaluate the
*     linearised fitting matrix ALPHA, and vector BETA, and calculate
*     chi-squared.
*        Copied from MRQCOF on p.681 of Numerical Recipes in Fortran, with
*     STATUS added.
*    Invocation :
*     CALL SCULIB_MRQCOF (X, Y, SIG, NDATA, A, IA, MA, ALPHA,
*    :  BETA, NALP, CHISQ, FUNCS, STATUS)
*    Parameters :
*     parameter[(dimensions)]=type(access)
*           <description of parameter>
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (REVAD::JFL)
*    History :
*     $Id$
*     13-SEP-1993: Original copy (REVAD::JFL)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER MA, NALP, NDATA, IA(MA)
      REAL A(MA), SIG(NDATA), X(NDATA), Y(NDATA)
*    Import-Export :
*    Export :
      REAL CHISQ, ALPHA(NALP,NALP), BETA(MA)
*    Status :
      INTEGER STATUS
*    External references :
      EXTERNAL FUNCS
*    Global variables :
*    Local Constants :
      INTEGER MMAX
      PARAMETER (MMAX = 20)
*    Local variables :
      INTEGER MFIT, I, J, K, L, M
      REAL DY, SIG2I, WT, YMOD, DYDA(MMAX)
*    Internal References :
*    Local data :
*-

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
