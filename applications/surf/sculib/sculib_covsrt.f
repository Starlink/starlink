*+  SCULIB_COVSRT - Numerical Recipes in Fortran routine called by 
*                   SCULIB_MRQMIN
      SUBROUTINE SCULIB_COVSRT (COVAR, NPC, MA, IA, MFIT, STATUS)
*    Description :
*     Expand in storage the covariance matrix COVAR, so as to take into account
*     parameters that are being held fixed. (For the latter return zero
*     covariances). Copied from COVSRT in Numerical Recipes in Fortran, p.669,
*     with STATUS added.
*    Invocation :
*     CALL SCULIB_COVSRT (COVAR, NPC, MA, IA, MFIT, STATUS)
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
*     13-SEP-1993: Original copy.
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER MA, MFIT, NPC, IA (MA)
*    Import-Export :
      REAL COVAR (NPC, NPC)
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
*    Global variables :
*    Local Constants :
*    Local variables :
      INTEGER I, J, K
      REAL SWAP
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

      DO I = MFIT + 1, MA
         DO J = 1, I
            COVAR (I,J) = 0.0
            COVAR (J,I) = 0.0
         END DO
      END DO

      K = MFIT

      DO J = MA, 1, -1
         IF (IA(J) .NE. 0) THEN
            DO I = 1, MA
               SWAP = COVAR (I,K)
               COVAR (I,K) = COVAR (I,J)
               COVAR (I,J) = SWAP
            END DO
            DO I = 1, MA
               SWAP = COVAR (K,I)
               COVAR (K,I) = COVAR (J,I)
               COVAR (J,I) = SWAP
            END DO
            K = K - 1
         END IF
      END DO

      END
