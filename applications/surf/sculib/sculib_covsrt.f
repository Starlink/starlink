      SUBROUTINE SCULIB_COVSRT (COVAR, NPC, MA, IA, MFIT, STATUS)
*+
*  Name:
*     SCULIB_COVSRT

*  Purpose:
*     Numerical Recipes in Fortran routine called by 
*     SCULIB_MRQMIN

*  Description:
*     Expand in storage the covariance matrix COVAR, so as to take into account
*     parameters that are being held fixed. (For the latter return zero
*     covariances). Copied from COVSRT in Numerical Recipes in Fortran, p.669,
*     with STATUS added.

*  Invocation:
*     CALL SCULIB_COVSRT (COVAR, NPC, MA, IA, MFIT, STATUS)

*  Arguments:
*     parameter[(dimensions)]=type(access)
*           <description of parameter>

*  Method:

*  Deficiencies:

*  Bugs:

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     $Id$
*     13-SEP-1993: Original copy.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER MA, MFIT, NPC, IA (MA)

*  Arguments Given & Returned:
      REAL COVAR (NPC, NPC)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I, J, K
      REAL SWAP

*  Internal References:

*  Local data:

*.

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
