      SUBROUTINE SCULIB_COVSRT (COVAR, NPC, MA, IA, MFIT, STATUS)
*+
*  Name:
*     SCULIB_COVSRT

*  Purpose:
*     Numerical Recipes in Fortran routine called by SCULIB_MRQMIN

*  Description:
*     Expand in storage the covariance matrix COVAR, so as to take into account
*     parameters that are being held fixed. (For the latter return zero
*     covariances). Derived from COVSRT in Numerical Recipes in Fortran, p.669,
*     with STATUS added.

*  Invocation:
*     CALL SCULIB_COVSRT (COVAR, NPC, MA, IA, MFIT, STATUS)

*  Arguments:
*     COVAR (  NPC, NPC ) = REAL (Given & Returned)
*        Covariance matrix
*     INTEGER NPC = INTEGER (Given)
*       Size of covariance matrix
*     INTEGER MA = INTEGER (Given)
*       Size of IA array. Total number of parameters.
*     INTEGER IA ( MA ) = INTEGER (Given)
*       Ordering of parameters. Not a free parameter if containes 0.
*     INTEGER MFIT = INTEGER (Given)
*       Number of required parameters
*     STATUS = INTEGER (Given & Returned)
*       Global status

*  Notes:
*     Derived from the 'Numerical Recipes in Fortran' COVSRT routine.

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1993,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.
*


*  Method:

*  Deficiencies:

*  Bugs:

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
      INTEGER MA
      INTEGER MFIT
      INTEGER NPC
      INTEGER IA (MA)

*  Arguments Given & Returned:
      REAL COVAR (NPC, NPC)

*  Arguments Returned:

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I                 ! Loop counter
      INTEGER J                 ! Loop counter
      INTEGER K                 ! Loop counter
      REAL    SWAP              ! Temp variable for swapping array entries

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
