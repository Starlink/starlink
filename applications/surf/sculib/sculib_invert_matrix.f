      SUBROUTINE SCULIB_INVERT_MATRIX (M, ARRAY, DET, IK, JK, STATUS)
*+
*  Name:
*     SCULIB_INVERT_MATRIX

*  Purpose:
*     invert a square matrix

*  Description:
*     This routine inverts a matrix by a method opaque to casual inspection
*     but which has the advantage that it doesn't require any more space
*     than the input matrix itself provides. The method is described in
*     `Data Reduction and Error Analysis for the Physical Sciences' by
*     Bevington and Robinson, and the code is adapted from the Pascal
*     version listed there.

*  Invocation:
*     CALL SCULIB_INVERT_MATRIX (M, ARRAY, DET, IK, JK, STATUS)

*  Arguments:
*     M                      = INTEGER (Given)
*           the dimensions of the matrix
*     ARRAY (M,M)            = DOUBLE PRECISION (Given and returned)
*           the matrix to be inverted
*     DET                    = DOUBLE PRECISION (Returned)
*           the determinant of the matrix
*     IK (M)                 = INTEGER (Scratch)
*           i indices of largest array elements
*     JK (M)                 = INTEGER (Scratch)
*           j indices of largest array elements
*     STATUS                 = INTEGER (Given and returned)
*           global status


*  Authors:
*     J.Lightfoot (JFL@ROE.AC.UK)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     29-MAR-1995: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER M

*  Arguments Given & Returned:
      DOUBLE PRECISION ARRAY (M,M)

*  Arguments Returned:
      DOUBLE PRECISION DET
      INTEGER IK (M)
      INTEGER JK (M)

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      DOUBLE PRECISION AMAX                  ! size of largest matrix element
      INTEGER          I                     ! DO loop index
      INTEGER          J                     ! DO loop index
      INTEGER          K                     ! DO loop index
      INTEGER          L                     ! DO loop index
      DOUBLE PRECISION SAVE                  ! scratch real

*  Internal References:

*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DET = 0.0D0

      DO K = 1, M

*  find the largest element

         AMAX = 0.0D0

 100     CONTINUE

         DO I = K, M
            DO J = K, M
               IF (ABS(ARRAY(I,J)) .GT. ABS(AMAX)) THEN
                  AMAX = ARRAY(I,J)
                  IK(K) = I
                  JK(K) = J
               END IF
            END DO
         END DO

         IF (AMAX .EQ. 0.0D0) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'SCULIB_INVERT_MATRIX: matrix is '//
     :        'singular', STATUS)
            RETURN
         END IF

         DET = 1.0D0

*  interchange rows and columns to put AMAX in ARRAY(K,K)

         I = IK(K)

         IF (I .LT. K) THEN
            GOTO 100
         ELSE IF (I .GT. K) THEN
            DO J = 1, M
               SAVE = ARRAY(K,J)
               ARRAY(K,J) = ARRAY(I,J)
               ARRAY(I,J) = -SAVE
            END DO
         END IF

         J = JK(K)

         IF (J .LT. K) THEN
            GOTO 100
         ELSE IF (J .GT. K) THEN
            DO I = 1, M
               SAVE = ARRAY(I,K)
               ARRAY(I,K) = ARRAY(I,J)
               ARRAY(I,J) = -SAVE
            END DO
         END IF

*  accumulate elements of inverse matrix

         DO I = 1, M
            IF (I .NE. K) THEN
               ARRAY(I,K) = -ARRAY(I,K) / AMAX
            END IF
         END DO

         DO I = 1, M
            DO J = 1, M
               IF ((I .NE. K) .AND. (J .NE. K)) THEN
                  ARRAY(I,J) = ARRAY(I,J) + ARRAY(I,K) * ARRAY(K,J)
               END IF
            END DO
         END DO

         DO J = 1, M
            IF (J .NE. K) THEN
               ARRAY(K,J) = ARRAY(K,J) / AMAX
            END IF
         END DO

         ARRAY(K,K) = 1 / AMAX
         DET = DET * AMAX
      END DO

*  restore ordering of matrix

      DO L = 1, M
         K = M + 1 - L
         J = IK(K)

         IF (J .GT. K) THEN
            DO I = 1, M
               SAVE = ARRAY(I,K)
               ARRAY(I,K) = -ARRAY(I,J)
               ARRAY(I,J) = SAVE
            END DO
         END IF

         I = JK(K)

         IF (I .GT. K) THEN
            DO J = 1, M
               SAVE = ARRAY(K,J)
               ARRAY(K,J) = -ARRAY(I,J)
               ARRAY(I,J) = SAVE
            END DO
         END IF
      END DO

      END
