      SUBROUTINE CAP_PRNG (PTS, ARRAY, MINVAL, MAXVAL, STATUS)
*+
*  Name:
*     CAP_PRNG
*  Purpose:
*     Determine the minimum and maximum values of an array.
*  Language:
*     Fortran 77.
*  Invocation:
*     CALL CAP_PRNG (PTS, ARRAY; MINVAL, MAXVAL; STATUS)
*  Description:
*     Determine the minimum and maximum values of an array.
*  Arguments:
*     PTS  =  INTEGER (Given)
*        Number of points in the array.
*     ARRAY(PTS)  =  REAL (Given)
*        Array of values.
*     MINVAL  =  REAL (Returned)
*        Minimum value in the array.
*     MAXVAL  =  REAL (Returned)
*        Maximum value in the array.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.
*  Algorithm:
*     Examine all the points in the array and find the minimum and
*     maximum values.
*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils
*  Authors:
*     ACD: A C Davenhall (Edinburgh)
*  History:
*     10/7/98 (ACD): Original version.
*  Bugs:
*     None known
*-
*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard Starlink constants.
C      INCLUDE 'CAT_PAR'           ! CAT parametric constants.
C      INCLUDE 'CAT_ERR'           ! CAT error codes.
C      INCLUDE 'SGZ_PAR'           ! StarGaze parametric constants.
*  Global Variables:
C      INCLUDE 'SGZ_CMN'           ! StarGaze common block.
*  Arguments Given:
      INTEGER
     :  PTS
      REAL
     :  ARRAY(PTS)
*  Arguments Returned:
      REAL
     :  MINVAL,
     :  MAXVAL
*  Status:
      INTEGER STATUS    ! Global status.
*  Local Variables:
      INTEGER
     :  LOOP            ! Loop index.
*.

      IF (STATUS .EQ. SAI__OK) THEN

*
*       Examine all the points in the array and find the minimum and
*       maximum values.

         MINVAL = ARRAY(1)
         MAXVAL = ARRAY(1)

         DO LOOP = 2, PTS
            IF (ARRAY(LOOP) .LT. MINVAL) THEN
               MINVAL = ARRAY(LOOP)
            END IF

            IF (ARRAY(LOOP) .GT. MAXVAL) THEN
               MAXVAL = ARRAY(LOOP)
            END IF
         END DO

      END IF

      END
