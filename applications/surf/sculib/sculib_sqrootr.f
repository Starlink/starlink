      SUBROUTINE SCULIB_SQROOTR (N, IN_DATA, IN_QUAL, ROOT, ROOT_QUAL,
     :   QUALITY, FLAGGED)
*+
*  Name:
*     SCULIB_SQROOTR

*  Purpose:
*     take the square root of a real array

*  Description:
*     Puts square root of real input array into output array. If FLAGGED and
*     QUALITY are both false then negative input numbers give a zero output.
*     If FLAGGED is true then input bad values, or negative input numbers,
*     give a bad output. If QUALITY is true then input values with bad
*     quality, or negative input numbers, lead to zero output data and bad
*     output quality.

*  Invocation:
*     CALL SCULIB_SQROOTR (N, IN_DATA, IN_QUAL, ROOT, ROOT_QUAL, QUALITY,
*    :   FLAGGED)

*  Arguments:
*     N                       = INTEGER (Given)
*           the number of array elements
*     IN_DATA (N)             = REAL (Given)
*           the array whose root is to be taken
*     IN_QUAL (N)             = INTEGER (Given)
*           the quality on the input data
*     ROOT (N)                = REAL (Returned)
*           the array to hold the square roots (may be the same as IN_DATA)
*     ROOT_QUAL (N)           = INTEGER (Returned)
*           the quality on the output root
*     QUALITY                 = LOGICAL (Given)
*           .TRUE. if input quality array exists
*     FLAGGED                 = LOGICAL (Given)
*           .TRUE. if input data has flagged values

*  Notes:
*     Consider using VEC_SQRTR (SUN/39) instead.

*  Implementation Status:
*     Uses INTEGER rather than BYTE Quality.

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     $Id$
*     26-OCT-1994: Checked.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'                  ! for VAL__BADR

*  Arguments Given:
      INTEGER N
      REAL IN_DATA (N)
      INTEGER IN_QUAL (N)
      LOGICAL QUALITY
      LOGICAL FLAGGED

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL ROOT (N)
      INTEGER ROOT_QUAL (N)

*  Status:

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I

*  Internal References:

*  Local data:

*.

      IF (QUALITY) THEN

         DO I = 1, N
            IF (IN_QUAL(I) .NE. 0) THEN
               ROOT (I) = 0.0
               ROOT_QUAL (I) = IN_QUAL(I)
            ELSE
               IF (IN_DATA (I) .LT. 0.0) THEN
                  ROOT (I) = 0.0
                  ROOT_QUAL (I) = 1
               ELSE
                  ROOT (I) = SQRT (IN_DATA(I))
                  ROOT_QUAL (I) = 0
               END IF
            END IF
         END DO

      ELSE IF (FLAGGED) THEN

         DO I = 1, N
            IF (IN_DATA(I) .EQ. VAL__BADR) THEN
               ROOT (I) = VAL__BADR
            ELSE
               IF (IN_DATA (I) .LT. 0.0) THEN
                  ROOT (I) = VAL__BADR
               ELSE
                  ROOT (I) = SQRT (IN_DATA(I))
               END IF
            END IF
         END DO

      ELSE

         DO I = 1, N
            IF (IN_DATA (I) .LT. 0.0) THEN
               ROOT (I) = 0.0
            ELSE
               ROOT (I) = SQRT (IN_DATA(I))
            END IF
         END DO

      END IF

      END
