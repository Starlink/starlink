      SUBROUTINE SCULIB_MULCAR (N, IN, RVAL, OUT)
*+
*  Name:
*     SCULIB_MULCAR

*  Purpose:
*     multiply real array by a constant

*  Description:
*     multiplies a real array by a real constant

*  Invocation:
*     CALL SCULIB_MULCAR (N, IN, RVAL, OUT)

*  Arguments:
*     N              = INTEGER (Given)
*           number of array elements
*     IN (N)         = REAL (Given)
*           array to be multiplied
*     RVAL           = REAL (Given)
*           multiplication factor
*     OUT (N)        = REAL (Returned)
*           output array (can be same as input)

*  Authors:
*     J.Lightfoot (REVAD::JFL)
*     T.Jenness   (JACH)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     25.11.1992: Original version
*     05.12.1996: Check for bad values
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER N
      REAL IN (N)
      REAL RVAL

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL OUT (N)

*  Status:

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I

*  Internal References:

*  Local data:

*.

      DO I = 1, N
         IF (IN(I) .NE. VAL__BADR) THEN
            OUT (I) = IN (I) * RVAL
         ELSE
            OUT(I) = IN(I)
         END IF
      END DO

      END
