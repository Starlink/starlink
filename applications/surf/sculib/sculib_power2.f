      INTEGER FUNCTION SCULIB_POWER2 (N,NP2)
*+
*  Name:
*     SCULIB_POWER2

*  Purpose:
*     <brief title for subroutine>

*  Description:
*     Given a number, returns that number or the next highest
*     number that is a power of 2, and returns the power itself.
*     If N<1, GEN_POWER2 is returned as -1, and NP2 is returned as 0.

*  Invocation:
*     I = SCULIB_POWER2 (N, NP2)

*  Arguments:
*     N              = INTEGER (Given)
*             The number in question.
*     NP2            = INTEGER (Returned)
*             The next number that is a power of 2.
*     SCULIB_POWER2  = INTEGER (Returned)
*             The power of 2.  ie NP2=2**SCULIB_POWER2

*  Method:

*  Deficiencies:

*  Bugs:

*  Authors:
*     J.Lightfoot (REVAD::JFL), copied from GEN_POWER2 by K.Shortridge

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  History:
*     $Id$
*     1.12.1992: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      INTEGER N

*  Arguments Given & Returned:

*  Arguments Returned:
      INTEGER NP2

*  Status:

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I,IP2

*  Internal References:

*  Local data:

*.

      IF (N .LT. 1) THEN
         SCULIB_POWER2 = -1
         NP2 = 0
      ELSE
         I = 0
         IP2 = 1
         DO WHILE (IP2 .LT. N)
            I = I + 1
            IP2 = IP2 + IP2
         END DO
         SCULIB_POWER2 = I
         NP2 = IP2
      END IF

      END
