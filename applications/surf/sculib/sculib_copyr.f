      SUBROUTINE SCULIB_COPYR (N, FROM, TO)
*+
*  Name:
*     SCULIB_COPYR

*  Purpose:
*     copy one real array to another

*  Description:
*     copies one real array into another

*  Invocation:
*     CALL SCULIB_COPYR (N, FROM, TO)

*  Arguments:
*     N          = INTEGER (Given)
*           number of reals in arrays
*     FROM (N)   = REAL (Given)
*           array copied from
*     TO (N)     = REAL (Returned)
*           array copied to

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
*     25.11.1992: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      INTEGER N
      REAL FROM (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL TO (N)

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
         TO (I) = FROM (I)
      END DO

      END
