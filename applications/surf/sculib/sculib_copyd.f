      SUBROUTINE SCULIB_COPYD (N, FROM, TO)
*+
*  Name:
*     SCULIB_COPYD

*  Purpose:
*     copy one double precision array to another

*  Description:
*     copies one double precision array into another

*  Invocation:
*     CALL SCULIB_COPYD (N, FROM, TO)

*  Arguments:
*     N          = INTEGER (Given)
*           number of elements in arrays
*     FROM (N)   = DOUBLE PRECISION (Given)
*           array copied from
*     TO (N)     = DOUBLE PRECISION (Returned)
*           array copied to

*  Notes:
*     This routine is deprecated. Use VEC_DTOD (SUN/39) instead.

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:

*  Bugs:


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
      DOUBLE PRECISION FROM (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION TO (N)

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
