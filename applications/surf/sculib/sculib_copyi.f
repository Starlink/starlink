      SUBROUTINE SCULIB_COPYI (N, FROM, TO)
*+
*  Name:
*     SCULIB_COPYI

*  Purpose:
*     copy one integer array to another

*  Description:
*     copies one integer array into another

*  Invocation:
*     CALL SCULIB_COPYI (N, FROM, TO)

*  Arguments:
*     N          = INTEGER (Given)
*           number of integers in arrays
*     FROM (N)   = INTEGER (Given)
*           array copied from
*     TO (N)     = INTEGER (Returned)
*           array copied to

*  Notes:
*     This routine is deprecated. Use VEC_ITOI (SUN/39) instead.

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
      INTEGER FROM (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      INTEGER TO (N)

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
