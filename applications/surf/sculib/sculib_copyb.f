      SUBROUTINE SCULIB_COPYB (N, FROM, TO)
*+
*  Name:
*     SCULIB_COPYB

*  Purpose:
*     copy one byte array to another

*  Description:
*     copies one integer array into another

*  Invocation:
*     CALL SCULIB_COPYB (N, FROM, TO)

*  Arguments:
*     N          = INTEGER (Given)
*           number of integers in arrays
*     FROM (N)   = BYTE (Given)
*           array copied from
*     TO (N)     = BYTE (Returned)
*           array copied to

*  Authors:
*     J.Lightfoot (REVAD::JFL)
*     T. Jenness  (timj@jach.hawaii.edu)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Notes:
*     This routine is deprecated. Use VEC_BTOB (SUN/39) instead.


*  History:
*     $Id$
*     25.11.1992: Original version
*    endhistory


*  Method:

*  Deficiencies:

*  Bugs:


*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      INTEGER N
      BYTE FROM (N)

*  Arguments Given & Returned:

*  Arguments Returned:
      BYTE TO (N)

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
