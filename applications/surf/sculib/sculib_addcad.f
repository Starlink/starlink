      SUBROUTINE SCULIB_ADDCAD (N, IN, DVAL, OUT)
*+
*  Name:
*     SCULIB_ADDCAD

*  Purpose:
*     add a constant to a double array

*  Description:
*     adds a real constant to a double array

*  Invocation:
*     CALL SCULIB_ADDCAD (N, IN, RVAL, OUT)

*  Arguments:
*     N              = INTEGER (Given)
*           number of elements in arrays
*     IN (N)         = DOUBLE PRECISION (Given)
*           input array
*     RVAL           = DOUBLE PRECISION (Given)
*           real constant to be added to array
*     OUT (N)        = DOUBLE PRECISION (Returned)
*           output array (may be same as input)

*  Notes:
*     - No range checks are performed
*     - No status checking
*     - No checks for bad values

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.


*  Method:

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
      DOUBLE PRECISION IN (N)
      DOUBLE PRECISION DVAL

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION OUT (N)

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
         OUT (I) = IN (I) + DVAL
      END DO

      END
