      SUBROUTINE SCULIB_ADDCAR (N, IN, RVAL, OUT)
*+
*  Name:
*     SCULIB_ADDCAR

*  Purpose:
*     add a constant to a real array

*  Description:
*     adds a real constant to a real array

*  Invocation:
*     CALL SCULIB_ADDCAR (N, IN, RVAL, OUT)

*  Arguments:
*     N              = INTEGER (Given)
*           number of elements in arrays
*     IN (N)         = REAL (Given)
*           input array
*     RVAL           = REAL (Given)
*           real constant to be added to array
*     OUT (N)        = REAL (Returned)
*           output array (may be same as input)

*  Method:

*  Notes:
*     - No range checks are performed
*     - No status checking
*     - No checks for bad values

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
         OUT (I) = IN (I) + RVAL
      END DO

      END
