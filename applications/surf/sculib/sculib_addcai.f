      SUBROUTINE SCULIB_ADDCAI (N, IN, IVAL, OUT)
*+
*  Name:
*     SCULIB_ADDCAI

*  Purpose:
*     add a constant to an integer array

*  Description:
*     adds an integer constant to an integer array

*  Invocation:
*     CALL SCULIB_ADDCAI (N, IN, IVAL, OUT)

*  Arguments:
*     N              = INTEGER (Given)
*           number of elements in arrays
*     IN (N)         = INTEGER (Given)
*           input array
*     IVAL           = INTEGER (Given)
*           constant to be added to array
*     OUT (N)        = INTEGER (Returned)
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
*     9-JUN-1993: Original version
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      INTEGER N
      INTEGER IN (N)
      INTEGER IVAL

*  Arguments Given & Returned:

*  Arguments Returned:
      INTEGER OUT (N)

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
         OUT (I) = IN (I) + IVAL
      END DO

      END
