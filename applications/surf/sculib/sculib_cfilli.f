      SUBROUTINE SCULIB_CFILLI (N, IVAL, ARRAY)
*+
*  Name:
*     SCULIB_CFILLI

*  Purpose:
*     fill an integer array with a constant

*  Description:
*     fills an integer array with a constant

*  Invocation:
*     CALL SCULIB_CFILLI (N, IVAL, ARRAY)

*  Arguments:
*     N                  = INTEGER (Given)
*           number of array elements
*     IVAL               = INTEGER (Given)
*           constant to which array is to be set
*     ARRAY (N)          = INTEGER (Returned)
*           array to be set

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
      INTEGER IVAL

*  Arguments Given & Returned:

*  Arguments Returned:
      INTEGER ARRAY (N)

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
         ARRAY (I) = IVAL
      END DO

      END
