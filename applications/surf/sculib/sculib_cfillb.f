      SUBROUTINE SCULIB_CFILLB (N, IVAL, ARRAY)
*+
*  Name:
*     SCULIB_CFILLB

*  Purpose:
*     fill a byte array with a constant

*  Description:
*     fills an byte array with a constant

*  Invocation:
*     CALL SCULIB_CFILLB (N, IVAL, ARRAY)

*  Arguments:
*     N                  = INTEGER (Given)
*           number of array elements
*     IVAL               = BYTE (Given)
*           constant to which array is to be set
*     ARRAY (N)          = BYTE (Returned)
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
      BYTE IVAL

*  Arguments Given & Returned:

*  Arguments Returned:
      BYTE ARRAY (N)

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
