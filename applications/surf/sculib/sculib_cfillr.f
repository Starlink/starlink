      SUBROUTINE SCULIB_CFILLR (N, RVAL, ARRAY)
*+
*  Name:
*     SCULIB_CFILLR

*  Purpose:
*     fill a real array with a constant

*  Description:
*     fills a real array with a constant

*  Invocation:
*     CALL SCULIB_CFILLR (N, RVAL, ARRAY)

*  Arguments:
*     N                  = INTEGER (Given)
*           number of array elements
*     RVAL               = REAL (Given)
*           constant to which array is to be set
*     ARRAY (N)          = REAL (Returned)
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
      REAL RVAL

*  Arguments Given & Returned:

*  Arguments Returned:
      REAL ARRAY (N)

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
         ARRAY (I) = RVAL
      END DO

      END
