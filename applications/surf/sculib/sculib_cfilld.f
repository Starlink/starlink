      SUBROUTINE SCULIB_CFILLD (N, DVAL, ARRAY)
*+
*  Name:
*     SCULIB_CFILLD

*  Purpose:
*     fill a double precision array with a constant

*  Description:
*     fills a double precision array with a constant

*  Invocation:
*     CALL SCULIB_CFILLD (N, DVAL, ARRAY)

*  Arguments:
*     N                  = INTEGER (Given)
*           number of array elements
*     DVAL               = DOUBLE PRECISION (Given)
*           constant to which array is to be set
*     ARRAY (N)          = DOUBLE PRECISION (Returned)
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
      DOUBLE PRECISION DVAL

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION ARRAY (N)

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
         ARRAY (I) = DVAL
      END DO

      END
