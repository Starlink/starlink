      SUBROUTINE SCULIB_NFILLR (N, ARRAY)
*+
*  Name:
*     SCULIB_NFILLR

*  Purpose:
*     fill a real array with its indices

*  Description:
*     fills a real array with its indices

*  Invocation:
*     CALL SCULIB_NFILLR (N, ARRAY)

*  Arguments:
*     N            = INTEGER (Given)
*           number of array elements
*     ARRAY (N)    = REAL (Returned)
*           array to be filled

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
*     25.11.1992: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:

*  Arguments Given:
      INTEGER N

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
         ARRAY (I) = REAL (I)
      END DO

      END
