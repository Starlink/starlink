      SUBROUTINE SCULIB_SUMAD (NELM, ARRAY, SUM)
*+
*  Name:
*     SCULIB_SUMAD

*  Purpose:
*     sum the elements of a double precision array

*  Description:
*     This routine adds up the NELM elements of the double precision ARRAY
*     and puts the result in SUM.

*  Invocation:
*     CALL SCULIB_SUMAD (NELM, ARRAY, SUM)

*  Arguments:
*     NELM               = INTEGER (Given)
*           the number of elements in the array
*     ARRAY (NELM)       = DOUBLE PRECISION (Given)
*           the array whose elements are to be summed
*     SUM                = DOUBLE PRECISION (Returned)
*            the sum


*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Implementation Status:
*     No status checking

*  Method:

*  Deficiencies:

*  Bugs:


*  History:
*     $Id$
*     24-SEP-1993: Original version.
*    endhistory

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:
      INTEGER NELM
      DOUBLE PRECISION ARRAY (NELM)

*  Arguments Given & Returned:

*  Arguments Returned:
      DOUBLE PRECISION SUM

*  Status:

*  External references:

*  Global variables:

*  Local Constants:

*  Local variables:
      INTEGER I

*  Internal References:

*  Local data:

*.

      SUM = 0.0D0

      DO I = 1, NELM
         SUM = SUM + ARRAY (I)
      END DO

      END
