      DOUBLE PRECISION FUNCTION SCULIB_LST ()
*+
*  Name:
*     SCULIB_LST

*  Purpose:
*     returns LST in radians

*  Description:
*     Calculates the current LST in radians. 

*  Invocation:
*     LST = SCULIB_LST

*  Arguments:
*     None

*  Returned Value:
*     LST = DOUBLE PRECISION 
*         LST in radians

*  Bugs:

*  Authors:
*     J.Lightfoot (REVAD::JFL)

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  Method:

*  Deficiencies:
*     Not working yet.


*  History:
*     $Id$

*-


*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given:

*  Arguments Given & Returned:

*  Arguments Returned:

*  Status:

*  External references:
      DOUBLE PRECISION SLA_GMST        ! SLA routine to convert UT1 to
                                       !   mean ST at Greenwich
      DOUBLE PRECISION SLA_DRANRM      ! normalise angle to range 0 - 2pi
      DOUBLE PRECISION SCULIB_UT1      ! function to calculate UT1

*  Global variables:

*  Local Constants:
      DOUBLE PRECISION LONG_OBS_RAD
      PARAMETER       (LONG_OBS_RAD = 3.56955225444D0) ! longitude in radians

*  Local variables:
      DOUBLE PRECISION UT1

*  Internal References:

*  Local data:

*.

      UT1 = SCULIB_UT1 ()
      SCULIB_LST = SLA_GMST (UT1) + LONG_OBS_RAD
      SCULIB_LST = SLA_DRANRM (SCULIB_LST)

      END

