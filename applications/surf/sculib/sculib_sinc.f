      REAL FUNCTION SCULIB_SINC( X )
*+
*  Name:
*     SCULIB_SINC

*  Purpose:
*     Calculate SIN(PI*X)/(PI*X)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SCULIB_SINC( X )

*  Description:
*     Calculate sinc function.

*  Arguments:
*     X = REAL (Given)
*        The argument to the sinc function - In turns

*  Returned Value:
*     SCULIB_SINC = REAL
*        SINC(PI*X)

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  Implementation Status:
*     - No status checking
*     - No bad value checking

*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.4  1999/08/19 03:46:45  timj
*     Modify actual code to reflect change of name from SINC to SCULIB_SINC!!
*
*     Revision 1.3  1999/08/06 01:48:28  timj
*     Rename SINC to SCULIB_SINC
*
*     19-JAN-1990 (JBVAD::PAH):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:

*  Arguments Given:
      REAL X

*  Local Constants:
      REAL SPI                   ! PI
      PARAMETER (SPI = 3.14159265)

*  Local Variables:
      REAL Y                     ! X*PI

*.

      IF ( X.EQ.0 ) THEN
         SCULIB_SINC=1.0
      ELSE
         Y=SPI*X
         SCULIB_SINC=SIN(Y)/Y
      END IF

      END

