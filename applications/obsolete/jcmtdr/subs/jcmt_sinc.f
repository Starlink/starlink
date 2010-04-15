      REAL FUNCTION SINC( X )
*+
*  Name:
*     SINC

*  Purpose:
*     Calculate SIN(PI*X)/(PI*X)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = SINC( X )

*  Description:
*     {routine_description}

*  Arguments:
*     X = REAL (Given)
*        The argument to the sinc function - In turns

*  Returned Value:
*     SINC = REAL
*        SINC(PI*X)

*  [optional_function_items]...
*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-JAN-1990 (JBVAD::PAH):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'ASTRO_PAR'        ! astronomical constants

*  Arguments Given:
      REAL X

*  Local Variables:
      REAL Y                     ! X*PI

*.

      IF ( X.EQ.0 ) THEN
         SINC=1.0
      ELSE
         Y=SPI*X
         SINC=SIN(Y)/Y
      END IF

      END

