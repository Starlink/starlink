      SUBROUTINE CONVDEC( CHARS, J, K, DEC, STATUS )
*+
*  Name:
*     CONVDEC

*  Purpose:
*     Read in a declination

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CONVDEC( CHARS, J, K, DEC, STATUS )

*  Description:
*     Convert a character string in sexagesimal format to a
*     declination expressed in radians.

*  Arguments:
*     CHARS = CHARACTER * ( * ) (Given)
*        String holding the declination
*     J = INTEGER (Given)
*        First character position to be used in CHARS
*     K = INTEGER (Given)
*        Last character position to be used in CHARS
*     DEC = DOUBLE PRECISION (Returned)
*        Numerical value of the declination in radians
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     -  {algorithmic_step}
*     [algorithm_description]...

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     ANO: Someone (Somewhere)
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     Sometime (PMA):
*        Original version.
*     8-FEB-1993 (PMA):
*        Change to handle Starlink style error reporting.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CHT_ERR'          ! CHART error constants

*  Global Variables:
      INCLUDE 'CONVF'            ! Constants used in conversions
*        RDSA = DOUBLE PRECISION (Read)
*           Radians per second of arc

*  Arguments Given:
      CHARACTER * ( * ) CHARS
      INTEGER J
      INTEGER K

*  Arguments Returned:
      DOUBLE PRECISION DEC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 40 ) STRING  ! [local_variable_description]
      DOUBLE PRECISION ANGLE     ! [local_variable_description]

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Extract the substring from the input argument.
      STRING = CHARS( J:K )

*  Convert it from sexagesimal format to a double precision value in
*  degrees.
      CALL CONVPOS( STRING, ANGLE, STATUS )
      IF ( STATUS .EQ. CHT__IVSEX ) THEN
         CALL ERR_REP( 'CONVDEC_SEX',
     :      'Declination is in Degrees, Minutes and Seconds', STATUS )
      ELSE

*  Convert the angle to radians.
         DEC  = ANGLE * 3.6D3 * RDSA
         IF ( ABS( DEC ) .GT. HALFPI ) THEN
**          IERR = 3
            STATUS = CHT__DECBG
            CALL ERR_REP( 'CONVDEC_TOOBIG',
     :         'Dec must be no more than 90 Degrees', STATUS )
         END IF
      END IF

      END
