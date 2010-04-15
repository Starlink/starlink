      SUBROUTINE CONVRA( CHARS, J, K, RA, STATUS )
*+
*  Name:
*     CONVRA

*  Purpose:
*     Read in a Right Ascension value

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CONVRA( CHARS, J, K, RA, STATUS )

*  Description:
*     Convert a character string in sexagesimal format to a right
*     ascension expressed in radians.

*  Arguments:
*     CHARS = CHARACTER * ( * ) (Given)
*        String holding the rights ascension
*     J = INTEGER (Given)
*        First character position to be used in CHARS
*     K = INTEGER (Given)
*        Last character position to be used in CHARS
*     RA = DOUBLE PRECISION (Returned)
*        Numerical value of the right ascension in radians
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
*     Sometime (ANO):
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
*        RDST = DOUBLE PRECISION (Read)
*           Radians per second of time

*  Arguments Given:
      CHARACTER * ( * ) CHARS
      INTEGER J
      INTEGER K

*  Arguments Returned:
      DOUBLE PRECISION RA

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
*  hours.
      CALL CONVPOS( STRING, ANGLE, STATUS )
      IF ( STATUS .EQ.CHT__IVSEX ) THEN
         CALL ERR_REP( 'CONVRA_SEX',
     :      'RA is in Hours, Minutes and Seconds', STATUS )
      ELSE

*  Convert the angle to radians.
         RA = ANGLE * 3.6D3 * RDST
         IF ( RA .LT. 0.0 .OR. RA .GT. TWOPI ) THEN
**          IERR = 3
            STATUS = CHT__RABIG
            CALL ERR_REP( 'CONVRA_TOOBIG',
     :         'RA must be between 0 and 24 hours', STATUS )
         END IF
      END IF
      END
