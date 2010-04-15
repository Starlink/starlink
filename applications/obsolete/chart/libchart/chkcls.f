      SUBROUTINE CHKCLS( STATUS )
*+
*  Name:
*     CHKCLS

*  Purpose:
*     Close any files that are still open

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHKCLS( STATUS )

*  Description:
*     Tidy up the CHART monolith by closing any files that are still
*     open.

*  Arguments:
*     STATUS = INTEGER (Returned)
*        The global status.

*  Algorithm:
*     -  Set the level of MSG output so that we can block unwanted
*        messages.
*     -  Close any files that are still open, possibly sending a
*        message about it to the user.

*  Copyright:
*     Copyright (C) 1993 Science & Engineering Research Council

*  Authors:
*     PMA: Peter Allan (Starlink, RAL)
*     {enter_new_authors_here}

*  History:
*     18-MAR-1993 (PMA):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'MSG_PAR'          ! MSG symbolic constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER U                  ! A FORTRAN I/O unit number
      LOGICAL OPENED             ! Is the unit open?

*.

*  Execute even if status is bad.

*  Set the filtering level.

      CALL MSG_IFSET( MSG__VERB, STATUS )

*  Close the files.

      DO U = 1, 4
         INQUIRE( UNIT=U, OPENED=OPENED )
         IF ( OPENED ) THEN
            CLOSE( UNIT=U )
            CALL MSG_SETI( 'UNIT', U )
            CALL MSG_OUTIF( MSG__VERB, ' ',
     :         'The file on unit ^UNIT has been closed by CHART',
     :          STATUS )
         END IF
      END DO

      U = 7
      INQUIRE( UNIT=U, OPENED=OPENED )
      IF ( OPENED ) THEN
         CLOSE( UNIT=U )
         CALL MSG_SETI( 'UNIT', U )
         CALL MSG_OUTIF( MSG__VERB, ' ',
     :      'The file on unit ^UNIT has been closed by CHART',
     :       STATUS )
      END IF

      DO U = 11, 13
         INQUIRE( UNIT=U, OPENED=OPENED )
         IF ( OPENED ) THEN
            CLOSE( UNIT=U )
            CALL MSG_SETI( 'UNIT', U )
            CALL MSG_OUTIF( MSG__VERB, ' ',
     :         'The file on unit ^UNIT has been closed by CHART',
     :          STATUS )
         END IF
      END DO

      END
