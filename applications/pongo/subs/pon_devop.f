      LOGICAL FUNCTION PON_DEVOP( ISERR, STATUS )
*+
* Name:
*    PON_DEVOP

*  Purpose:
*     Checks if a PGPLOT device is opened.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PON_DEVOP( ISERR, STATUS )

*  Description:
*     This routine checks if PGPLOT has been opened or not. If not then
*     the result is .FALSE. Also if ISERR is .TRUE. then the global
*     status is set bad and an error is reported. If the PGPLOT is
*     opened then the return is .TRUE.

*  Arguments:
*     ISERR = LOGICAL (Given)
*        Whether or not it is a serious error for PGPLOT not to be
*        opened.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Returned Value:
*     PON_DEVOP = LOGICAL
*        .TRUE. if PGPLOT is open, .FALSE. otherwise.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1994 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      LOGICAL ISERR

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 6 ) STATE    ! State of PGPLOT device
      INTEGER LEN                ! Length of state string

*.

*  Default mode is not opened.
      PON_DEVOP = .FALSE.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ask PGPLOT about its state.
      CALL PGQINF( 'STATE', STATE, LEN )
      IF ( LEN .NE. 4 ) THEN
         IF ( ISERR ) THEN

*  It's a serious problem.
            STATUS = SAI__ERROR
            CALL ERR_REP( 'DEVNOTOPN',
     :           'Device is not open -- use BEGPLOT', STATUS )
         END IF
      ELSE
         PON_DEVOP = .TRUE.
      END IF
      END
* $Id$
