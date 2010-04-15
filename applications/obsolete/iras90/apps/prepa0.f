      SUBROUTINE PREPA0( PARAM, SIZE, TEXT, IGRP, STATUS )
*+
*  Name:
*     PREPA0

*  Purpose:
*     Get a group of character strings.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPA0( PARAM, SIZE, TEXT, IGRP, STATUS )

*  Description:
*     A case sensitive group is obtained from the environment using
*     the supplied parameter. The user si re-prompted until the group
*     holds exactly the specified number of elements.

*  Arguments:
*     PARAM = CHARACTER * ( * )  (Given)
*        The parameter to use.
*     SIZE = INTEGER (Given)
*        The number of elements required in the returned group.
*     TEXT = CHARACTER * ( * ) (Given)
*        Text to display when more strings are required from the user.
*     IGRP = INTEGER (Returned)
*        The group identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'PAR_ERR'          ! PAR_ error constants.
      INCLUDE 'GRP_PAR'          ! GRP_ constants.

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER SIZE
      CHARACTER TEXT*(*)

*  Arguments Returned:
      INTEGER IGRP

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER GRPEXP*(GRP__SZGEX)! Group expression.

      INTEGER ADDED              ! No. of values added to the group.
      INTEGER NBAD               ! No. of bad groups given.
      INTEGER SIZE0              ! Current size of supplied group.

      LOGICAL FLAG               ! True if further values are to be
                                 ! added to the group.

*.

*  Ensure a value of GRP__NOID is returned for the group identifier if
*  an error exists on entry.
      IGRP = GRP__NOID

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the number of bad groups given to zero.
      NBAD = 0

*  Create a group to hold the strings.
      CALL GRP_NEW( ' ', IGRP, STATUS )

*  Ensure that the group is case sensitive.
      CALL GRP_SETCS( IGRP, .TRUE., STATUS )

*  Get an initial group expression from the environment.
  10  CONTINUE
      CALL PAR_GET0C( PARAM, GRPEXP, STATUS )

*  If a null value has been supplied, annul the error, delete the group
*  and return.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL GRP_DELET( IGRP, STATUS )
         GO TO 999
      END IF

*  Store the strings specified by the first group expression.
      CALL GRP_GRPEX( GRPEXP, GRP__NOID, IGRP, SIZE0, ADDED, FLAG,
     :                STATUS )

*  Loop round obtaining strings to add to the group until a group
*  expression is given which is not flagged (i.e. which does not end
*  with a minus sign).
      DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )

         CALL MSG_OUTIF( MSG__NORM, 'PREPA0_MSG1', TEXT, STATUS )
         CALL PAR_CANCL( PARAM, STATUS )
         CALL PAR_GET0C( PARAM, GRPEXP, STATUS )
         CALL GRP_GRPEX( GRPEXP, GRP__NOID, IGRP, SIZE0, ADDED, FLAG,
     :                   STATUS )

      END DO

*  The user may have indicated the end of the group by giving a null
*  value for the parameter. This would normally cause the application to
*  abort, so annul the error in order to prevent this from happening.
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  If too few or too many strings have been given report an error.
      IF( SIZE0 .LT. SIZE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR

         CALL MSG_SETI( 'S', SIZE )
         CALL MSG_SETI( 'S0', SIZE0 )

         CALL ERR_REP( 'PREPA0_ERR1',
     :                 'PREPA0: Only ^S0 values given (^S required).',
     :                 STATUS )

      ELSE IF( SIZE0 .GT. SIZE .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR

         CALL MSG_SETI( 'S', SIZE )
         CALL MSG_SETI( 'S0', SIZE0 )

         CALL ERR_REP( 'PREPA0_ERR2',
     :                'PREPA0: ^S0 values given (only ^S can be used).',
     :                 STATUS )

      END IF

*  If an error has occurred (other than a parameter request abort), add
*  another report asking the user to supply a new group, flush the
*  error status, cancel the parameter value, set the group size to zero
*  and return for a new group.
      IF( STATUS .NE. SAI__OK .AND. STATUS .NE. PAR__ABORT .AND.
     :    NBAD .LE. 4 ) THEN
         CALL ERR_REP( 'PREPA0_ERR3',
     :                 'PREPA0: Please give a new group of values.',
     :                 STATUS )
         CALL ERR_FLUSH( STATUS )
         CALL PAR_CANCL( PARAM, STATUS )
         CALL GRP_SETSZ( IGRP, 0, STATUS )
         NBAD = NBAD + 1
         GO TO 10
      END IF

*  Finish
 999  CONTINUE

      END
