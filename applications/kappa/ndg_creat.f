      SUBROUTINE NDG_CREAT( PARAM, IGRP0, IGRP, SIZE, FLAG, STATUS )
*+
*  Name:
*     NDG_CREAT

*  Purpose:
*     Obtain the names of a group of NDF to be created from the
*     environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_CREAT( PARAM, IGRP0, IGRP, SIZE, FLAG, STATUS )

*  Description:
*     A group expression is obtained from the environment using the
*     supplied parameter. The expression is parsed (using the
*     facilities of the GRP routine GRP_GROUP, see SUN/150) to produce
*     a list of explicit NDF names. These names are appended
*     to the group identified by IGRP. The user is re-prompted if an 
*     error occurs while parsing the group expression. If IGRP has the 
*     value GRP__NOID on entry, then a new group is created and IGRP is 
*     returned holding the new group identifier.
*
*     If IGRP0 holds a valid group identifier on entry, then the group
*     identified by IGRP0 is used as the basis for any modification
*     element contained in the group expression obtained from the
*     environment. If IGRP0 holds an invalid identifier (such as
*     GRP__NOID) on entry then modification elements are included
*     literally in the output group.
*
*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The parameter with which to associate the group.
*     IGRP0 = INTEGER (Given)
*        The GRP identifier for the group to be used as the basis for
*        any modification elements. 
*     IGRP = INTEGER (Given and Returned)
*        The GRP identifier for the group to which the supplied .sdf
*        files are to be appended. 
*     SIZE = INTEGER (Returned)
*        The total number of file names in the returned group.
*     FLAG = LOGICAL (Returned)
*        If the group expression was terminated by the GRP "flag"
*        character, then FLAG is returned .TRUE. Otherwise it is
*        returned .FALSE. Returned .FALSE. if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.


*     Notes:
*        -  If an error is reported the group is returned unaltered.
*        -  A null value (!) can be given for the parameter to indicate
*        that no more NDFs are to be specified. The corresponding error 
*        is annulled before returning unless no NDFs have been added to
*        the group.
*        -  Routine NDG_CREXP can be used if the application already 
*        knows the names of the NDFs to be stored in the group, avoiding
*        the use of the parameter system.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1992 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'PAR_ERR'          ! Parameter system error constants.

*  Arguments Given:
      CHARACTER PARAM*(*)
      INTEGER   IGRP0

*  Arguments Given and Returned:
      INTEGER IGRP

*  Arguments Returned:
      INTEGER SIZE
      LOGICAL FLAG

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER GRPEXP*(GRP__SZGEX)! Group expression.
      INTEGER IPAR               ! SUBPAR parameter identifier.
      INTEGER SIZE0              ! Initial size of the group.

*.

*  Ensure that a .FALSE. value is returned for FLAG if an error 
*  has already occured.
      FLAG = .FALSE.
      SIZE = 0

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that error reports are defferred.
      CALL ERR_MARK

*  Get the initial size of the group. If an invalid group identifier was
*  supplied, use a size of zero.
      CALL GRP_GRPSZ( IGRP, SIZE0, STATUS )
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_ANNUL( STATUS )         
         SIZE0 = 0
      END IF

*  Get a group expression from the environment using the supplied
*  parameter.
 10   CONTINUE
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, GRPEXP, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Expand the group expression into a list of NDF names and append
*  them to the end of the specified group.
      CALL NDG_CREXP( GRPEXP, IGRP0, IGRP, SIZE, FLAG, STATUS )

*  If an error has occurred while expanding the group 
*  expression, ask the user to try again.
      IF( STATUS .NE. SAI__OK ) THEN

*  Add contextual information.
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_CREAT_ERR1',
     : 'Please supply replacements for any bad data set names given '//
     : 'by parameter %^P', STATUS )
         CALL ERR_REP( ' ', ' ', STATUS )

*  Flush the error stack.
         CALL ERR_FLUSH( STATUS )

*  Cancel the parameter value.
         CALL SUBPAR_CANCL( IPAR, STATUS )

*  Annul any errors produced by the previous line.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*  Go back for a re-prompt.
         GO TO 10

      END IF

*  If a null parameter value was given, annul the error. If no NDFs
*  were added to the group re-report it with a more friendly report.
 999  CONTINUE

      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         IF( SIZE .LE. SIZE0 ) THEN
            STATUS = PAR__NULL
            CALL MSG_SETC( 'P', PARAM )
            CALL ERR_REP( 'NDG_CREAT_ERR2',
     :            'NDG_CREAT: Null value given for the %^P parameter.', 
     :                     STATUS )
         END IF

*  If the parameter request was aborted, annul the error and re-report
*  it with a more friendly message.
      ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_CREAT_ERR3',
     :    'NDG_CREAT: Aborted attempt to associate a group of data '//
     :    'sets with the %^P parameter.', STATUS )

*  If any other error occurred, add a context message.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_CREAT_ERR4',
     : 'NDG_CREAT: Unable to associate a group of data sets with '//
     : 'parameter %^P', STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
