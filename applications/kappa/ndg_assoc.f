      SUBROUTINE NDG_ASSOC( PARAM, IGRP, SIZE, FLAG, STATUS )
*+
*  Name:
*     NDG_ASSOC

*  Purpose:
*     Store names of existing NDFs specified through the environment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL NDG_ASSOC( PARAM, IGRP, SIZE, FLAG, STATUS )

*  Description:
*     A group expression is obtained from the environment using the
*     supplied parameter. The expression is parsed (using the
*     facilities of the GRP routine GRP_GROUP, see SUN/150) to produce
*     a list of explicit NDF names which are appended to the end of the
*     supplied group. If an error occurs while parsing the group 
*     expression, the user is re-prompted for a new group expression. 
*     A new group is created if the supplied group identifier is equal 
*     to GRP__NOID.  Any names containing wildcards are expanded into 
*     a list of NDF names.

*  Arguments:
*     PARAM = CHARACTER*(*) (Given)
*        The parameter with which to associate the group expression.
*     IGRP = INTEGER (Given and Returned)
*        The identifier of the group in which the NDF names are to be
*        stored.
*     SIZE = INTEGER (Returned)
*        The total number of NDF names in the returned group.
*     FLAG = LOGICAL (Returned)
*        If the group expression was terminated by the GRP "flag
*        character", then FLAG is returned true. Otherwise it is
*        returned false. Returned .FALSE. if an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine checks that the specified files exist, and also 
*     checks that any .sdf files contain legal native NDFs. However, it 
*     does not attempt to check the legality of foreign data format files.
*     -  If an error is reported the group is returned unaltered. If
*     no group is supplied, an empty group is returned.
*     -  A null value (!) can be given for the parameter to indicate
*     that no more NDFs are to be specified. The corresponding error 
*     is annulled before returning unless no NDFs have been added to
*     the group.
*     -  If the last character in the supplied group expression is 
*     a colon (:), a list of the NDFs represented by the group 
*     expression (minus the colon) is displayed, but none are
*     actually added to the group. The user is then re-prompted for 
*     a new group expression.
*     -  Routine NDG_ASEXP can be used if the application already 
*     knows the names of the NDFs to be stored in the group, avoiding
*     the use of the parameter system.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-AUG-1992 (DSB):
*        Original version.
*     5-FEB-1993 (DSB):
*        Facility added for listing files represented by a group 
*        expression by terminating the group expression with a colon.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'GRP_ERR'          ! GRP error constants.
      INCLUDE 'PAR_ERR'          ! Parameter system error constants.
      INCLUDE 'NDG_ERR'          ! NDG error constants.

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Given and Returned:
      INTEGER   IGRP
      
*  Arguments Returned:
      INTEGER   SIZE
      LOGICAL   FLAG

*  Status:
      INTEGER   STATUS             ! Global status

*  Local Variables:
      LOGICAL AGAIN                ! True if the user is to be re-prompted.
      CHARACTER GRPEXP*(GRP__SZGEX)! Group expression.
      INTEGER FIRST                ! Index of first non-blank character.
      INTEGER I                    ! Loop count.
      INTEGER IGRP2                ! A group to contain any bad NDF names.
      INTEGER IPAR                 ! SUBPAR parameter identifier.
      INTEGER ISTAT                ! Temporary status.
      INTEGER LAST                 ! Index of last non-blank character.
      LOGICAL LIST                 ! True if a listing of files is required.
      CHARACTER NAME*(GRP__SZNAM)  ! Good NDF file name.
      INTEGER SIZE0                ! The initial size fo the group.
      INTEGER SZBAD                ! The size of the group holding the
                                   ! list of bad NDFs.
      CHARACTER STRING*(GRP__SZNAM)! String identifying a bad NDF 

*.

*  Ensure that a false value is returned for FLAG if an error has
*  already occured.
      FLAG = .FALSE.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Ensure that error reports are defferred.
      CALL ERR_MARK

*  Overwrite any value SIZE may have on entry.
      SIZE = 0

*  If a group has been supplied, get its initial size.
      IF( IGRP .NE. GRP__NOID ) THEN
         CALL GRP_GRPSZ( IGRP, SIZE0, STATUS )
      ELSE
         SIZE0 = 0
      END IF

*  Create a group to hold any bad NDF names, and the reassons why they
*  are bad.
      CALL GRP_NEW( 'BAD DATA SETS', IGRP2, STATUS )

*  Get a group expression from the environment using the supplied
*  parameter.
 10   CONTINUE
      CALL SUBPAR_FINDPAR( PARAM, IPAR, STATUS )
      CALL SUBPAR_GETNAME( IPAR, GRPEXP, STATUS )

*  If the first and last characters are single quotes, remove them.
      CALL CHR_FANDL( GRPEXP, FIRST, LAST )
      IF( GRPEXP( FIRST : FIRST ) .EQ. '''' .AND.
     :    GRPEXP( LAST : LAST ) .EQ. '''' ) THEN
         GRPEXP( FIRST : FIRST ) = ' '
         GRPEXP( LAST : LAST ) = ' '
      END IF

*  If the last character is a colon remove it and set a flag 
*  indicating that the names are to be listed but not included in the 
*  returned group.
      CALL CHR_FANDL( GRPEXP, FIRST, LAST )
      IF( GRPEXP( LAST : LAST ) .EQ. ':' ) THEN
         LIST = .TRUE.      
         GRPEXP( LAST : LAST ) = ' '
      ELSE
         LIST = .FALSE.
      END IF

*  Expand the group expression into a list of NDF names and append
*  them to the end of the specified group.
      CALL NDG_ASEXP( GRPEXP, IGRP2, IGRP, SIZE, FLAG, STATUS )

*  If some of the files were not valid NDFs, annul the error and then
*  re-report a more friendly message for each bad NDF.
      IF( STATUS .EQ. NDG__NOFIL ) THEN
         CALL ERR_ANNUL( STATUS )

*  Set up a temporary bad inherited status which can be passed to 
*  ERR_REP.
         ISTAT = SAI__ERROR

*  Get the size of the group which holds the names of the bad NDFs.
         CALL GRP_GRPSZ( IGRP2, SZBAD, STATUS )

*  Loop round each bad name.
         DO I = 1, SZBAD

*  Get the next bad NDF name.
            CALL GRP_GET( IGRP2, I, 1, STRING, STATUS )

*  Abort if anything has gone wrong with the GRP routines.
            IF( STATUS .NE. SAI__OK ) GO TO 999

*  Report it, using the temporary (bad) inherited status.
            CALL MSG_SETC( 'NDF', STRING )
            CALL ERR_REP( 'NDG_ASSOC_ERR1', '  Cannot access ^NDF', 
     :                    ISTAT )

         END DO

*  Set the size of the group holding the bad NDFs back to zero.
         CALL GRP_SETSZ( IGRP2, 0, STATUS )

*  Abort if anything has gone wrong with the GRP routines.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Set a bad status to correspond with the messages reported with the
*  temporary status.
         STATUS = SAI__ERROR

*  Indicate that the user should be re-prompted.
         AGAIN = .TRUE.

*  If there was something wrong with the format of the supplied group 
*  expression, indicate that the user is to re-prompted.
      ELSE IF( STATUS .EQ. GRP__BADME .OR.
     :         STATUS .EQ. GRP__DEEP .OR.
     :         STATUS .EQ. GRP__FIOER .OR.
     :         STATUS .EQ. GRP__NULNM ) THEN
         AGAIN = .TRUE.

*  If all went well, but the group expression ended in a colon,
*  list the new names added to the group, and indicate that a new 
*  group is required. Flush each report individually to avoid the 
*  possibilioty of the EMS stack overflowing if many NDFs have
*  been specified.
      ELSE IF( LIST .AND. STATUS .EQ. SAI__OK ) THEN

         ISTAT = SAI__ERROR
         CALL ERR_REP( ' ', ' ', ISTAT )

         DO I = SIZE0 + 1, SIZE
            CALL GRP_GET( IGRP, I, 1, NAME, STATUS )
            CALL MSG_SETC( 'NAME', NAME )
            ISTAT = SAI__ERROR
            CALL ERR_REP( ' ','    ^NAME', ISTAT )
            CALL ERR_FLUSH( ISTAT )
         END DO

         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', ' ', STATUS )
         AGAIN = .TRUE.

*  If any other error occurred, or if no error occurred, the user will
*  not be re-prompted.
      ELSE
         AGAIN = .FALSE.
      END IF

*  If the user is to be re-prompted...
      IF( AGAIN ) THEN      

*  Ask the user to give a new parameter value.
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_ASSOC_ERR2',
     :                 '  Please give a new value for parameter %^P',
     :                 STATUS )
         CALL ERR_REP( ' ', ' ', STATUS )

*  Flush the errors so that the user sees them.
         CALL ERR_FLUSH( STATUS )

*  Cancel the parameter value.
         CALL SUBPAR_CANCL( IPAR, STATUS )

*  Annul any errors produced by the previous line.
         IF( STATUS .NE. SAI__OK ) CALL ERR_ANNUL( STATUS )

*  Set the group back to its previous size.
         CALL GRP_SETSZ( IGRP, SIZE0, STATUS )

*  Go back for a re-prompt.
         GO TO 10

      END IF

*  Delete the group used to hold the list of bad NDFs.
 999  CONTINUE
      CALL GRP_DELET( IGRP2, STATUS )

*  If a null parameter value was given, annul the error. If no NDFS 
*  have been added to the group re-report it with a more friendly 
*  message.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         IF( SIZE .LE. SIZE0 ) THEN
            STATUS = PAR__NULL
            CALL MSG_SETC( 'P', PARAM )
            CALL ERR_REP( 'NDG_ASSOC_ERR3',
     :    'NDG_ASSOC: A null group of data sets was given for the %^P'//
     :    ' parameter.', STATUS )
         END IF         

*  If the parameter request was aborted, annul the error and re-report
*  it with a more friendly message.
      ELSE IF ( STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
         STATUS = PAR__ABORT
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_ASSOC_ERR4',
     :    'NDG_ASSOC: Aborted attempt to associate a group of data '//
     :    'sets with the %^P parameter.', STATUS )

*  If any other error occurred, add a context message.
      ELSE IF( STATUS .NE. SAI__OK ) THEN
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'NDG_ASSOC_ERR5',
     : 'NDG_ASSOC: Unable to associate a group of data sets with '//
     : ' parameter %^P', STATUS )
      END IF

*  Release the error stack.
      CALL ERR_RLSE

      END
