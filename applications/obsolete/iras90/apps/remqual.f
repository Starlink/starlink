      SUBROUTINE REMQUAL( STATUS )
*+
*  Name:
*     REMQUAL

*  Purpose:
*     Remove specified quality definitions from a group of NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL REMQUAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine removes selected quality name definitions from each
*     NDF specified by parameter NDF.  All quality names information may
*     be removed by specifying a quality name of "ANY".  For more
*     information about using quality within the IRAS90 package see
*     the help on "Quality_in_IRAS90".

*  Usage:
*     REMQUAL NDF QNAMES

*  ADAM Parameters:
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be added to the NDFs.
*        See help on "History_in_IRAS90" for more information on
*        history.                              [current history setting]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering".
*                                       [current message filter setting]
*     NDF = NDF (Update)
*        A group of NDFs from which quality information is to be
*        removed. This should be in the form of a group expression (see
*        help on "Group_expressions").
*     QNAMES = LITERAL (Read)
*        A list of up to 10 quality names to be removed from the input
*        NDFs. This should be in the form of a group expression (see
*        help on "Group_expressions"). If more than 10 names are
*        supplied, only the first 10 are used. If any of the supplied
*        quality names are not defined in the NDF, then warning
*        messages are given but the application continues to remove any
*        other specified quality names. If the string ANY is specified,
*        then all defined quality names are removed. If no defined
*        quality names remain, the structure used to store quality name
*        information is deleted. This feature can be used to get rid of
*        corrupted quality name information.

*  Examples:
*     REMQUAL M51* ANY
*        This example will remove all defined quality names from all
*        NDFs with names starting with the string "M51".

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-OCT-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PAR_ERR'          ! PAR error values.
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'GRP_PAR'          ! GRP constants.
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXQNM             ! Max. no. of quality names which can
                                 ! be deleted.
      PARAMETER ( MAXQNM = 10 )

*  Local Variables:
      INTEGER ADDED              ! No. of names added to group.
      LOGICAL FLAG               ! True if group expression was
                                 ! terminated with a minus sign.
      INTEGER IGRP1              ! Identifier for group holding input
                                 ! NDF names.
      INTEGER HISREC             ! No. of lines of history.
      CHARACTER HSTORY( MAXQNM + 1 )*(IRQ__SZQNM+40)! Lines of history
                                 ! to add to the NDF.
      INTEGER I                  ! NDF count.
      INTEGER INAME              ! Name count.
      INTEGER IGRP2              ! Group identifier.
      CHARACTER LOCS(5)*(DAT__SZLOC)! Locators to the quality name
                                 ! information.
      INTEGER NDFIN              ! Identifier for the input NDF.
      INTEGER NIN                ! No. of input NDFs.
      INTEGER NNAMES             ! No. of quality names to be removed.
      CHARACTER QNAME*(IRQ__SZQNM)! Qaulity name to be removed.
      CHARACTER XNAME*(DAT__SZNAM)! NDF enstension holding the quality
                                 ! name information.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Get a group containing the names of the NDFs to be processed.
      CALL IRM_RDNDF( 'NDF', 0, 1, '  Give more NDF names...', 
     :                IGRP1, NIN, STATUS )

*  Create a group to hold the quality names which are to be removed.
      CALL GRP_NEW( 'QUALITY NAMES', IGRP2, STATUS )

*  Ensure that the group is case insensitive.
      CALL GRP_SETCS( IGRP2, .FALSE., STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999 

*  Obtain the quality names to be removed, storing them in the group
*  just created. Loop until the group expression obtained from the
*  environment does not terminate with a minus sign.
      FLAG = .TRUE.
      DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )

*  Unless the maximum no. of quality names have already been supplied,
*  get more quality names.
         IF( NNAMES .LT. MAXQNM ) THEN
            CALL GRP_GROUP( 'QNAMES', GRP__NOID, IGRP2, NNAMES, ADDED,
     :                       FLAG, STATUS )

*  If the maximum no. of quality names have already been supplied, warn
*  the user.
         ELSE
            FLAG = .FALSE.
            CALL MSG_SETI( 'MX', MAXQNM )
            CALL MSG_OUTIF( MSG__QUIET, 'REMQUAL_MSG1',
     : 'WARNING: Only ^MX quality names can be removed in a single run',
     :                     STATUS )

*  If too many quality names have been supplied, delete the surplus
*  names from the group.
            IF( NNAMES .GT. MAXQNM ) THEN
               CALL MSG_SETI( 'MX', MAXQNM )
               CALL MSG_OUTIF( MSG__QUIET, 'REMQUAL_MSG2',
     :'WARNING: Only the first ^MX supplied quality names will be used',
     :                     STATUS )
               CALL GRP_SETSZ( IGRP2, MAXQNM, STATUS )
            END IF

         END IF

      END DO

*  The user may have indicated the end of the group by giving a null
*  value for the parameter. This would normally cause the application to
*  abort, so annul the error in order to prevent this from happening.
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  If there are no quality names to be removed, abort.
      IF( NNAMES .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'REMQUAL_ERR1',
     :                 'REMQUAL: No quality names specified',
     :                 STATUS )
      END IF

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Loop round each NDF to be processed.
      DO I = 1, NIN
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'UPDATE', NDFIN, STATUS )

*  Tell the user which NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', NDFIN )
         CALL MSG_OUTIF( MSG__NORM, 'REMQUAL_MSG3',
     :                   '  Processing ^NDF...', STATUS )

*  Attempt to locate any existing quality name information in the input
*  NDF. If such information is found, LOC is returned holding a set of
*  5 HDS locators which identify the NDF and the various components of
*  the quality information. XNAME is returned holding the name of the
*  NDF extension in which the information was found. If no quality name
*  information is found, then an error is reported.
         CALL IRQ_FIND( NDFIN, LOCS, XNAME, STATUS )

*  Initialise the first line of text to store as history information
*  within the NDF.
         HISREC = 1
         HSTORY( HISREC ) = 'The following quality name(s) were '//
     :                      'removed: '

*  Loop round to remove each specified quality name.
         DO INAME = 1, NNAMES

*  Get the next quality name.
            CALL GRP_GET( IGRP2, INAME, 1, QNAME, STATUS )

*  Remove the quality name. 
            IF( STATUS .EQ. SAI__OK ) THEN
               CALL IRQ_REMQN( LOCS, QNAME, STATUS )

*  If the quality name was not found, annul the error and give a warning
*  message.
               IF( STATUS .EQ. IRQ__NOQNM ) THEN
                  CALL ERR_ANNUL( STATUS )
                  CALL MSG_SETC( 'QN', QNAME )
                  CALL MSG_OUTIF( MSG__NORM, 'REMQUAL_MSG4',
     :            '    Quality name "^QN" not found', STATUS )

*  Otherwise, add the quality name to the list of quality names
*  successfully removed.
               ELSE
                  IF( QNAME .NE. 'ANY' ) THEN
                     CALL MSG_SETC( 'QN', QNAME )
                     CALL MSG_OUTIF( MSG__NORM, 'REMQUAL_MSG5',
     :                               '    Quality name "^QN" removed', 
     :                               STATUS )
                     HISREC = HISREC + 1
                     HSTORY( HISREC ) = '  '//QNAME

*  Give a better message if all quality names have been removed.
                  ELSE
                     CALL MSG_OUTIF( MSG__NORM, 'REMQUAL_MSG6',
     :                               '    All quality names removed', 
     :                               STATUS )
                     HISREC = 2
                     HSTORY( 1 ) = 'All quality names were removed.'
                     HSTORY( 2 ) = ' '

                  END IF

               END IF

            END IF

         END DO

*  Add a history record to the NDF, so long as some quality names were
*  succesfully removed.
         IF( HISREC .GT. 1 ) THEN
            CALL IRM_HIST( 'HISTORY', NDFIN, 'IRAS90:REMQUAL', HISREC,
     :                      HSTORY, STATUS )
         END IF

*  Release the quality name information.
         CALL IRQ_RLSE( LOCS, STATUS )

*  Annul the NDF identifier.
         CALL NDF_ANNUL( NDFIN, STATUS )

*  If an error occured processing the current NDF, flush the error.
         IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Process the next NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Store a list of the processed files for use by later applications.
      CALL IRM_LISTN( 'NDFLIST', IGRP1, 'REMQUAL', STATUS )

*  Delete the groups which holds the quality names and NDF names.
 999  CONTINUE
      CALL GRP_DELET( IGRP1, STATUS )
      CALL GRP_DELET( IGRP2, STATUS )

*  End the NDF context. This automatically annuls the NDF identifier.
      CALL NDF_END( STATUS )

*  If an error occurred, clear up.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was equested, 
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            CALL ERR_REP( 'REMQUAL_ERR2',
     :    'REMQUAL: Unable to remove quality names from a set of NDFs.',
     :                     STATUS )
         END IF
      END IF

      END
