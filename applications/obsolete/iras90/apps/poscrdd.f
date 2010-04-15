      SUBROUTINE POSCRDD( STATUS )
*+
*  Name:
*     POSCRDD

*  Purpose:
*     Find CRDD samples which lie close to a given sky position.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL POSCRDD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine identifies CRDD files containing scans which pass
*     close to a given sky position. The user supplies a group of CRDD
*     files and the sky coordinates of the required position. A line of
*     text is then displayed (and optionally logged to a text file) for
*     each CRDD file containing the following items of information:
*
*     1) The name of the CRDD file (minus directory specification).
*
*     2) The Satellite Observation Plan (SOP) number.
*
*     3) The observation number.
*
*     4) The minimum cross scan distance (in arc-minutes) from the
*     required position to the boresight track. Positive if the required
*     position is on the sun-ward side of the boresight track.
*
*     5) The in-scan distance between the required position and the
*     reference position stored in the CRDD file, in arc-minutes. This
*     is positive if the closest approach to the required position is
*     reached later than the closest approach to the CRDD file
*     reference position.
*
*     6) The sample number at which the detector specified by item 7
*     reaches its closest approach to the required position. This item
*     is left blank if item 7 is blank.
*
*     7) The number of the detector which is closest to the required
*     position at its point of closest approach. If the position is
*     beyond the edges of the focal plane (i.e. is seen by none of the
*     detectors) then this item (together with item 6) is left blank.
*
*     8) The position angle of the scan at the point of closest
*     approach to the required position, in degrees. This is the angle
*     from north to the focal plane Y axis, going positive through
*     east.
*
*  Usage:
*     POSCRDD IN LON LAT

*  ADAM Parameters:
*     COORDS = LITERAL (Read)
*        Specifies the coordinate system used for referring to sky
*        positions. Valid values include ECLIPTIC, EQUATORIAL,
*        GALACTIC. See help on "Sky_coordinates" for more information
*        on available sky coordinate systems.
*                                        [current sky coordinate system]
*     IN = NDF (Read)
*        Specifies a group of input CRDD files. This should be in the
*        form of a group expression (see help on "Group_expressions").
*     LAT = LITERAL (Read)
*        The sky latitude of the required position, in the coordinate
*        system specified by the parameter COORDS (eg if COORDS is
*        EQUATORIAL then LAT should be given the Declination of the
*        required position). See help on "Sky_coordinates" for the
*        formats allowed for this value. The suggested default is the
*        reference position of the first CRDD file.
*     LOGFILE = LITERAL (Write)
*        The name of a text file to receive a copy of the displayed
*        information. The run time default is for no log file to be
*        produced.                                                   [!]
*     LON = LITERAL (Read)
*        The sky longitude of the required position, in the coordinate
*        system specified by the parameter COORDS (eg if COORDS is
*        EQUATORIAL then LON should be given the Right Ascension of the
*        required position).See help on "Sky_coordinates" for the
*        formats allowed for this value. The suggested default is the
*        reference position of the first CRDD file.
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering"). Output to the log file is not
*        effected by the setting of this parameter.
*                                       [current message filter setting]

*  Examples:
*     POSCRDD ZCMA* "7 2 10" -12 LOGFILE=POS.LOG COORDS=EQ
*        This example displays the distance of all CRDD files which
*        start with the string "ZCMA" from the position RA=7h 2m 10s
*        DEC=-12d 0m 0s. Any scans which actually pass through the
*        position are indicated by the columns with titles "Sample" and
*        "Det." being non-blank. The output information is displayed on
*        the screen and logged to the text file POS.LOG.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-DEC-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'PAR_ERR'          ! PAR_ error constants.
      INCLUDE 'IRA_PAR'          ! IRA_ constants.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NITEM              ! The number of items of information
      PARAMETER ( NITEM = 8 )    ! about each NDF stored in the
                                 ! workspace.


*  Local Variables:
      CHARACTER SCS*(IRA__SZSCS) ! Sky coordinate system to use.


      DOUBLE PRECISION A         ! Sky longitude of required position.
      DOUBLE PRECISION B         ! Sky latitude of required position.
      DOUBLE PRECISION DEC       ! Declination of required position.
      DOUBLE PRECISION RA        ! Right Ascension of required position.


      INTEGER FD                 ! File descriptor for log file.
      INTEGER IGRP               ! The GRP identifier for the group
                                 ! holding the input NDF names.
      INTEGER INDEX              ! Index of current input NDF.
      INTEGER INDF               ! Identifier for input NDF.
      INTEGER IPW1               ! Pointer to work space.
      INTEGER IPW2               ! Pointer to work space.
      INTEGER NCRDDF             ! The number of input CRDD files.
      INTEGER NUSED              ! The number of usable CRDD files.


      LOGICAL LOGPOS             ! True if information is to be logged
                                 ! to a text file.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( STATUS )

*  Get a GRP identifier for a group of NDFs containing the input CRDD.
      CALL IRM_RDNDF( 'IN', 0, 1, 'Give more CRDD file names',
     :                IGRP, NCRDDF, STATUS )

*  Abort if an error has been reported, or if there are no NDFs to
*  process.
      IF ( STATUS .NE. SAI__OK .OR. NCRDDF .EQ. 0 ) GO TO 999

*  If required open a log file. The file descriptor returned in FD is
*  used to access this file.
      CALL IRM_ASFIO( 'LOGFILE', 'WRITE', 'LIST', 80, FD, LOGPOS,
     :                 STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Tell the user that output is being logged to the text file.
      IF( LOGPOS ) THEN
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_OUTIF( MSG__NORM, 'POSCRDD_MSG1',
     :                   '  Logging displayed information to $LOGFILE',
     :                   STATUS )
      END IF

*  Get a value for parameter COORDS.
      CALL IRA_GTSCS( 'COORDS', .FALSE., SCS, STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Start up the IRC package.
      CALL IRC_INIT( STATUS )

*  Get the sky coordinate of the required position.
      CALL POSCA4( 'LON', 'LAT', SCS, IGRP, NCRDDF, A, B, STATUS )

*  Convert to RA and DEC (B1950).
      CALL IRA_CONVT( 1, A, B, SCS, 'EQU(B1950)', IRA__IRJEP, RA, DEC,
     :                STATUS )

*  Get workspace to hold values associated with each CRDD file.
      CALL PSX_CALLOC( NCRDDF*NITEM, '_DOUBLE', IPW1, STATUS )
      CALL PSX_CALLOC( NCRDDF*NITEM, '_DOUBLE', IPW2, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 998

*  Loop round each input CRDD file, storing information about each in
*  the workspace.
      DO INDEX = 1, NCRDDF

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP, INDEX, 'READ', INDF, STATUS )

*  Store information about this CRDD file in the workspace. Invalid
*  values are stored in the workspace if an error occurs (or has already
*  occurred).
         CALL POSCA0( INDF, INDEX, RA, DEC, NCRDDF, NITEM,
     :                %VAL( IPW1 ), STATUS )

*  If an error occured processing the current NDF, add a context report,
*  and flush the errors.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL NDF_MSG( 'NDF', INDF )
            CALL ERR_REP( 'POSCRDD_ERR1',
     :      '    Unable to obtain required information from ^NDF',
     :                    STATUS )
            CALL ERR_FLUSH( STATUS )
         END IF

*  Annul the NDF identifier.
         CALL NDF_ANNUL( INDF, STATUS )

*  Flush any error which occurred during the call to NDF_ANNUL.
         IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

      END DO

*  Close down the IRC package.
      CALL IRC_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Sort the stored information.
      CALL POSCA1( NCRDDF, NITEM, %VAL( IPW1), %VAL( IPW2 ), NUSED,
     :             STATUS )

*  Display the sorted information.
      CALL POSCA2( IGRP, A, B, SCS, LOGPOS, FD, NCRDDF, NITEM,
     :             %VAL( IPW1 ), NUSED, STATUS )

*  Store a list of the processed files for use by later applications.
      CALL IRM_LISTN( 'NDFLIST', IGRP, 'POSCRDD', STATUS )

*  Release the work space.
 998  CONTINUE
      CALL PSX_FREE( IPW1, STATUS )
      CALL PSX_FREE( IPW2, STATUS )

*  Close any log file.
      IF( LOGPOS ) CALL FIO_CANCL( 'LOGFILE', STATUS )

*  If a parameter null or abort value was given, annul the error.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Delete the group holding the names of the input NDFs.
 999  CONTINUE
      CALL GRP_DELET( IGRP, STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'POSCRDD_ERR2',
     :'POSCRDD: Unable to find CRDD samples close to a given position',
     :                 STATUS )
      END IF

      END
