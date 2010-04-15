      SUBROUTINE IRASTRACE( STATUS )
*+
*  Name:
*     IRASTRACE

*  Purpose:
*     Display the attributes of a group of IRAS90 NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IRASTRACE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine displays the information stored in the IRAS
*     extensions of a group of NDFs. Either CRDD files or images may be
*     specified.

*  Usage:
*     IRASTRACE IN [LOGFILE]

*  ADAM Parameters:
*     IN = NDF (Read)
*        The group of NDFs to process. This should be in the form of a
*        group expression (see help on "Group_expressions").
*     LOGFILE = LITERAL (Write)
*        The name of a text file to receive a copy of the displayed
*        information. The run time default is for no log file to be
*        produced.                                                   [!]
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen and
*        logged to the log file. This should take one of the values
*        QUIET, NORMAL or VERBOSE (see help on "Message_filtering").
*                                       [current message filter setting]

*  Examples:
*     IRASTRACE CENA* CENA.LIS
*        This will display the contents of the IRAS extensions within
*        all NDF beginning with the string "CENA". The information will
*        also be written to the text file CENA.LIS

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JAN-1992 (DSB):
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
      INCLUDE 'PAR_ERR'          ! PAR_ error values.
      INCLUDE 'MSG_PAR'          ! MSG_ constants.
      INCLUDE 'IRI_PAR'          ! IRI_ constants.

*  Global Variables:
      INCLUDE 'TRA_COM'          ! IRASTRACE common blocks.
*        TRA_FD = INTEGER (Read and Write)
*           The file descriptor for the log file.
*        TRA_FILT = INTEGER (Read and Write)
*           The current conditional message filter level.
*        TRA_LOG = LOGICAL (Read and Write)
*           True if displayed information is to be logged to a text
*           file.
*        TRA_NBL = INTEGER (Read and Write)
*           No. of leading spaces to display in front of the supplied
*           text.

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ITRAA0            ! Routine to replace MSG_OUTIF.

*  Local Variables:
      CHARACTER CLOC*(DAT__SZLOC)! Locator to a component of the IRASTRACE extension.
      CHARACTER CTYPE*(DAT__SZTYP)! Type of component.
      CHARACTER CVAL*60          ! Component value (converted to
                                 ! character form if necessary).
      CHARACTER INSTRM*(IRI__SZINS)! IRAS instrument.
      CHARACTER LOC*(DAT__SZLOC) ! Locator to the IRASTRACE extension.
      CHARACTER NAME*(DAT__SZNAM)! Name of current IRAS component.
      CHARACTER UNITS*(IRI__SZUNI)! NDF units.
      CHARACTER TYPE*(IRI__SZTYP)! Type of input image.

      INTEGER BAND               ! IRAS waveband index.
      INTEGER I                  ! Loop count.
      INTEGER IDA                ! IRA identifier for astrometry info.
      INTEGER IDC                ! IRC identifier for CRDD info.
      INTEGER IGRP1              ! Identifier for the NDF group.
      INTEGER J                  ! Loop count.
      INTEGER NCOMP              ! No. of components within IRAS extension.
      INTEGER NDFIN              ! Identifier for input NDF.
      INTEGER NIN                ! No. of NDFs to be processed.
      INTEGER SIZE               ! Size of component.

      LOGICAL AST                ! True if ASTROMETRY is found.
      LOGICAL CRDD               ! True if CRDD_INFO is found.
      LOGICAL IMAGE              ! True if IMAGE_INFO is found.
      LOGICAL ON                 ! True if screen output is enabled.
      LOGICAL PRIM               ! True if component is of primative type.
      LOGICAL THERE              ! True if IRAS extension exists.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL, and store it in common.
      CALL MSG_IFGET( STATUS )
      CALL MSG_IFLEV( TRA_FILT )

*  Get a group containing the names of the NDFs to be processed.
      CALL IRM_RDNDF( 'IN', 0, 1, '  Give more NDF names...', IGRP1,
     :                NIN, STATUS )

*  Abort if an error has been reported, or if there are no NDFs to
*  process.
      IF ( STATUS .NE. SAI__OK .OR. NIN .EQ. 0 ) GO TO 999

*  If required open a log file. The file descriptor returned in FD is
*  used to access this file.
      CALL IRM_ASFIO( 'LOGFILE', 'WRITE', 'LIST', 80, TRA_FD, TRA_LOG,
     :                 STATUS )
      IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )

*  Tell the user that output is being logged to the text file.
      IF( TRA_LOG ) THEN
         CALL MSG_BLANKIF( MSG__NORM, STATUS )
         CALL MSG_OUTIF( MSG__NORM, 'IRASTRACE_MSG1',
     :           '  Logging displayed information to $LOGFILE', STATUS )
      END IF

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise the IRC and IRA systems.
      CALL IRC_INIT( STATUS )
      CALL IRA_INIT( STATUS )

*  Indicate that a new screen page is to be started.
      CALL IRM_PAGE

*  Abort if an error has occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each NDF to be processed.
      DO I = 1, NIN

*  Ensure no extra leading spaces are added to the strings displayed by
*  routine ITRAA0.
         TRA_NBL = 0

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP1, I, 'READ', NDFIN, STATUS )

*  Tell the user which NDF is currently being procesed.
         CALL ITRAA0( MSG__NORM, ' ', ' ', STATUS )
         CALL NDF_MSG( 'NDF', NDFIN )
         CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG2',
     :                '  Processing ^NDF ...', STATUS )

*  Display TITLE, LABEL and UNITS.
         CALL NDF_CMSG( 'TITLE', NDFIN, 'TITLE', STATUS )
         CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG3', '    Title: ^TITLE',
     :                STATUS )

         CALL NDF_CMSG( 'LABEL', NDFIN, 'LABEL', STATUS )
         CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG4', '    Label: ^LABEL',
     :                STATUS )

         CALL NDF_CMSG( 'UNITS', NDFIN, 'UNITS', STATUS )
         CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG5', '    Units: ^UNITS',
     :                STATUS )

*  See if the NDF contains an IRAS extension.
         CALL NDF_XSTAT( NDFIN, 'IRAS', THERE, STATUS )

*  If not, tell the user and pass on to the next NDF.
         IF( .NOT. THERE ) THEN
            CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG6',
     :                   '    The NDF contains no IRAS extension',
     :                   STATUS )

*  If there is an IRAS extension within the NDF...
         ELSE

*  Get a locator to the IRAS extension.
            CALL NDF_XLOC( NDFIN, 'IRAS', 'READ', LOC, STATUS )

*  See how many components are contained within the IRAS extension.
            CALL DAT_NCOMP( LOC, NCOMP, STATUS )

*  List the names of the components of the IRAS extension, noting if
*  certain key components are present.
            CALL ITRAA0( MSG__NORM, ' ', ' ', STATUS )
            CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG7',
     :    '    The IRAS extension contains the following components...',
     :                   STATUS )

            CRDD = .FALSE.
            IMAGE = .FALSE.
            AST = .FALSE.

            DO J = 1, NCOMP

               CALL DAT_INDEX( LOC, J, CLOC, STATUS )
               CALL DAT_NAME( CLOC, NAME, STATUS )

               CALL MSG_SETC( 'NAME', NAME )

               IF( NAME .EQ. 'CRDD_INFO' ) THEN
                  CRDD = .TRUE.
                  CALL MSG_SETC( 'V', ' ' )

               ELSE IF( NAME .EQ. 'IMAGE_INFO' ) THEN
                  IMAGE = .TRUE.
                  CALL MSG_SETC( 'V', ' ' )

               ELSE IF( NAME .EQ. 'ASTROMETRY' ) THEN
                  AST = .TRUE.
                  CALL MSG_SETC( 'V', ' ' )

               ELSE
                  CALL DAT_PRIM( CLOC, PRIM, STATUS )
                  CALL DAT_SIZE( CLOC, SIZE, STATUS )

                  IF( .NOT. PRIM .OR. SIZE .GT. 1 ) THEN
                     CALL DAT_TYPE( CLOC, CTYPE, STATUS )
                     CALL MSG_SETC( 'V', ' - <' )
                     CALL MSG_SETC( 'V', CTYPE )
                     IF( SIZE .GT. 1 ) THEN
                        CALL MSG_SETC( 'V', '(' )
                        CALL MSG_SETI( 'V', SIZE )
                        CALL MSG_SETC( 'V', ')' )
                     END IF
                     CALL MSG_SETC( 'V', '>' )
                  ELSE
                     CALL DAT_GET0C( CLOC, CVAL, STATUS )
                     CALL MSG_SETC( 'V', ' -' )
                     CALL MSG_SETC( 'V', ' '//CVAL )
                  END IF

               END IF

               CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG8',
     :                      '       ^NAME ^V', STATUS )

               CALL DAT_ANNUL( CLOC, STATUS )

            END DO

*  Tell the user what sort of IRAS NDF it is.
            IF( CRDD ) THEN

               IF( IMAGE .OR. AST ) THEN
                  CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG9',
     :     '    and therefore this NDF could be either an image or a '//
     :     'CRDD file (!!)', STATUS )
               ELSE
                  CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG10',
     :                     '    and therefore this NDF is a CRDD file.',
     :                         STATUS )
               END IF

            ELSE IF( IMAGE ) THEN
               CALL ITRAA0( MSG__NORM, ' ', ' ', STATUS )
               CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG11',
     :                      '    and therefore this NDF is an image.',
     :                      STATUS )

            END IF

*  If a CRDD_INFO component was found...
            IF( CRDD ) THEN

*  try importing the NDF into the IRC system.
               CALL IRC_IMPRT( NDFIN, IDC, STATUS )

*  Display the CRDD information with 7 leading spaces.
               CALL ITRAA0( MSG__NORM, ' ', ' ', STATUS )
               CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG12',
     :         '    Contents of the CRDD_INFO component...', STATUS )

               TRA_NBL = 7
               CALL IRC_TRACE( IDC, ITRAA0, STATUS )
               TRA_NBL = 0

*  Annul the IRC identifier.
               CALL IRC_ANNUL( IDC, STATUS )

            END IF

*  If an ASTROMETRY component was found, try importing the NDF into the
*  IRA system.
            IF( AST ) THEN
               CALL IRA_IMPRT( NDFIN, IDA, STATUS )

*  Display the astrometry information with 7 leading spaces.
               CALL ITRAA0( MSG__NORM, ' ', ' ', STATUS )
               CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG13',
     :         '    Contents of the ASTROMETRY component...', STATUS )

               TRA_NBL = 7
               CALL IRA_TRACE( IDA, ITRAA0, STATUS )
               TRA_NBL = 0

*  Annul the IRA identifier.
               CALL IRA_ANNUL( IDA, STATUS )

            END IF

*  If an IMAGE_INFO component was found, try importing the NDF into the
*  IRI system.
            IF( IMAGE ) THEN
               CALL IRI_OLD( NDFIN, INSTRM, BAND, TYPE, UNITS, CLOC,
     :                       STATUS )

*  Display the image information with 7 leading spaces.
               CALL ITRAA0( MSG__NORM, ' ', ' ', STATUS )
               CALL ITRAA0( MSG__NORM, 'IRASTRACE_MSG14',
     :         '    Contents of the IMAGE_INFO component...', STATUS )

               TRA_NBL = 7
               CALL IRI_TRACE( CLOC, ITRAA0, STATUS )
               TRA_NBL = 0

*  Annul the locator.
               CALL DAT_ANNUL( CLOC, STATUS )

            END IF

         END IF

*  Annul the NDF identifier.
         CALL NDF_ANNUL( NDFIN, STATUS )

*  If an error occured processing the current NDF, flush the error.
         IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  If the user has switched off screen output, don't process any more
*  input NDFs.
         CALL IRM_SPAGE( ON )
         IF( .NOT. ON ) GO TO 998

      END DO

*  Display a blank line.
 998  CONTINUE
      CALL ITRAA0( MSG__NORM, ' ', ' ', STATUS )

*  Close down the IRA and IRC systems.
      CALL IRC_CLOSE( STATUS )
      CALL IRA_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Store a list of the processed files for use by later applications.
      CALL IRM_LISTN( 'NDFLIST', IGRP1, 'IRASTRACE', STATUS )

*  Delete the group holding NDFs.
 999  CONTINUE
      CALL GRP_DELET( IGRP1, STATUS )

*  Close any log file.
      IF( TRA_LOG ) CALL FIO_CANCL( 'LOGFILE', STATUS )

*  If a parameter null or abort value was given, annul the error.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IRASTRACE_ERR1',
     :   'IRASTRACE: Error displaying IRAS extension information.',
     :   STATUS )
      END IF

      END
