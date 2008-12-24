      SUBROUTINE SHOWQUAL( STATUS )
*+
*  Name:
*     SHOWQUAL

*  Purpose:
*     Display the quality names defined in a group of NDFs.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SHOWQUAL( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine displays a list of all the quality names currently
*     defined within each NDF specified by parameter IN. The
*     descriptive comments which were stored with the quality names
*     when they were originally defined are also displayed. An option
*     exists for also displaying the number of pixels which hold each
*     quality. For more information about using quality within the
*     IRAS90 package see help on "Quality_in_IRAS90".

*  Usage:
*     SHOWQUAL IN [COUNT]

*  ADAM Parameters:
*     COUNT = _LOGICAL (Read)
*        If true, then the number of pixels in each NDF which holds
*        each defined quality is displayed. These figures are shown
*        in parentheses between the quality name and associated
*        comment.  This option adds significantly to the run time.  [NO]
*     IN = NDF (Read)
*        A group of NDFs in which the quality information is to be
*        stored. This should be in the form of a group expression (see
*        help on "Group_expressions"). 
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]

*  Examples:
*     SHOWQUAL "M51,CENA" YES
*        This example displays all the quality names currently defined
*        for the two NDFs M51 and CENA, together with the number of
*        pixels holding each quality.

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
      INCLUDE 'IRQ_PAR'          ! IRQ constants.
      INCLUDE 'IRQ_ERR'          ! IRQ error values.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER BIT                ! If FIXED is false, then BIT holds
                                 ! the bit number (LSB=1) within the
                                 ! QUALITY component, at which the
                                 ! current quality is stored. If FIXED
                                 ! is true, then BIT is indeterminate.
      LOGICAL CDONE              ! True if a count of pixels holding
                                 ! each quality has been performed.
      CHARACTER COMMNT*(IRQ__SZCOM)! A comment describing the current
                                 ! quality.
      INTEGER CONTXT             ! Search context for IRQ_NXTQN.
      LOGICAL COUNT              ! True if a count of pixels holding
                                 ! each quality is required.
      LOGICAL DONE               ! True if all quality names have been
                                 ! obtained.
      LOGICAL FIXED              ! True if the the current quality is
                                 ! either held or not held by all
                                 ! pixels.
      INTEGER I                  ! Loop count.
      INTEGER IGRP               ! Identifier for group holding NDF
                                 ! names.
      CHARACTER LOCS( 5 )*(DAT__SZLOC) ! Locators for quality name
                                 ! information.
      INTEGER NAMES              ! No. of quality names defined in the
                                 ! NDF.
      INTEGER NDFIN              ! Identifier for input NDF.
      INTEGER NIN                ! The total number of NDFs to be
                                 ! processed.
      INTEGER NPIX               ! The number of pixels in the input NDF.
      INTEGER NPXSET( IRQ__QBITS )! No. of set pixels in each bit plane
                                 ! of the QUALITY component.
      INTEGER QCOUNT             ! The number of pixels which hold the
                                 ! current quality.
      LOGICAL QI                 ! True if quality names information was
                                 ! found in the input NDF.
      CHARACTER QNAME*(IRQ__SZQNM)! The next quality name.
      LOGICAL VALUE              ! If FIXED is true, then VALUE is true
                                 ! if all pixels hold the quality, and
                                 ! false if no pixels hold the quality.
                                 ! If FIXED is false, then VALUE is
                                 ! indeterminate.
      CHARACTER XNAME*(DAT__SZNAM)! NDF extension in whcih quality
                                 ! names are stored.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      QI = .FALSE.

*  Establish the conditional message filter level using parameter
*  MSG_LEVEL.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*  Get a group containing the names of the NDFs to be processed.
      CALL IRM_RDNDF( 'IN', 0, 1, '  Give more NDF names...', 
     :                IGRP, NIN, STATUS )

*  See if the numbers of pixels which hold each quality are to be
*  displayed.
      CALL PAR_GET0L( 'COUNT', COUNT, STATUS )

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Loop round each NDF to be processed.
      DO I = 1, NIN
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRP, I, 'READ', NDFIN, STATUS )

*  Tell the user which NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', NDFIN )
         CALL MSG_OUTIF( MSG__NORM, 'SHOWQUAL_MSG1',
     :                   '  Processing ^NDF...', STATUS )

*  Attempt to locate any existing quality name information in the input
*  NDF. If such information is found, LOC is returned holding a set of
*  5 HDS locators which identify the NDF and various components of the
*  quality information. XNAME is returned holding the name of the NDF
*  extension in which the information was found. If no quality name
*  information is found, then an error is reported.
         CALL IRQ_FIND( NDFIN, LOCS, XNAME, STATUS )

*  If quality names information was found, find the number of defined
*  quality names.
         IF( STATUS .EQ. SAI__OK ) THEN
            CALL IRQ_NUMQN( LOCS, NAMES, STATUS )
            QI = .TRUE.

*  If no quality names information was found, annul the error and set 
*  the number of defined names to zero.
         ELSE IF( STATUS .EQ. IRQ__NOQNI ) THEN
            CALL ERR_ANNUL( STATUS )
            NAMES = 0
            QI = .FALSE.
         END IF

*  If no quality names are defined give a message.
         IF( NAMES .EQ. 0 ) THEN
            CALL MSG_OUTIF( MSG__NORM, 'SHOWQUAL_MSG2',
     :      '    No quality name defintions found', STATUS )

*  Otherwise...
         ELSE

*  Indicate that no explicit count has yet been made of the number of
*  pixels set in each bit plane of the QUALITY component.
            CDONE = .FALSE.

*  If a count is required, get the total number of pixels in the input
*  NDF.
            IF( COUNT ) CALL NDF_SIZE( NDFIN, NPIX, STATUS )

*  Initialise the context for the IRQ_NXTQN routine and get
*  the first defined quality name.
            CONTXT = 0
            CALL IRQ_NXTQN( LOCS, CONTXT, QNAME, FIXED, VALUE, BIT,
     :                      COMMNT, DONE, STATUS )
      
*  Loop round displaying each quality name in turn.
            DO WHILE( .NOT. DONE .AND. STATUS .EQ. SAI__OK )

*  If a count of pixels with each quality is required...
               IF( COUNT ) THEN

*  ...fixed quality names refer to all or none of the pixel in the NDF.
                  IF( FIXED ) THEN
                     IF( VALUE ) THEN
                        QCOUNT = NPIX
                     ELSE
                        QCOUNT = 0
                     END IF

*  Variable quality names can differ from pixel to pixel. Therefore an
*  explicit count needs to be made through the QUALITY component. If
*  this has not already been done, do it now.
                  ELSE
                     IF( .NOT. CDONE ) THEN
                        CALL IRQ_CNTQ( LOCS, IRQ__QBITS, NPXSET,
     :                                 STATUS )
                        CDONE = .TRUE.
                     END IF

*  Find the number of pixels which hold the current quality.
                     QCOUNT = NPXSET( BIT )
                  END IF

*  Display the quality name, comment, and count.
                  CALL MSG_SETI( 'CNT', QCOUNT )
                  CALL MSG_SETC( 'COM', COMMNT )
                  CALL MSG_OUTIF( MSG__NORM, 'SHOWQUAL_MSG3',
     :                            '    '//QNAME//' - "^COM" (^CNT)',
     :                            STATUS )

*  If no count is required, just display the quality name and comment.
               ELSE
                  CALL MSG_SETC( 'COM', COMMNT )
                  CALL MSG_OUTIF( MSG__NORM, 'SHOWQUAL_MSG4',
     :                            '    '//QNAME//' - "^COM"',
     :                            STATUS )

               END IF

*  Get the next defined quality name and related information.
               CALL IRQ_NXTQN( LOCS, CONTXT, QNAME, FIXED, VALUE, BIT,
     :                         COMMNT, DONE, STATUS )

            END DO
         END IF

*  Release the quality name information.
         IF( QI ) CALL IRQ_RLSE( LOCS, STATUS )

*  Annul the NDF identifier.
         CALL NDF_ANNUL( NDFIN, STATUS )

*  If an error occured processing the current NDF, flush the error.
         IF( STATUS .NE. SAI__OK ) CALL ERR_FLUSH( STATUS )

*  Process the next NDF.
      END DO

*  Display a blank line.
      CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Store a list of the processed files for use by later applications.
      CALL IRM_LISTN( 'NDFLIST', IGRP, 'SHOWQUAL', STATUS )

*  Delete the group which contains the NDF names.
 999  CONTINUE
      CALL GRP_DELET( IGRP, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN

*  If a null parameter was given or a parameter abort was equested, 
*  annul the error.
         IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
            CALL ERR_ANNUL( STATUS )

*  If any other error occurred, then report a contextual message.
         ELSE
            CALL ERR_REP( 'SHOWQUAL_ERR1',
     :                    'SHOWQUAL: Unable to display quality names.',
     :                     STATUS )
         END IF
      END IF

      END
