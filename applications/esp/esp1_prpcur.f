

      SUBROUTINE ELF1_PRPCUR(MNCHOI,SWCHOI,TERMES,NTERMS,IMGMES,
     :                       NIMGMS,BUTTNS,CURSOR,IMGDIS,STATUS)
*+
*    Description :
*
*     This determines whether a cursor with a suitable number of choices
*     is available on the current graphics device.  Messages are given
*     describing which buttons to press if the device is a terminal or
*     an image display.  The messages has parameters CHOICETERMn or
*     CHOICEIDn, where n is number of the message starting from 1.
*
*    Invocation :
*
*     CALL ELF1_PRPCUR( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
*    :             NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*
*    Parameters :
*
*     MNCHOI=INTEGER (Given)
*        The minimum number of choices required by the calling
*          application.  It must be positive.
*     SWCHOI=INTEGER (Given)
*        The maximum number of choices for the graphics-device to be an
*          image display. It must be at least %MNCHOI.
*     TERMES( NTERMS )=CHARACTER (Given)
*        Description of which terminal buttons to press to obtain the
*          various choices, to be reported to the user if the device
*          is nominally a terminal, i.e. its number of choices exceeds
*          %SWCHOI.
*     NTERMS=INTEGER (Given)
*        Number of lines describing the action of the terminal choices.
*     IMGMES( NIMGMS )=CHARACTER (Given)
*        Description of the action of the mouse or trackerball buttons
*          to be reported to the user if the device is nominally an
*          image display, i.e. its number of choices is less than or
*          equal to %SWCHOI.
*     NIMGMS=INTEGER (Given)
*        Number of lines describing the action of the image-display
*          choices.
*     BUTTNS=CHARACTER (Given)
*        The terminal buttons to be pressed to obtain the different
*          choices, e.g. '1A.' would mean '1' would give the first
*          choice, 'A' would the second and '.' to exit. A fullstop
*          is the recommended Starlink method for terminating such an
*          interaction.  The last character is assumed to be the exit
*          choice in cases where this string is longer than the number
*          of choices plus one (the exit).
*          characters.  There must be at least %MNCHOI+1 characters.
*          This string is ignored if the device is an image display.
*     CURSOR=LOGICAL (Returned)
*        If true there is a suitable cursor and number of choices.
*     IMGDIS=LOGICAL (Returned)
*        If true the choice device is an image-display mouse or
*        trackerball
*     DEVICE=DEVICE (Given)
*        The graphics workstation.
*
*    Arguments :
*
*     STATUS=INTEGER (Given and Returned)
*        The global status.
*
*    Method :
*
*     If status is bad then exit
*     Validate input data
*     Determine the number of options on the workstation's choice device
*     If the number of choices is less than specified minimum then
*       report error context and abort
*     Activate the cursor and specify the options depending on the
*       number of choices and set cursor-ready flag
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK  (RAL::CUR)
*
*    History :
*
*     1989 Nov 10: Original version (RAL::CUR).
*
*    Type definitions :
      IMPLICIT NONE              ! No implicit typing

*    Global Constants :
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*    Import :
      INTEGER MNCHOI             ! Minimum number of choices
      INTEGER SWCHOI             ! Maximum number fo choices if the
                                 ! device is to be classed as an image
                                 ! display
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages

      CHARACTER *(*) TERMES( NTERMS )
                                 ! Informational messages if device is
                                 ! a terminal
      CHARACTER *(*) IMGMES( NTERMS )
                                 ! Informational messages if device is
                                 ! an image display
      CHARACTER *(*) BUTTNS      ! Choices buttons for a terminal.

*    Export :
      LOGICAL CURSOR             ! Device has a sutiable cursor and
                                 ! choices
      LOGICAL IMGDIS             ! Device is an image-display for the
                                 ! purpose of using the cursor

*    Status :
      INTEGER STATUS             ! Global status

*    External references :
      INTEGER
     :  CHR_LEN

*    Local variables :
      CHARACTER*80 BUTLST        ! List of buttons which may be a
                                 ! trimmed version of the input list
      CHARACTER DATREC(10)*80    ! Data record return by GKS inquiry

      INTEGER CONID              ! Connection identifier
      INTEGER GSTAT              ! Graphics status
      INTEGER I                  ! Loop index
      CHARACTER*4 IC             ! Message counter
      CHARACTER*14 LABEL         ! Informational-message parameter
      INTEGER LDR                ! Length of data record returned by
                                 ! GKS inquiry
      INTEGER MALT               ! Number of alternatives for choice
                                 ! input on graphics device
      INTEGER NC                 ! Number of characters in a string
      INTEGER OL                 ! Number of available prompt/echo types
                                 ! for graphics device
      INTEGER PET                ! Element of prompt/echo types of
                                 ! device returned by GKS inquiry
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WTYPE              ! Workstation type

      REAL EAREA( 4 )            ! Graphics device echo area

                                 ! True if:
      LOGICAL CURAVA             ! A cursor is available

*-

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CURSOR=.FALSE.
      IMGDIS=.FALSE.

*    Validate input data.

      IF ( MNCHOI .LT. 1 .OR. SWCHOI .LT. MNCHOI ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__PROG',
     :     'PRPCUR: Programmer error.  Check calling arguments',
     :      STATUS )
         GOTO 999
      END IF

*    Put out a blank line to ensure the commentary appears on the alpha
*    plane of the terminal.

      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Is there a cursor?

      CURAVA=.FALSE.
      CALL SGS_ICUAV( CURAVA )

      IF ( .NOT. CURAVA ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__NOCUR',
     :     'PRPCUR: Chosen workstation does not have a cursor.',
     :     STATUS )

         GOTO 999
      END IF

      CALL SGS_ICURW( WKID )

*    Find workstation type

      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*    Find number of options on choice device

      CALL GQDCH( WTYPE, 1, 1, 10, GSTAT, MALT, OL, PET, EAREA, LDR,
     :            DATREC )

*    At least one choice required

      IF ( MALT .LT. MNCHOI ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__WDV',
     :     'PRPCUR: Graphics device chosen has unsuitable choice '/
     :     /'device (e.g. mouse or trackerball) for this application.',
     :     STATUS )

         GOTO 999

*       Tell the user what to do.

      ELSE IF ( MALT .LE. SWCHOI ) THEN

*       first for an image display with a few buttons, and..

         DO  I=1, NIMGMS
            CALL MSG_SETC( 'IMGMSG', IMGMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL='CHOICEID'//IC( :NC )

            CALL MSG_OUT( LABEL, '^IMGMSG', STATUS )
         END DO

*       Set the flag to say the cursor is ready for use.

         CURSOR=.TRUE.

*       Nominally an image display.

         IMGDIS=.FALSE.
      ELSE

*       a terminal with many choices.

*       First validate list of buttons.

         NC=CHR_LEN( BUTTNS )
         IF ( NC .LT. MNCHOI ) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP( 'PRPCUR__PROG',
     :        'PRPCUR: Programmer error.  Check calling arguments',
     :         STATUS )
            GOTO 999
         END IF

*       Trim the button list if necessary.

         IF ( NC .GT. MALT + 1 ) THEN
            BUTLST=BUTTNS( :MNCHOI ) //BUTTNS( NC:NC )
         ELSE
            BUTLST=BUTTNS
         END IF

*       Ensure that the messages below appear before activating the
*       cursor, otherwise they may appear on the graphics plane instead
*       of the alpha plane. This is a two-part operation. First we
*       need to give time to switch to the alpha plane.

         CALL MSG_SYNC( STATUS )

         DO  I=1, NTERMS
            CALL MSG_SETC( 'TERMSG', TERMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL='CHOICETERM'//IC( :NC )

            CALL MSG_OUT( LABEL, '^TERMSG', STATUS )
         END DO

*       The part is to wait for the messages to appear before returning
*       to graphics plane.

         CALL MSG_SYNC( STATUS )

*       Activate the cursor

         CALL SGS_CUVIS( .TRUE. )
         CALL SGS_SELCH( 0 )
         CALL SGS_DEFCH( BUTLST )

*       Set the flag to say the cursor is ready for use.

         CURSOR=.TRUE.
      END IF

 999  CONTINUE

      END


      SUBROUTINE ELP1_PRPCUR ( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
     :                    NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*+
*    Description :
*
*     This determines whether a cursor with a suitable number of choices
*     is available on the current graphics device.  Messages are given
*     describing which buttons to press if the device is a terminal or
*     an image display.  The messages has parameters CHOICETERMn or
*     CHOICEIDn, where n is number of the message starting from 1.
*
*    Invocation :
*
*     CALL ELP1_PRPCUR( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
*    :             NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*
*    Parameters :
*
*     MNCHOI=INTEGER (Given)
*        The minimum number of choices required by the calling
*          application.  It must be positive.
*     SWCHOI=INTEGER (Given)
*        The maximum number of choices for the graphics-device to be an
*          image display. It must be at least %MNCHOI.
*     TERMES( NTERMS )=CHARACTER (Given)
*        Description of which terminal buttons to press to obtain the
*          various choices, to be reported to the user if the device
*          is nominally a terminal, i.e. its number of choices exceeds
*          %SWCHOI.
*     NTERMS=INTEGER (Given)
*        Number of lines describing the action of the terminal choices.
*     IMGMES( NIMGMS )=CHARACTER (Given)
*        Description of the action of the mouse or trackerball buttons
*          to be reported to the user if the device is nominally an
*          image display, i.e. its number of choices is less than or
*          equal to %SWCHOI.
*     NIMGMS=INTEGER (Given)
*        Number of lines describing the action of the image-display
*          choices.
*     BUTTNS=CHARACTER (Given)
*        The terminal buttons to be pressed to obtain the different
*          choices, e.g. '1A.' would mean '1' would give the first
*          choice, 'A' would the second and '.' to exit. A fullstop
*          is the recommended Starlink method for terminating such an
*          interaction.  The last character is assumed to be the exit
*          choice in cases where this string is longer than the number
*          of choices plus one (the exit).
*          characters.  There must be at least %MNCHOI+1 characters.
*          This string is ignored if the device is an image display.
*     CURSOR=LOGICAL (Returned)
*        If true there is a suitable cursor and number of choices.
*     IMGDIS=LOGICAL (Returned)
*        If true the choice device is an image-display mouse or
*          trackerball
*     DEVICE=DEVICE (Given)
*        The graphics workstation.
*
*    Arguments :
*
*     STATUS=INTEGER (Given and Returned)
*        The global status.
*
*    Method :
*
*     If status is bad then exit
*     Validate input data
*     Determine the number of options on the workstation's choice device
*     If the number of choices is less than specified minimum then
*       report error context and abort
*     Activate the cursor and specify the options depending on the
*       number of choices and set cursor-ready flag
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK  (RAL::CUR)
*
*    History :
*
*     1989 Nov 10: Original version (RAL::CUR).
*
*    Type definitions :
      IMPLICIT NONE              ! No implicit typing

*    Global Constants :
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*    Import :
      INTEGER MNCHOI             ! Minimum number of choices
      INTEGER SWCHOI             ! Maximum number fo choices if the
                                 ! device is to be classed as an image
                                 ! display
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages

      CHARACTER *(*) TERMES( NTERMS )
                                 ! Informational messages if device is
                                 ! a terminal
      CHARACTER *(*) IMGMES( NTERMS )
                                 ! Informational messages if device is
                                 ! an image display
      CHARACTER *(*) BUTTNS      ! Choices buttons for a terminal.

*    Export :
      LOGICAL CURSOR             ! Device has a sutiable cursor and
                                 ! choices
      LOGICAL IMGDIS             ! Device is an image-display for the
                                 ! purpose of using the cursor

*    Status :
      INTEGER STATUS             ! Global status

*    External references :
      INTEGER
     :  CHR_LEN

*    Local variables :
      CHARACTER*80 BUTLST        ! List of buttons which may be a
                                 ! trimmed version of the input list
      CHARACTER DATREC(10)*80    ! Data record return by GKS inquiry

      INTEGER CONID              ! Connection identifier
      INTEGER GSTAT              ! Graphics status
      INTEGER I                  ! Loop index
      CHARACTER*4 IC             ! Message counter
      CHARACTER*14 LABEL         ! Informational-message parameter
      INTEGER LDR                ! Length of data record returned by
                                 ! GKS inquiry
      INTEGER MALT               ! Number of alternatives for choice
                                 ! input on graphics device
      INTEGER NC                 ! Number of characters in a string
      INTEGER OL                 ! Number of available prompt/echo types
                                 ! for graphics device
      INTEGER PET                ! Element of prompt/echo types of
                                 ! device returned by GKS inquiry
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WTYPE              ! Workstation type

      REAL EAREA( 4 )            ! Graphics device echo area

                                 ! True if:
      LOGICAL CURAVA             ! A cursor is available

*-

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CURSOR=.FALSE.
      IMGDIS=.FALSE.

*    Validate input data.

      IF ( MNCHOI .LT. 1 .OR. SWCHOI .LT. MNCHOI ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__PROG',
     :     'PRPCUR: Programmer error.  Check calling arguments',
     :      STATUS )
         GOTO 999
      END IF

*    Put out a blank line to ensure the commentary appears on the alpha
*    plane of the terminal.

      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Is there a cursor?

      CURAVA=.FALSE.
      CALL SGS_ICUAV( CURAVA )

      IF ( .NOT. CURAVA ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__NOCUR',
     :     'PRPCUR: Chosen workstation does not have a cursor.',
     :     STATUS )

         GOTO 999
      END IF

      CALL SGS_ICURW( WKID )

*    Find workstation type

      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*    Find number of options on choice device

      CALL GQDCH( WTYPE, 1, 1, 10, GSTAT, MALT, OL, PET, EAREA, LDR,
     :            DATREC )

*    At least one choice required

      IF ( MALT .LT. MNCHOI ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__WDV',
     :     'PRPCUR: Graphics device chosen has unsuitable choice '/
     :     /'device (e.g. mouse or trackerball) for this application.',
     :     STATUS )

         GOTO 999

*       Tell the user what to do...

      ELSE IF ( MALT .LE. SWCHOI ) THEN

*       first for an image display with a few buttons, and...

         DO  I=1, NIMGMS
            CALL MSG_SETC( 'IMGMSG', IMGMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL='CHOICEID'//IC( :NC )

            CALL MSG_OUT( LABEL, '^IMGMSG', STATUS )
         END DO

*       Set the flag to say the cursor is ready for use.

         CURSOR=.TRUE.

*       Nominally an image display.

         IMGDIS=.FALSE.
      ELSE

*       a terminal with many choices.

*       First validate list of buttons.

         NC=CHR_LEN( BUTTNS )
         IF ( NC .LT. MNCHOI ) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP( 'PRPCUR__PROG',
     :        'PRPCUR: Programmer error.  Check calling arguments',
     :         STATUS )
            GOTO 999
         END IF

*       Trim the button list if necessary.

         IF ( NC .GT. MALT + 1 ) THEN
            BUTLST=BUTTNS( :MNCHOI ) //BUTTNS( NC:NC )
         ELSE
            BUTLST=BUTTNS
         END IF

*       Ensure that the messages below appear before activating the
*       cursor, otherwise they may appear on the graphics plane instead
*       of the alpha plane. This is a two-part operation. First we
*       need to give time to switch to the alpha plane.

         CALL MSG_SYNC( STATUS )

         DO  I=1, NTERMS
            CALL MSG_SETC( 'TERMSG', TERMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL='CHOICETERM'//IC( :NC )

            CALL MSG_OUT( LABEL, '^TERMSG', STATUS )
         END DO

*       The part is to wait for the messages to appear before returning
*       to graphics plane.

         CALL MSG_SYNC( STATUS )

*       Activate the cursor

         CALL SGS_CUVIS( .TRUE. )
         CALL SGS_SELCH( 0 )
         CALL SGS_DEFCH( BUTLST )

*       Set the flag to say the cursor is ready for use.

         CURSOR=.TRUE.
      END IF

 999  CONTINUE

      END

************************************
*** KAPPA/KAPGEN CODE ADDED HERE ***
************************************

      SUBROUTINE GAU1_PRPCUR ( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
     :                    NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*+
*    Description :
*
*     This determines whether a cursor with a suitable number of choices
*     is available on the current graphics device.  Messages are given
*     describing which buttons to press if the device is a terminal or
*     an image display.  The messages has parameters CHOICETERMn or
*     CHOICEIDn, where n is number of the message starting from 1.
*
*    Invocation :
*
*     CALL GAU1_PRPCUR( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
*    :             NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*
*    Parameters :
*
*     MNCHOI=INTEGER (Given)
*        The minimum number of choices required by the calling
*          application.  It must be positive.
*     SWCHOI=INTEGER (Given)
*        The maximum number of choices for the graphics-device to be an
*          image display. It must be at least %MNCHOI.
*     TERMES( NTERMS )=CHARACTER (Given)
*        Description of which terminal buttons to press to obtain the
*          various choices, to be reported to the user if the device
*          is nominally a terminal, i.e. its number of choices exceeds
*          %SWCHOI.
*     NTERMS=INTEGER (Given)
*        Number of lines describing the action of the terminal choices.
*     IMGMES( NIMGMS )=CHARACTER (Given)
*        Description of the action of the mouse or trackerball buttons
*          to be reported to the user if the device is nominally an
*          image display, i.e. its number of choices is less than or
*          equal to %SWCHOI.
*     NIMGMS=INTEGER (Given)
*        Number of lines describing the action of the image-display
*          choices.
*     BUTTNS=CHARACTER (Given)
*        The terminal buttons to be pressed to obtain the different
*          choices, e.g. '1A.' would mean '1' would give the first
*          choice, 'A' would the second and '.' to exit. A fullstop
*          is the recommended Starlink method for terminating such an
*          interaction.  The last character is assumed to be the exit
*          choice in cases where this string is longer than the number
*          of choices plus one (the exit).
*          characters.  There must be at least %MNCHOI+1 characters.
*          This string is ignored if the device is an image display.
*     CURSOR=LOGICAL (Returned)
*        If true there is a suitable cursor and number of choices.
*     IMGDIS=LOGICAL (Returned)
*        If true the choice device is an image-display mouse or
*          trackerball
*     DEVICE=DEVICE (Given)
*        The graphics workstation.
*
*    Arguments :
*
*     STATUS=INTEGER (Given and Returned)
*        The global status.
*
*    Method :
*
*     If status is bad then exit
*     Validate input data
*     Determine the number of options on the workstation's choice device
*     If the number of choices is less than specified minimum then
*       report error context and abort
*     Activate the cursor and specify the options depending on the
*       number of choices and set cursor-ready flag
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK  (RAL::CUR)
*
*    History :
*
*     1989 Nov 10: Original version (RAL::CUR).
*
*    Type definitions :
      IMPLICIT NONE              ! No implicit typing

*    Global Constants :
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*    Import :
      INTEGER MNCHOI             ! Minimum number of choices
      INTEGER SWCHOI             ! Maximum number fo choices if the
                                 ! device is to be classed as an image
                                 ! display
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages

      CHARACTER *(*) TERMES( NTERMS )
                                 ! Informational messages if device is
                                 ! a terminal
      CHARACTER *(*) IMGMES( NTERMS )
                                 ! Informational messages if device is
                                 ! an image display
      CHARACTER *(*) BUTTNS      ! Choices buttons for a terminal.

*    Export :
      LOGICAL CURSOR             ! Device has a sutiable cursor and
                                 ! choices
      LOGICAL IMGDIS             ! Device is an image-display for the
                                 ! purpose of using the cursor

*    Status :
      INTEGER STATUS             ! Global status

*    External references :
      INTEGER
     :  CHR_LEN

*    Local variables :
      CHARACTER*80 BUTLST        ! List of buttons which may be a
                                 ! trimmed version of the input list
      CHARACTER DATREC(10)*80    ! Data record return by GKS inquiry

      INTEGER CONID              ! Connection identifier
      INTEGER GSTAT              ! Graphics status
      INTEGER I                  ! Loop index
      CHARACTER*4 IC             ! Message counter
      CHARACTER*14 LABEL         ! Informational-message parameter
      INTEGER LDR                ! Length of data record returned by
                                 ! GKS inquiry
      INTEGER MALT               ! Number of alternatives for choice
                                 ! input on graphics device
      INTEGER NC                 ! Number of characters in a string
      INTEGER OL                 ! Number of available prompt/echo types
                                 ! for graphics device
      INTEGER PET                ! Element of prompt/echo types of
                                 ! device returned by GKS inquiry
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WTYPE              ! Workstation type

      REAL EAREA( 4 )            ! Graphics device echo area

                                 ! True if:
      LOGICAL CURAVA             ! A cursor is available

*-

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CURSOR=.FALSE.
      IMGDIS=.FALSE.

*    Validate input data.

      IF ( MNCHOI .LT. 1 .OR. SWCHOI .LT. MNCHOI ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__PROG',
     :     'PRPCUR: Programmer error.  Check calling arguments',
     :      STATUS )
         GOTO 999
      END IF

*    Put out a blank line to ensure the commentary appears on the alpha
*    plane of the terminal.

      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Is there a cursor?

      CURAVA=.FALSE.
      CALL SGS_ICUAV( CURAVA )

      IF ( .NOT. CURAVA ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__NOCUR',
     :     'PRPCUR: Chosen workstation does not have a cursor.',
     :     STATUS )

         GOTO 999
      END IF

      CALL SGS_ICURW( WKID )

*   Find workstation type

      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*    Find number of options on choice device

      CALL GQDCH( WTYPE, 1, 1, 10, GSTAT, MALT, OL, PET, EAREA, LDR,
     :            DATREC )

*    At least one choice required

      IF ( MALT .LT. MNCHOI ) THEN
         STATUS=SAI__ERROR
         CALL ERR_REP( 'PRPCUR__WDV',
     :     'PRPCUR: Graphics device chosen has unsuitable choice '/
     :     /'device (e.g. mouse or trackerball) for this application.',
     :     STATUS )

         GOTO 999

*       Tell the user what to do...

      ELSE IF ( MALT .LE. SWCHOI ) THEN

*       first for an image display with a few buttons, and...

         DO  I=1, NIMGMS
            CALL MSG_SETC( 'IMGMSG', IMGMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL='CHOICEID'//IC( :NC )

            CALL MSG_OUT( LABEL, '^IMGMSG', STATUS )
         END DO

*       Set the flag to say the cursor is ready for use.

         CURSOR=.TRUE.

*       Nominally an image display.

         IMGDIS=.FALSE.
      ELSE

*       a terminal with many choices.

*       First validate list of buttons.

         NC=CHR_LEN( BUTTNS )
         IF ( NC .LT. MNCHOI ) THEN
            STATUS=SAI__ERROR
            CALL ERR_REP( 'PRPCUR__PROG',
     :        'PRPCUR: Programmer error.  Check calling arguments',
     :         STATUS )
            GOTO 999
         END IF

*       Trim the button list if necessary.

         IF ( NC .GT. MALT + 1 ) THEN
            BUTLST=BUTTNS( :MNCHOI ) //BUTTNS( NC:NC )
         ELSE
            BUTLST=BUTTNS
         END IF

*       Ensure that the messages below appear before activating the
*       cursor, otherwise they may appear on the graphics plane instead
*       of the alpha plane. This is a two-part operation. First we
*       need to give time to switch to the alpha plane.

         CALL MSG_SYNC( STATUS )

         DO  I=1, NTERMS
            CALL MSG_SETC( 'TERMSG', TERMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL='CHOICETERM'//IC( :NC )

            CALL MSG_OUT( LABEL, '^TERMSG', STATUS )
         END DO

*       The part is to wait for the messages to appear before returning
*       to graphics plane.

         CALL MSG_SYNC( STATUS )

*       Activate the cursor

         CALL SGS_CUVIS( .TRUE. )
         CALL SGS_SELCH( 0 )
         CALL SGS_DEFCH( BUTLST )

*       Set the flag to say the cursor is ready for use.

         CURSOR=.TRUE.
      END IF

 999  CONTINUE

      END


      SUBROUTINE GRA1_PRPCUR ( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
     :                    NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*+
*    Description :
*
*     This determines whether a cursor with a suitable number of choices
*     is available on the current graphics device.  Messages are given
*     describing which buttons to press if the device is a terminal or
*     an image display.  The messages has parameters CHOICETERMn or
*     CHOICEIDn, where n is number of the message starting from 1.
*
*    Invocation :
*
*     CALL GRA1_PRPCUR( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
*    :             NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*
*    Parameters :
*
*     MNCHOI = INTEGER (Given)
*        The minimum number of choices required by the calling
*          application.  It must be positive.
*     SWCHOI = INTEGER (Given)
*        The maximum number of choices for the graphics-device to be an
*          image display. It must be at least %MNCHOI.
*     TERMES( NTERMS ) = CHARACTER (Given)
*        Description of which terminal buttons to press to obtain the
*          various choices, to be reported to the user if the device
*          is nominally a terminal, i.e. its number of choices exceeds
*          %SWCHOI.
*     NTERMS = INTEGER (Given)
*        Number of lines describing the action of the terminal choices.
*     IMGMES( NIMGMS ) = CHARACTER (Given)
*        Description of the action of the mouse or trackerball buttons
*          to be reported to the user if the device is nominally an
*          image display, i.e. its number of choices is less than or
*          equal to %SWCHOI.
*     NIMGMS = INTEGER (Given)
*        Number of lines describing the action of the image-display
*          choices.
*     BUTTNS = CHARACTER (Given)
*        The terminal buttons to be pressed to obtain the different
*          choices, e.g. '1A.' would mean '1' would give the first
*          choice, 'A' would the second and '.' to exit. A fullstop
*          is the recommended Starlink method for terminating such an
*          interaction.  The last character is assumed to be the exit
*          choice in cases where this string is longer than the number
*          of choices plus one (the exit).
*          characters.  There must be at least %MNCHOI+1 characters.
*          This string is ignored if the device is an image display.
*     CURSOR = LOGICAL (Returned)
*        If true there is a suitable cursor and number of choices.
*     IMGDIS = LOGICAL (Returned)
*        If true the choice device is an image-display mouse or
*          trackerball
*     DEVICE = DEVICE (Given)
*        The graphics workstation.
*
*    Arguments :
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*    Method :
*
*     If status is bad then exit
*     Validate input data
*     Determine the number of options on the workstation's choice device
*     If the number of choices is less than specified minimum then
*       report error context and abort
*     Activate the cursor and specify the options depending on the
*       number of choices and set cursor-ready flag
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK  (RAL::CUR)
*
*    History :
*
*     1989 Nov 10: Original version (RAL::CUR).
*
*    Type definitions :
      IMPLICIT NONE              ! No implicit typing

*    Global Constants :
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*    Import :
      INTEGER MNCHOI             ! Minimum number of choices
      INTEGER SWCHOI             ! Maximum number fo choices if the
                                 ! device is to be classed as an image
                                 ! display
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages

      CHARACTER *(*) TERMES( NTERMS )
                                 ! Informational messages if device is
                                 ! a terminal
      CHARACTER *(*) IMGMES( NTERMS )
                                 ! Informational messages if device is
                                 ! an image display
      CHARACTER *(*) BUTTNS      ! Choices buttons for a terminal.

*    Export :
      LOGICAL CURSOR             ! Device has a sutiable cursor and
                                 ! choices
      LOGICAL IMGDIS             ! Device is an image-display for the
                                 ! purpose of using the cursor

*    Status :
      INTEGER STATUS             ! Global status

*    External references :
      INTEGER
     :  CHR_LEN

*    Local variables :
      CHARACTER*80 BUTLST        ! List of buttons which may be a
                                 ! trimmed version of the input list
      CHARACTER DATREC(10)*80    ! Data record return by GKS inquiry

      INTEGER CONID              ! Connection identifier
      INTEGER GSTAT              ! Graphics status
      INTEGER I                  ! Loop index
      CHARACTER*4 IC             ! Message counter
      CHARACTER*14 LABEL         ! Informational-message parameter
      INTEGER LDR                ! Length of data record returned by
                                 ! GKS inquiry
      INTEGER MALT               ! Number of alternatives for choice
                                 ! input on graphics device
      INTEGER NC                 ! Number of characters in a string
      INTEGER OL                 ! Number of available prompt/echo types
                                 ! for graphics device
      INTEGER PET                ! Element of prompt/echo types of
                                 ! device returned by GKS inquiry
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WTYPE              ! Workstation type

      REAL EAREA( 4 )            ! Graphics device echo area

                                 ! True if:
      LOGICAL CURAVA             ! A cursor is available

*-

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CURSOR = .FALSE.
      IMGDIS = .FALSE.

*    Validate input data.

      IF ( MNCHOI .LT. 1 .OR. SWCHOI .LT. MNCHOI ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PRPCUR__PROG',
     :     'PRPCUR: Programmer error.  Check calling arguments',
     :      STATUS )
         GOTO 999
      END IF

*    Put out a blank line to ensure the commentary appears on the alpha
*    plane of the terminal.

      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Is there a cursor?

      CURAVA = .FALSE.
      CALL SGS_ICUAV( CURAVA )

      IF ( .NOT. CURAVA ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PRPCUR__NOCUR',
     :     'PRPCUR: Chosen workstation does not have a cursor.',
     :     STATUS )

         GOTO 999
      END IF

      CALL SGS_ICURW( WKID )

*    Find workstation type

      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*    Find number of options on choice device

      CALL GQDCH( WTYPE, 1, 1, 10, GSTAT, MALT, OL, PET, EAREA, LDR,
     :            DATREC )

*    At least one choice required

      IF ( MALT .LT. MNCHOI ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PRPCUR__WDV',
     :     'PRPCUR: Graphics device chosen has unsuitable choice '/
     :     /'device (e.g. mouse or trackerball) for this application.',
     :     STATUS )

         GOTO 999

*       Tell the user what to do...

      ELSE IF ( MALT .LE. SWCHOI ) THEN

*       first for an image display with a few buttons, and...

         DO  I = 1, NIMGMS
            CALL MSG_SETC( 'IMGMSG', IMGMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL = 'CHOICEID'//IC( :NC )

            CALL MSG_OUT( LABEL, '^IMGMSG', STATUS )
         END DO

*       Set the flag to say the cursor is ready for use.

         CURSOR = .TRUE.

*       Nominally an image display.

         IMGDIS = .FALSE.
      ELSE

*       a terminal with many choices.

*       First validate list of buttons.

         NC = CHR_LEN( BUTTNS )
         IF ( NC .LT. MNCHOI ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PRPCUR__PROG',
     :        'PRPCUR: Programmer error.  Check calling arguments',
     :         STATUS )
            GOTO 999
         END IF

*       Trim the button list if necessary.

         IF ( NC .GT. MALT + 1 ) THEN
            BUTLST = BUTTNS( :MNCHOI ) //BUTTNS( NC:NC )
         ELSE
            BUTLST = BUTTNS
         END IF

*       Ensure that the messages below appear before activating the
*       cursor, otherwise they may appear on the graphics plane instead
*       of the alpha plane. This is a two-part operation. First we
*       need to give time to switch to the alpha plane.

         CALL MSG_SYNC( STATUS )

         DO  I = 1, NTERMS
            CALL MSG_SETC( 'TERMSG', TERMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL = 'CHOICETERM'//IC( :NC )

            CALL MSG_OUT( LABEL, '^TERMSG', STATUS )
         END DO

*       The part is to wait for the messages to appear before returning
*       to graphics plane.

         CALL MSG_SYNC( STATUS )

*       Activate the cursor

         CALL SGS_CUVIS( .TRUE. )
         CALL SGS_SELCH( 0 )
         CALL SGS_DEFCH( BUTLST )

*       Set the flag to say the cursor is ready for use.

         CURSOR = .TRUE.
      END IF

 999  CONTINUE

      END


      SUBROUTINE SEC1_PRPCUR ( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
     :                    NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*+
*    Description :
*
*     This determines whether a cursor with a suitable number of choices
*     is available on the current graphics device.  Messages are given
*     describing which buttons to press if the device is a terminal or
*     an image display.  The messages has parameters CHOICETERMn or
*     CHOICEIDn, where n is number of the message starting from 1.
*
*    Invocation :
*
*     CALL SEC1_PRPCUR( MNCHOI, SWCHOI, TERMES, NTERMS, IMGMES,
*    :             NIMGMS, BUTTNS, CURSOR, IMGDIS, STATUS )
*
*    Parameters :
*
*     MNCHOI = INTEGER (Given)
*        The minimum number of choices required by the calling
*          application.  It must be positive.
*     SWCHOI = INTEGER (Given)
*        The maximum number of choices for the graphics-device to be an
*          image display. It must be at least %MNCHOI.
*     TERMES( NTERMS ) = CHARACTER (Given)
*        Description of which terminal buttons to press to obtain the
*          various choices, to be reported to the user if the device
*          is nominally a terminal, i.e. its number of choices exceeds
*          %SWCHOI.
*     NTERMS = INTEGER (Given)
*        Number of lines describing the action of the terminal choices.
*     IMGMES( NIMGMS ) = CHARACTER (Given)
*        Description of the action of the mouse or trackerball buttons
*          to be reported to the user if the device is nominally an
*          image display, i.e. its number of choices is less than or
*          equal to %SWCHOI.
*     NIMGMS = INTEGER (Given)
*        Number of lines describing the action of the image-display
*          choices.
*     BUTTNS = CHARACTER (Given)
*        The terminal buttons to be pressed to obtain the different
*          choices, e.g. '1A.' would mean '1' would give the first
*          choice, 'A' would the second and '.' to exit. A fullstop
*          is the recommended Starlink method for terminating such an
*          interaction.  The last character is assumed to be the exit
*          choice in cases where this string is longer than the number
*          of choices plus one (the exit).
*          characters.  There must be at least %MNCHOI+1 characters.
*          This string is ignored if the device is an image display.
*     CURSOR = LOGICAL (Returned)
*        If true there is a suitable cursor and number of choices.
*     IMGDIS = LOGICAL (Returned)
*        If true the choice device is an image-display mouse or
*          trackerball
*     DEVICE = DEVICE (Given)
*        The graphics workstation.
*
*    Arguments :
*
*     STATUS = INTEGER (Given and Returned)
*        The global status.
*
*    Method :
*
*     If status is bad then exit
*     Validate input data
*     Determine the number of options on the workstation's choice device
*     If the number of choices is less than specified minimum then
*       report error context and abort
*     Activate the cursor and specify the options depending on the
*       number of choices and set cursor-ready flag
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Malcolm J. Currie  STARLINK  (RAL::CUR)
*
*    History :
*
*     1989 Nov 10: Original version (RAL::CUR).
*
*    Type definitions :
      IMPLICIT NONE              ! No implicit typing

*    Global Constants :
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*    Import :
      INTEGER MNCHOI             ! Minimum number of choices
      INTEGER SWCHOI             ! Maximum number fo choices if the
                                 ! device is to be classed as an image
                                 ! display
      INTEGER NTERMS             ! Number of lines of terminal messages
      INTEGER NIMGMS             ! Number of lines of image-display
                                 ! messages

      CHARACTER *(*) TERMES( NTERMS )
                                 ! Informational messages if device is
                                 ! a terminal
      CHARACTER *(*) IMGMES( NTERMS )
                                 ! Informational messages if device is
                                 ! an image display
      CHARACTER *(*) BUTTNS      ! Choices buttons for a terminal.

*    Export :
      LOGICAL CURSOR             ! Device has a sutiable cursor and
                                 ! choices
      LOGICAL IMGDIS             ! Device is an image-display for the
                                 ! purpose of using the cursor

*    Status :
      INTEGER STATUS             ! Global status

*    External references :
      INTEGER
     :  CHR_LEN

*    Local variables :
      CHARACTER*80 BUTLST        ! List of buttons which may be a
                                 ! trimmed version of the input list
      CHARACTER DATREC(10)*80    ! Data record return by GKS inquiry

      INTEGER CONID              ! Connection identifier
      INTEGER GSTAT              ! Graphics status
      INTEGER I                  ! Loop index
      CHARACTER*4 IC             ! Message counter
      CHARACTER*14 LABEL         ! Informational-message parameter
      INTEGER LDR                ! Length of data record returned by
                                 ! GKS inquiry
      INTEGER MALT               ! Number of alternatives for choice
                                 ! input on graphics device
      INTEGER NC                 ! Number of characters in a string
      INTEGER OL                 ! Number of available prompt/echo types
                                 ! for graphics device
      INTEGER PET                ! Element of prompt/echo types of
                                 ! device returned by GKS inquiry
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WTYPE              ! Workstation type

      REAL EAREA( 4 )            ! Graphics device echo area

                                 ! True if:
      LOGICAL CURAVA             ! A cursor is available

*-

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

      CURSOR = .FALSE.
      IMGDIS = .FALSE.

*    Validate input data.

      IF ( MNCHOI .LT. 1 .OR. SWCHOI .LT. MNCHOI ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PRPCUR__PROG',
     :     'PRPCUR: Programmer error.  Check calling arguments',
     :      STATUS )
         GOTO 999
      END IF

*    Put out a blank line to ensure the commentary appears on the alpha
*    plane of the terminal.

      CALL MSG_OUT( 'BLANK', ' ', STATUS )

*    Is there a cursor?

      CURAVA = .FALSE.
      CALL SGS_ICUAV( CURAVA )

      IF ( .NOT. CURAVA ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PRPCUR__NOCUR',
     :     'PRPCUR: Chosen workstation does not have a cursor.',
     :     STATUS )

         GOTO 999
      END IF

      CALL SGS_ICURW( WKID )

*    Find workstation type

      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*    Find number of options on choice device

      CALL GQDCH( WTYPE, 1, 1, 10, GSTAT, MALT, OL, PET, EAREA, LDR,
     :            DATREC )

*    At least one choice required

      IF ( MALT .LT. MNCHOI ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PRPCUR__WDV',
     :     'PRPCUR: Graphics device chosen has unsuitable choice '/
     :     /'device (e.g. mouse or trackerball) for this application.',
     :     STATUS )

         GOTO 999

*       Tell the user what to do...

      ELSE IF ( MALT .LE. SWCHOI ) THEN

*       first for an image display with a few buttons, and...

         DO  I = 1, NIMGMS
            CALL MSG_SETC( 'IMGMSG', IMGMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL = 'CHOICEID'//IC( :NC )

            CALL MSG_OUT( LABEL, '^IMGMSG', STATUS )
         END DO

*       Set the flag to say the cursor is ready for use.

         CURSOR = .TRUE.

*       Nominally an image display.

         IMGDIS = .FALSE.
      ELSE

*       a terminal with many choices.

*       First validate list of buttons.

         NC = CHR_LEN( BUTTNS )
         IF ( NC .LT. MNCHOI ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PRPCUR__PROG',
     :        'PRPCUR: Programmer error.  Check calling arguments',
     :         STATUS )
            GOTO 999
         END IF

*       Trim the button list if necessary.

         IF ( NC .GT. MALT + 1 ) THEN
            BUTLST = BUTTNS( :MNCHOI ) //BUTTNS( NC:NC )
         ELSE
            BUTLST = BUTTNS
         END IF

*       Ensure that the messages below appear before activating the
*       cursor, otherwise they may appear on the graphics plane instead
*       of the alpha plane. This is a two-part operation. First we
*       need to give time to switch to the alpha plane.

         CALL MSG_SYNC( STATUS )

         DO  I = 1, NTERMS
            CALL MSG_SETC( 'TERMSG', TERMES( I ) )
            CALL CHR_ITOC( I, IC, NC )
            LABEL = 'CHOICETERM'//IC( :NC )

            CALL MSG_OUT( LABEL, '^TERMSG', STATUS )
         END DO

*       The part is to wait for the messages to appear before returning
*       to graphics plane.

         CALL MSG_SYNC( STATUS )

*       Activate the cursor

         CALL SGS_CUVIS( .TRUE. )
         CALL SGS_SELCH( 0 )
         CALL SGS_DEFCH( BUTLST )

*       Set the flag to say the cursor is ready for use.

         CURSOR = .TRUE.
      END IF

 999  CONTINUE

      END
