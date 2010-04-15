      SUBROUTINE PHO1_PRCUR( MNCHOI, BUTTNS, CURSOR, IMGDIS, STATUS )
*+
*  Name:
*     PHO1_PRCUR

*  Purpose:
*     Prepares the graphics cursor to select points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PHO1_PRCUR( MNCHOI, BUTTNS, CURSOR, IMGDIS, STATUS )

*  Description:
*     This determines whether a cursor with a suitable number of choices
*     is available on the current graphics device.  A workstation when
*     more than one choice is needed, is classified as a terminal by
*     this routine; and it is classified as an image-display when only
*     one choice is needed.  In the former case pressing the left-hand
*     and right-hand mouse buttons will still give the first choice and
*     the exit respectively.

*  Arguments:
*     MNCHOI = INTEGER (Given)
*        The minimum number of choices required by the calling
*        application.  It must be positive.
*     BUTTNS = CHARACTER (Given)
*        The terminal buttons to be pressed to obtain the different
*        choices, e.g. '1A.' would mean '1' would give the first
*        choice, 'A' would the second and '.' to exit.  A fullstop is
*        the recommended Starlink method for terminating such an
*        interaction.  The last character is assumed to be the exit
*        choice in cases where this string is longer than the number of
*        choices plus one (the exit).  There must be at least %MNCHOI+1
*        characters.  This string is ignored if the device is an image
*        display, however for a workstation give at least one choice
*        and the exit, e.g. ' .'.
*     CURSOR = LOGICAL (Returned)
*        If true there is a suitable cursor and number of choices.
*     IMGDIS = LOGICAL (Returned)
*        If true the choice device is an image-display mouse or
*        trackerball.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Algorithm:
*     If status is bad then exit
*     Validate input data
*     Determine the number of options on the workstation's choice device
*        and inquire the workstation class
*     If the number of choices is less than specified minimum then
*       report error context and abort
*     Report the user instructions depending on the class of the device.
*     For terminals activate the cursor and specify the options
*       depending on the number of choices and set cursor-ready flag
*     End

*  Prior Requirements:
*     A SGS (GKS) workstation must be active.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     PWD: Peter W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     1994 October 24 (MJC):
*        Original version based on PRPCUR.  Note that PRPCUR's second
*        argument is missing from this routine.
*     1995 December 16 (MJC):
*        Increased from 1 to 2 the maximum number of choices to decide
*        whether or not to assign the windows device to be a terminal
*        or an image display for the cursor interaction.
*     1996 November 8 (PWD):
*        Taken from KAPPA an renamed PHO1_PRCUR (was KPG1_PRCUR) for
*        use in PHOTOM.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing


*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GNS_PAR'          ! Graphics name system constants

*  Arguments Given:
      INTEGER MNCHOI             ! Minimum number of choices
      CHARACTER * ( * ) BUTTNS   ! Choices buttons for a terminal.

*  Arguments Returned:
      LOGICAL CURSOR             ! Device has a sutiable cursor and
                                 ! choices
      LOGICAL IMGDIS             ! Device is an image-display for the
                                 ! purpose of using the cursor

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Effective length of a string

*  Local Constants:
      INTEGER MOUSCH             ! Number of choices on mouse (assumes
                                 ! GKS 7.4 and three buttons)
      PARAMETER ( MOUSCH = 2 )

*  Local Variables:
      CHARACTER * ( 80 ) BUTLST  ! List of buttons which may be a
                                 ! trimmed version of the input list
      LOGICAL CURAVA             ! True if a cursor is available
      INTEGER CONID              ! Connection identifier
      CHARACTER * ( 80 ) DATREC( 10 )! Data record return by GKS inquiry
      REAL EAREA( 4 )            ! Graphics device echo area
      INTEGER GSTAT              ! Graphics status
      INTEGER I                  ! Loop index
      CHARACTER * ( 4 ) IC       ! Message counter
      CHARACTER * ( 14 ) LABEL   ! Informational-message parameter
      INTEGER LDR                ! Length of data record returned by
                                 ! GKS inquiry
      INTEGER MALT               ! Number of alternatives for choice
                                 ! input on graphics device
      INTEGER NC                 ! Number of characters in a string
      INTEGER OL                 ! Number of available prompt/echo types
                                 ! for graphics device
      INTEGER PET                ! Element of prompt/echo types of
                                 ! device returned by GKS inquiry
      CHARACTER * ( GNS__SZKEY ) WKCLAS ! The name of the workstation
      INTEGER WKID               ! GKS workstation identifier
      INTEGER WTYPE              ! Workstation type

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise returned values.
      CURSOR = .FALSE.
      IMGDIS = .FALSE.

*  Validate the input data.
      IF ( MNCHOI .LT. 1 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PHO1_PRCUR_PROG',
     :     'PHO1_PRCUR: Programmer error.  Check calling arguments',
     :      STATUS )
         GOTO 999
      END IF

*  Is there a cursor?
      CURAVA = .FALSE.
      CALL SGS_ICUAV( CURAVA )

*  Report an error if there is no cursor.
      IF ( .NOT. CURAVA ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'PHO1_PRCUR_NOCUR',
     :     'PHO1_PRCUR: Chosen workstation does not have a cursor.',
     :     STATUS )

         GOTO 999
      END IF

*  Inquire the workstation identifier from GKS.
      CALL SGS_ICURW( WKID )

*  Start the GNS system for GKS.
      CALL GNS_START( 'GKS', STATUS )

*  Find out which type of device this is.
      CALL GNS_IWCG( WKID, 'CLASS', WKCLAS, STATUS )

*  Stop the GNS system for GKS.
      CALL GNS_STOP( 'GKS', STATUS )

*  Find the workstation's type.
      CALL GQWKC( WKID, GSTAT, CONID, WTYPE )

*  Find the number of options on the choice device.
      CALL GQDCH( WTYPE, 1, 1, 10, GSTAT, MALT, OL, PET, EAREA, LDR,
     :            DATREC )

*  There must be sufficient choices.  Report the error.
      IF ( MALT .LT. MNCHOI ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'M', MALT )
         CALL MSG_SETI( 'N', MNCHOI )
         CALL ERR_REP( 'PHO1_PRCUR_WDV',
     :     'PHO1_PRCUR: Graphics device chosen only offers ^M choices '/
     :     /'(e.g. by using a mouse or trackerball) where ^N are '/
     :     /'for this application.', STATUS )

         GOTO 999
      END IF

*  Tell the user what to do... first for an image display, here defined
*  as a device with a mouse or trackerball buttons.
      IF ( WKCLAS( 1:6 ) .EQ. 'IMAGE_' ) THEN

*  Set the flag to say the cursor is ready for use.
         CURSOR = .TRUE.

*  Nominally an image display.
         IMGDIS = .TRUE.
      ELSE

*  The device is a workstation or terminal with many choices.
*  Unfortunately, using GKS we cannot determine how many choices there
*  are available on the mouse for a workstation, and so this part of
*  the routine cannot be coded portably.
*
*  Here we assume that a three-button mouse is standard with one choice
*  (left button), break (middle button), and exit (right button).  In
*  GKS 7.2, as used on VMS, the break is replaced by a second choice so
*  that the parameter for the number of mouse choices would need to be
*  increased from one to two.  Since the `break' can be detected by
*  SGS_REQCU returning choice 0, we do indeed set the maximum number of
*  choices at two for UNIX too.  In contrast, the keyboard of a
*  workstation offers many choices.  Thus a workstation is deemed to be
*  an image display if no more than two choices are required, say for
*  selecting points; if the calling routine needs more it is deemed to
*  be a terminal.

*  First validate list of buttons.  Report any error.
         NC = CHR_LEN( BUTTNS )
         IF ( NC .LT. MNCHOI ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PHO1_PRCUR_PROG',
     :        'PHO1_PRCUR: Programmer error.  Check calling arguments',
     :         STATUS )
            GOTO 999
         END IF

*  Trim the list of keyboard choices if necessary, but terminating it
*  with the final character---the exit key.
         IF ( NC .GT. MALT + 1 ) THEN
            BUTLST = BUTTNS( :MNCHOI ) //BUTTNS( NC:NC )
         ELSE
            BUTLST = BUTTNS
         END IF

*  Test for a workstation where we can use the mouse.
         IF ( WKCLAS( 1:6 ) .EQ. 'WINDOW' .AND.
     :        MNCHOI .LE. MOUSCH ) THEN

*  This is or is demmed to be an image display with a useable mouse or
*  trackerball.
            IMGDIS = .TRUE.
         END IF

*  Activate the cursor.
         CALL SGS_CUVIS( .TRUE. )
         CALL SGS_SELCH( 0 )
         CALL SGS_DEFCH( BUTLST )

*  Set the flag to say the cursor is ready for use.
         CURSOR = .TRUE.
      END IF

 999  CONTINUE

      END
* $Id$
