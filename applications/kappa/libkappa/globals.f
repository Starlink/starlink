      SUBROUTINE GLOBALS( STATUS )
*+
*  Name:
*     GLOBALS

*  Purpose:
*     Displays the values of the KAPPA global parameters.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GLOBALS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This procedure lists the meanings and values of the KAPPA global
*     parameters.  If a global parameter does not have a value, the
*     string "<undefined>" is substituted where the value would have
*     been written.

*  Usage:
*     globals

*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 18 (MJC):
*        Original version.
*     1992 June 5 (MJC):
*        Finds the operating system in order to define the path to the
*        global file.
*     1995 August 31 (MJC):
*        Lists the current transformation.
*     1995 December 6 (MJC):
*        Revised the displayed wording for the current DATA_ARRAY to
*        apply to foreign formats too.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'DAT_ERR'          ! HDS error definitions

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Length of character string ignoring
                                 ! trialing blanks

*  Local Variables:
      CHARACTER * ( DAT__SZLOC )
     :  LOC,                     ! Locator to the global file.
     :  PLOC                     ! Locator to the parameter-system
                                 ! structure containing the file and
                                 ! device names

      CHARACTER
     :  GLOVAL * ( 132 ),        ! Global variable
     :  MACHIN * ( 24 ),         ! Machine name
     :  NODE * ( 20 ),           ! Node name
     :  PATH * ( 132 ),          ! Path name of ADAM_USER
     :  RELEAS * ( 10 ),         ! Release of operating system
     :  SYSNAM * ( 10 ),         ! Operating system
     :  VERSIO * ( 10 )          ! Sub-version of operating system

      INTEGER
     :  NC                       ! Number of characters
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Determine the host operating system.
*  ====================================
*
*  This assumes that the system is either VMS or UNIX.  It is needed
*  to specify the path of the file containing the global parameters.
      CALL PSX_UNAME( SYSNAM, NODE, RELEAS, VERSIO, MACHIN, STATUS )

*  Open the global file.
*  =====================
      LOC = ' '

*  Look for VMS or VAX/VMS.  If found use thre ADAM_USER logical name.
      IF ( INDEX( SYSNAM, 'VMS' ) .NE. 0 ) THEN
         CALL HDS_OPEN( 'ADAM_USER:GLOBAL', 'READ', LOC, STATUS )

*  Assume that it is a UNIX system.
      ELSE

         CALL ERR_MARK

*  Translate the ADAM_USER environment variable and its length.
         CALL PSX_GETENV( 'ADAM_USER', PATH, STATUS )
         NC = CHR_LEN( PATH )
      
*  First look in ADAM_USER.  If this is not defined deal with the error
*  silently and try the default directory.
         CALL HDS_OPEN( PATH( :NC )//'/GLOBAL', 'READ', LOC,
     :                  STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )

*  Translate the HOME environment variable.  Get its length.
            CALL PSX_GETENV( 'HOME', PATH, STATUS )
            NC = CHR_LEN( PATH )

            LOC = ' '
            CALL HDS_OPEN( PATH( :NC )//'/adam/GLOBAL', 'READ', LOC,
     :                     STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  Inquire the various global parameters.
*  ======================================      

*  Bracket each with an error context, since we want to annul the
*  error if the object is not found (DAT__OBJNF) and substitute another
*  report.

*  The data array.  This is a file name so it is inside a structure.
      CALL ERR_MARK
      PLOC = ' '

*  Find and obtain the value.
      CALL DAT_FIND( LOC, 'DATA_ARRAY', PLOC, STATUS )
      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )

*  Create the appropriate token when there is no object.
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF

*    Report the current global value.
      CALL MSG_OUT( 'GLOBAL1',
     :  'The current data file is             : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*    The graphics device.
      CALL ERR_MARK
      PLOC = ' '
      CALL DAT_FIND( LOC, 'GRAPHICS_DEVICE', PLOC, STATUS )
      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUT( 'GLOBAL2',
     :  'The current graphics device is       : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*    The image-display device.
      CALL ERR_MARK
      PLOC = ' '
      CALL DAT_FIND( LOC, 'IMAGE_DISPLAY', PLOC, STATUS )
      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUT( 'GLOBAL3',
     :  'The current image-display device is  : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*    The image-display device.
      CALL ERR_MARK
      PLOC = ' '
      CALL DAT_FIND( LOC, 'IMAGE_OVERLAY', PLOC, STATUS )
      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUT( 'GLOBAL4',
     :  'The current image-display overlay is : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*    The lookup table.
      CALL ERR_MARK
      PLOC = ' '
      CALL DAT_FIND( LOC, 'LUT', PLOC, STATUS )
      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUT( 'GLOBAL5',
     :  'The current lookup table file is     : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*    The transformation.
      CALL ERR_MARK
      PLOC = ' '
      CALL DAT_FIND( LOC, 'TRANSFORM', PLOC, STATUS )
      CALL CMP_GET0C( PLOC, 'NAMEPTR', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUT( 'GLOBAL6',
     :  'The current transformation is        : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*    The interaction mode.
      CALL ERR_MARK
      CALL CMP_GET0C( LOC, 'INTERACTIONMODE', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUT( 'GLOBAL7',
     :  'The current interaction mode is      : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*    The co-ordinate system.
      CALL ERR_MARK
      CALL CMP_GET0C( LOC, 'COORD_SYSTEM', GLOVAL, STATUS )
      IF ( STATUS .EQ. DAT__OBJNF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL MSG_SETC( 'GLOVAL', '<undefined>' )
      ELSE
         CALL MSG_SETC( 'GLOVAL', GLOVAL )
      END IF
      CALL MSG_OUT( 'GLOBAL8',
     :  'The current co-ordinate system is    : ^GLOVAL', STATUS )
      CALL ERR_RLSE

*  Close the global file.
*  =====================
      CALL HDS_CLOSE( LOC, STATUS )

*  Closedown.
*  ==========

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'GLOBALS_ERR',
     :     'GLOBALS: Error listing the KAPPA global parameters.',
     :     STATUS )
      END IF

      END
