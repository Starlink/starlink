      SUBROUTINE KPS1_AGNLS( IGRP, STATUS )
*+
*  Name:
*     KPS1_AGNLS

*  Purpose:
*     List the regions in the supplied group (for ARDGEN)

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_AGNLS( IGRP, STATUS )

*  Description:
*     The supplied GRP group contains the ARD descriptions for each
*     region.  The first region in the group has `index' 1, and
*     subsequent regions are indexed sequentially.  The `index' of a
*     region should not be confused with the indices used by GRP to
*     refer to individual elements within a group.  Each ARD
*     description may occupy several elements in the group.  The ARD
*     keyword for a region always starts at column 1 of a new element.
*     If there are too many arguments to fit them into the rest of the
*     element, then they continue in the next element.  Such
*     continuation elements are marked by the fact that they start with
*     one or more spaces.
*
*     Each region is displayed in turn with the corresponding region
*     index.  The user is requested to press RETURN to continue when
*     the screen is full.
      
*  Arguments:
*     IGRP = INTEGER (Given)
*        The GRP identifier for the group holding the ARD descriptions.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-DEC-1994 (DSB):
*        Original version.
*     1995 March 16 (MJC):
*        Corrected some typo's and used the modern style for variable
*        declarations.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'GRP_PAR'          ! GRP public constants

*  Global Variables:
      INCLUDE 'HLPCMD'           ! Paged screen I/O
*        CMD = CHARACTER * ( 80 ) (Write)
*           The command line.
*        LHELP = INTEGER (Write)
*           Lines of output this screenful.
*        HELPN = LOGICAL (Write)
*           If true, output is enabled.
*        LTOP = INTEGER (Write)
*           Top line number for the scrolling region.
*        LBOT = INTEGER (Write)
*           Bottom line number for the scrolling region.
*        ANSI = LOGICAL (Write)
*           If true, an ANSI terminal is in use.
*        LUCMD = INTEGER (Write)
*           Logical-unit number of the command input.
*        LUTERM = INTEGER (Write)
*           Logical-unit number of the terminal output.

*  Arguments Given:
      INTEGER IGRP
      
*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      INTEGER PTHLPO

*  Local Variables:
      INTEGER ARGLST             ! Position of start of argument list
      LOGICAL BLANK              ! Display a blank line?
      INTEGER I                  ! Current group element index
      INTEGER ISTAT              ! Local status for PTHLPO routine
      CHARACTER * ( 80 ) LINE    ! Output buffer
      INTEGER LINLEN             ! Used length of LINE
      INTEGER REG                ! Current region index
      INTEGER SIZE               ! Number of elements in group
      CHARACTER * ( GRP__SZNAM ) TEXT ! Element of text from the group
      INTEGER WIDTH              ! Screen width

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the local status associated with the paged output routine
*  PTHLPO.
      ISTAT = 1

*  Space output from the previous text.
      CALL MSG_BLANK( STATUS )

*  Get the group size.  If it zero there are no defined regions, so
*  warn the user and return.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )      
      IF( SIZE .EQ. 0 ) THEN
         CALL MSG_OUT( 'KPS1_AGNLS_MSG1', 'There are currently no '/
     :                 /'regions defined.', STATUS )
         GO TO 999
      END IF

*  Set up the common block used by the paged screen output routine
*  PTHLPO.  This is the routine used to output help text.

*  Nothing has been output, and there is no command.
      LHELP = 0
      HELPN = .TRUE.
      CMD = ' '

*  Fixed for test purposes.  Note these are hardware specific.
      ANSI = .FALSE.
      LUCMD = 5
      LUTERM = 6

*  Find the height and width of the screen.  Use the full screen area.
*  A zero or negative LBOT (which occurs when there is an error) will
*  suppress paging.
      CALL KPG1_SCRSZ( WIDTH, LBOT, STATUS )
      LTOP = 1

*  Initialise the next region index.
      REG = 0

*  Display a heading.  Abort if an error occurs while writing to the
*  screen.
      ISTAT = PTHLPO( ' Region          Region Description' ) 
      IF ( ISTAT .NE. 1 ) GO TO 999

      ISTAT = PTHLPO( ' Index' )
      IF ( ISTAT .NE. 1 ) GO TO 999

*  Put a blank line above the first region.
      BLANK = .TRUE.

*  Go through the group.
      DO I = 1, SIZE

*  Get the text of the next element.
         CALL GRP_GET( IGRP, I, 1, TEXT, STATUS )

*  If the first character is not blank, this is the start of a new
*  region.  Display a blank line if the previous line was a
*  continuation line.  Store the region index and text ready for
*  display.  Find the position of the opening parenthesis which marks
*  the start of the argument list.
         IF ( TEXT( 1 : 1 ) .NE. ' ' ) THEN
            REG = REG + 1

            IF ( BLANK ) THEN
               ISTAT = PTHLPO( ' ' ) 
               IF ( ISTAT .NE. 1 ) GO TO 999
               BLANK = .FALSE.
            END IF

            LINE = ' '
            WRITE( LINE, '(I4)' ) REG
            LINE( 6 : ) = '  -  '//TEXT
            ARGLST = INDEX( LINE, '(' )

*  If the first character is blank, this element is a continuation of
*  the current region.  Store the text with the first non-blank
*  character aligned with the start of the argument list.
         ELSE
            BLANK = .TRUE.
            CALL CHR_LDBLK( TEXT )
            LINE = ' '
            LINE( ARGLST + 2: ) = TEXT

         END IF

*  Display the stored text.
         LINLEN = CHR_LEN( LINE )
         ISTAT = PTHLPO( LINE( : LINLEN ) ) 
         IF ( ISTAT .NE. 1 ) GO TO 999

      END DO

      ISTAT = PTHLPO( ' ' ) 

 999  CONTINUE

*  If an error occurred, writing to the screen, report an error.
      IF ( ISTAT .NE. 1 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPS1_AGNLS_ERR', 'An error occurred writing '/
     :                 /'paged text to the output device', STATUS )
      END IF

      END
