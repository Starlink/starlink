      SUBROUTINE CCD1_DOCMD( STYPE, SCRIPT, LOGFIL, STATUS )
*+
*  Name:
*     CCD1_DOCMD

*  Purpose:
*     Executes a CCDPACK command procedure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_DOCMD( SCRIPT, LOGFIL, STATUS )

*  Description:
*     This routine executes a CCDPACK command procedure in a forked
*     process. The new process is independent of the current execution
*     environment as a copy of all the current application parameter
*     files is taken.

*  Arguments:
*     STYPE = CHARACTER * ( * ) (Given)
*        The type of script to be executed. This can be "CSH" or "ICL"
*        The different script types require executing in different ways.
*     SCRIPT = CHARACTER * ( * ) (Given)
*        The name of the file containing the CCDPACK commands which are
*        to be executed.
*     LOGFIL = CHARACTER * ( * ) (Given)
*        The logfile for the output from the script when it is
*        executed.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     UNIX specific file names etc.

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     9-FEB-1994 (PDRAPER):
*        Original version.
*     4-SEP-1995 (PDRAPER):
*        Added ICL support.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STYPE
      CHARACTER * ( * ) SCRIPT
      CHARACTER * ( * ) LOGFIL

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      CHARACTER * ( 132 ) COMMAND ! String containing command to be
                                ! performed
      INTEGER IAT               ! Length of variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Generate the string to perform the command (use the environment
*  variable CCDPACK_DIR and an explicit sh as some systems use the shell 
*  as set by the $SHELL variable). The final command looks something like.
*
*     /bin/sh -c "$CCDPACK_DIR/ccdexecute CSH script logfile"
*  or 
*     /bin/sh -c "$CCDPACK_DIR/ccdexecute ICL script logfile"
*
      COMMAND = '/bin/sh -c "$CCDPACK_DIR/ccdexecute'
      IAT = 37
      CALL CHR_APPND( STYPE, COMMAND, IAT )
      IAT = IAT + 1
      CALL CHR_APPND( SCRIPT, COMMAND, IAT )
      IAT = IAT + 1
      CALL CHR_APPND( LOGFIL, COMMAND, IAT )
      IAT = IAT + 1
      COMMAND( IAT :IAT ) = '"'

*  Execute it.
      CALL CCD1_EXEC( COMMAND, STATUS )

*  Check the return status.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CCD1_DOCMDERR', '  Error executing command:',
     :                 STATUS )
         CALL MSG_SETC( 'COMMAND', COMMAND )
         CALL ERR_REP( ' ', '    ^COMMAND', STATUS )
      END IF
      END
