      SUBROUTINE CCD1_ASRC( STATUS )
*+
*  Name:
*     CCD1_ASRC

*  Purpose:
*     Source routine for use by AST Channels.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_ASRC( STATUS )

*  Description:
*     This routine implements a source routine which has to be passed to
*     the AST Channel construction routines (AST_CHANNEL, AST_FITSCHAN)
*     in order to do input/output on AST objects to a file.  It uses FIO 
*     to do the input, via a file descriptor held in a common block.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     CHAN = AST_CHANNEL( CCD1_ASRC, SINK, OPTIONS, STATUS )

*  Copyright:
*     Copyright (C) 1999 Central Laboratory of the Research Councils

*  Authors:
*     MBT: Mark Taylor (STARLINK - IoA)
*     {enter_new_authors_here}

*  History:
*     02-MAR-1999 (MBT):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO error constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
      
*  Global Variables:
      INCLUDE 'CCD1_FDCM'        ! File descriptor for AST channel CCD1_ASTFD
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for input
      INTEGER NCHAR              ! Number of characters read
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin error context so that harmless errors can be dealt with internally.
      CALL ERR_MARK

*  Get input from file using FIO system.
      CALL FIO_READ( CCD1_ASTFD, LINE, NCHAR, STATUS )

*  If line was read successfully, pass line to AST system.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL AST_PUTLINE( LINE, NCHAR, STATUS )

*  If end of file was reached, tell that to the AST system.
      ELSE IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL AST_PUTLINE( ' ', -1, STATUS )

*  If another error was encountered, exit with non-zero STATUS.
      END IF

*  End error context.
      CALL ERR_RLSE

      END
* $Id$
