      SUBROUTINE CCD1_ASNK( STATUS )
*+
*  Name:
*     CCD1_ASNK

*  Purpose:
*     Sink routine for use by AST Channels.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_ASNK( STATUS )

*  Description:
*     This routine implements a sink routine which has to be passed to
*     the AST Channel construction routines (AST_CHANNEL, AST_FITSCHAN)
*     in order to do input/output on AST objects to a file.  It uses 
*     FIO to do the output, via a file descriptor held in a common block.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Usage:
*     CHAN = AST_CHANNEL( SOURCE, CCD1_ASNK, OPTIONS, STATUS )

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
      INCLUDE 'CCD1_PAR'         ! CCDPACK parameters
      
*  Global Variables:
      INCLUDE 'CCD1_FDCM'        ! File descriptor for AST channel CCD1_ASTFD
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for output
      INTEGER NCHAR              ! Number of characters to write
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      
*  Allow AST system to fill buffer.
      CALL AST_GETLINE( LINE, NCHAR, STATUS )

*  Write to output file using FIO system.
      IF ( NCHAR .GT. 0 ) 
     :   CALL FIO_WRITE( CCD1_ASTFD, LINE( :NCHAR ), STATUS )

      END
* $Id$
