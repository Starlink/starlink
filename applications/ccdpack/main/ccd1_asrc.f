      SUBROUTINE CCD1_ASRC( STATUS )
*+
*  Name:
*     CCD1_ASRC

*  Purpose:
*     Source routine for use by AST Channels.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CHAN = AST_CHANNEL( CCD1_ASRC, SINK, OPTIONS, STATUS )

*  Description:
*     This routine implements a source routine which has to be passed to
*     the AST Channel construction routines (AST_CHANNEL, AST_FITSCHAN)
*     in order to do input/output on AST objects other than to or from
*     the WCS component of an NDF.  It uses FIO to do the input, via
*     a file descriptor held in a common block.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
      INCLUDE 'CCD1_FDCM'        ! File descriptor common block
      
*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Buffer for input
      INTEGER NCHAR              ! Number of characters read
      
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get input from file using FIO system.
      CALL FIO_READ( CCD1_FDIN, LINE, NCHAR, STATUS )

*  Pass line to AST system.
      CALL AST_PUTLINE( LINE, NCHAR, STATUS )

      END
* $Id$
