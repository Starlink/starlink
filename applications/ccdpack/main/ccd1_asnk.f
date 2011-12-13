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

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

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
