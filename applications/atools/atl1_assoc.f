      SUBROUTINE ATL1_ASSOC( PARAM, IAST, STATUS )
*+
*  Name:
*     ATL1_ASSOC

*  Purpose:
*     Read an AST Object from a text file specified using an environment
*     parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL1_ASSOC( PARAM, IAST, STATUS )

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     IAST = INTEGER (Returned)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JAN-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants

*  Arguments Given:
      CHARACTER PARAM*(*)

*  Arguments Returned:
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables.
      INTEGER FD
      COMMON /ATL1SRC/ FD

*  External References:
      EXTERNAL ATL1_SRC

*  Local Variables:
      INTEGER CHAN
*.

*  Initialise.
      IAST = AST__NULL

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Open a file and get an FIO identifier for it.
      CALL FIO_ASSOC( PARAM, 'READ', 'LIST', 0, FD, STATUS )

*  Create an AST Channel to read the file.
      CHAN = AST_CHANNEL( ATL1_SRC, AST_NULL, ' ', STATUS )

*  Read an Object from the Channel.
      IAST = AST_READ( CHAN, STATUS )

*  Report an error if no Object was read.
      IF( STATUS .EQ. SAI__OK .AND. IAST .EQ. AST__NULL ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'P', PARAM )
         CALL ERR_REP( 'ATL1_ASSOC_ERR1', 'Failed to read an AST '//
     :                 'Object from file ''$^P''.', STATUS )
      END IF

*  Annul the channel.
      CALL AST_ANNUL( CHAN, STATUS )

*  Close the file.
      CALL FIO_ANNUL( FD, STATUS )

*  Return null object if error.
      IF( STATUS .NE. SAI__OK ) CALL AST_ANNUL( IAST, STATUS )

      END


      SUBROUTINE ATL1_SRC( STATUS )

      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'

*  Arguments:
      INTEGER STATUS

*  Global Variables.
      INTEGER FD
      COMMON /ATL1SRC/ FD

*  Local Variables:
      CHARACTER BUF*200
      INTEGER NC

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read a line from the file.
      CALL FIO_READ( FD, BUF, NC, STATUS )

*  If succesful, add it to the Channel.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL AST_PUTLINE( BUF, NC, STATUS )

*  Otherwise, if end of file has been reached, annul the error and return a
*  length of -1.
      ELSE IF( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
         CALL AST_PUTLINE( BUF, -1, STATUS )

      END IF

      END
