      SUBROUTINE ATL_CREAT( PARAM, IAST, STATUS )
*+
*  Name:
*     ATL_CREAT

*  Purpose:
*     Write an AST Object to a text file or NDF specified using an environment
*     parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ATL_CREAT( PARAM, IAST, STATUS )

*  Description:
*     Write an AST Object to a text file or NDF specified using an environment
*     parameter.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The parameter name.
*     IAST = INTEGER (Given)
*        The AST Object, or AST__NULL.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001, 2003 Central Laboratory of the Research
*     Councils. All Rights Reserved.

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
*     15-SEP-2003 (DSB):
*        Changed call to FIO_ANNUL to FIO_CLOSE. Previously the output
*        text file was not completely flushed when being used as a
*        monolith (e.g. from ICL), resulting in incomplete output files.
*     11-MAY-2006 (DSB):
*        Increase maximum line length to 300 characters.
*     30-MAY-2006 (DSB):
*        Moved into ATL library and changed prefix from "ATL1_" to "ATL_".
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
      INTEGER IAST

*  Status:
      INTEGER STATUS             ! Global status

*  Global Variables.
      INTEGER FD
      COMMON /ATL1SNK/ FD

*  External References:
      EXTERNAL ATL_SNK

*  Local Variables:
      CHARACTER FNAME*255
      INTEGER CHAN
      INTEGER NOBJ
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the name of the output file.
      CALL PAR_GET0C( PARAM, FNAME, STATUS )

*  We delete any pre-existing file first.
      CALL ATL_RM( FNAME, STATUS )

*  Open a new file and get an FIO identifier for it.
      CALL FIO_OPEN( FNAME, 'WRITE', 'LIST', 300, FD, STATUS )

*  Create an AST Channel to write to the file.
      CHAN = AST_CHANNEL( AST_NULL, ATL_SNK, ' ', STATUS )

*  Write the Object to the Channel.
      NOBJ = AST_WRITE( CHAN, IAST, STATUS )

*  Report an error if no Object was written.
      IF( STATUS .EQ. SAI__OK .AND. NOBJ .EQ. 0 ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'F', FNAME )
         CALL ERR_REP( 'ATL_CREAT_ERR1', 'Failed to write an AST '//
     :                 'Object to file ''^F''.', STATUS )

      ELSE
         CALL MSG_SETC( 'F', FNAME )
         CALL ATL_NOTIF( '   AST data written to text file ''^F''.',
     :                    STATUS )
      END IF

*  Annul the channel.
      CALL AST_ANNUL( CHAN, STATUS )

*  Close the file.
      CALL FIO_CLOSE( FD, STATUS )

      END


      SUBROUTINE ATL_SNK( STATUS )

      INCLUDE 'SAE_PAR'
      INCLUDE 'FIO_ERR'

*  Arguments:
      INTEGER STATUS

*  Global Variables.
      INTEGER FD
      COMMON /ATL1SNK/ FD

*  Local Variables:
      CHARACTER BUF*300
      INTEGER NC

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read a line from the Channel.
      CALL AST_GETLINE( BUF, NC, STATUS )

*  If succesful, write it to the file.
      IF( NC .GT. 0 ) CALL FIO_WRITE( FD, BUF( : NC ), STATUS )

      END
