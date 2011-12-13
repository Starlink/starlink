      SUBROUTINE GRP1_READF( UNIT, LINE, EOF, STATUS )
*+
*  Name:
*     GRP1_READF

*  Purpose:
*     Read a record from a formatted sequential file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GRP1_READF( UNIT, LINE, EOF, STATUS )

*  Description:
*     The routine reads an input record from a formatted sequential file
*     and handles any error which may arise. If the end of file is
*     reached, EOF is set true but no error is reported.

*  Arguments:
*     UNIT = INTEGER (Given)
*        Fortran I/O unit attached to the file.
*     LINE = CHARACTER * ( * ) (Returned)
*        Line read from the file.
*     EOF = LOGICAL (Returned)
*        True if the end of file has been reached. False otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 2000 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
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
*     18-AUG-1992 (DSB):
*        Original version
*     27-OCT-2000 (DSB):
*        Changed to return any text read from the last line in a file if
*        the line is terminated by EOF rather than newline.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE               ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'           ! Standard SAE constants
      INCLUDE 'GRP_CONST'        ! GRP private constants.
      INCLUDE 'GRP_PAR'           ! GRP public constants
      INCLUDE 'GRP_ERR'           ! GRP errors.

*  Arguments Given:
      INTEGER   UNIT

*  Arguments Returned:
      CHARACTER LINE*(*)
      LOGICAL   EOF

*  Status:
      INTEGER   STATUS            ! Global status

*  Local Variables:
      CHARACTER FNAME*(GRP__SZFNM)! File name
      INTEGER   IOERR             ! READ error status
      INTEGER   IOS               ! INQUIRE error status

*.
*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Read the file.
      LINE = ' '
      READ( UNIT, '(A)', END = 10, IOSTAT = IOERR ) LINE

*  If an error occurred, then construct a message and report it.
      IF ( IOERR .NE. 0 ) THEN
         STATUS = GRP__FIOER

         INQUIRE ( UNIT = UNIT, NAME = FNAME, IOSTAT = IOS )

         IF( IOS .EQ. 0 ) THEN
            CALL MSG_SETC( 'FILE', FNAME )
         ELSE
            CALL MSG_SETC( 'FILE', ' ' )
         END IF

         CALL MSG_SETI( 'UNIT', UNIT )
         CALL ERR_FIOER( 'MESSAGE', IOERR )

         CALL ERR_REP( 'GRP1_READF_ERR1',
     :'GRP1_READF: Error reading file ^FILE on Fortran unit ^UNIT - '//
     :'^MESSAGE.', STATUS )

      END IF

*  Indicate that the end of file has not yet been reached.
      EOF = .FALSE.

*  Skip the "end of file reached" section.
      GO TO 999

*  Arrive here if the end of file has been reached.
 10   CONTINUE
      EOF = .TRUE.

 999  CONTINUE

      END
