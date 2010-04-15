      SUBROUTINE SST_PUT( INDENT, LINE, STATUS )
*+
*  Name:
*     SST_PUT

*  Purpose:
*     Send an line to the output file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_PUT( INDENT, LINE, STATUS )

*  Description:
*     The routine sends a line to the output file with a specified
*     number of blanks preceding it to provide indentation. Leading and
*     trailing blanks within the line itself are removed before
*     applying the indentation.

*  Arguments:
*     INDENT = INTEGER (Given)
*        Indentation level (number of blanks in front of first non-blank
*        character).
*     LINE = CHARACTER * ( * ) (Given)
*        Line to be output.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1989, 1990 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify it under
*     the terms of the GNU General Public License as published by the Free Software
*     Foundation; either version 2 of the License, or (at your option) any later
*     version.
*
*     This program is distributed in the hope that it will be useful,but WITHOUT ANY
*     WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
*     PARTICULAR PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License along with
*     this program; if not, write to the Free Software Foundation, Inc., 59 Temple
*     Place,Suite 330, Boston, MA  02111-1307, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-DEC-1989 (RFWS):
*        Original version.
*     8-AUG-1990 (RFWS):
*        Added comprehensive error report if a write error occurs.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SST_PAR'          ! SST_ constants
      INCLUDE 'FIO_PAR'          ! FIO_ public constants

*  Global Variables:
      INCLUDE 'SST_SCB'          ! SST_ Source Code Buffer

*  Arguments Given:
      INTEGER INDENT
      CHARACTER * ( * ) LINE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( FIO__SZFNM ) FNAME ! Output file name
      CHARACTER * ( SST__SZLIN ) BLANKS ! Blank characters
      INTEGER F                  ! First non-blank character
      INTEGER IND                ! Number of blanks for indentation
      INTEGER IOERR              ! I/O error status
      INTEGER L                  ! Last non-blank character

*  Local Data:
      DATA BLANKS / ' ' /

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Find the first and last non-blank characters to be output.
      CALL CHR_FANDL( LINE, F, L )

*  If the line is blank, then output a blank line.
      IF ( F .GT. L ) THEN
         WRITE( SCB_OUT, '( A )', IOSTAT = IOERR )

*  Find the number of blanks required for indentation.
      ELSE
         IND = MIN( MAX( 0, INDENT ), SST__SZLIN )

*  If none are required, output the line without indentation.
         IF ( IND .EQ. 0 ) THEN
            WRITE( SCB_OUT, '( A )', IOSTAT = IOERR ) LINE( F : L )

*  Otherwise, prefix some blanks.
         ELSE
            WRITE( SCB_OUT, '( A, A )', IOSTAT = IOERR )
     :         BLANKS( : IND ), LINE( F : L )
         END IF
      END IF

*  If an error occurred during the write operation, then set STATUS to
*  an appropriate value.
      IF ( IOERR .NE. 0 ) THEN
         CALL FIO_SERR( IOERR, STATUS )

*  Construct a message and report the error.
         FNAME = '?'
         INQUIRE ( UNIT = SCB_OUT, NAME = FNAME )
         CALL MSG_SETC( 'FILE', FNAME )
         CALL MSG_SETI( 'UNIT', SCB_OUT )
         CALL ERR_FIOER( 'MESSAGE', IOERR )
         CALL ERR_REP( 'SST_PUT_WRITE',
     :   'Error writing to file ^FILE on Fortran unit ^UNIT - ' //
     :   '^MESSAGE.', STATUS )
      END IF

      END
* @(#)sst_put.f   1.1   94/12/05 11:31:31   96/07/05 10:27:31
