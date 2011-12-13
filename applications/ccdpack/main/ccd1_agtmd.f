      SUBROUTINE CCD1_AGTMD( FD, FITROT, STATUS )
*+
*  Name:
*     CCD1_AGTMD

*  Purpose:
*     Read modifiers from AST file.

*  Language:
*     Starlink Fortran 77.

*  Invocation:
*     CALL CCD1_AGTMD( FD, FITROT, STATUS )

*  Description:
*     This routine reads modifier strings from an FIO file on the file
*     descriptor given.  It returns in its parameters values for any
*     which are found.  The format of these parameters is generally
*
*        USE keyword value
*
*     where the two are separated by an arbitrary amount of whitespace.
*     Additional whitespace at the beginning or end of each line is
*     ignored.  If more than one of the same keyword is read, the last
*     value is used.  Any characters following a '#' character are
*     ignored.
*
*     The routine reads until it finds a blank line (without even a
*     comment) or the end of file.  If it reads any line which is not
*     blank or a comment and does not start 'USE' it will exit with
*     error status.  Unrecognised modifier words are skipped harmlessly.
*
*     Currently, only a single modifier is recognised:
*        USE FITSROT value

*  Arguments:
*     FD = _INTEGER (Given)
*        FIO file descriptor for file from which to read.
*     FITROT = _CHARACTER * ( * ) (Returned)
*        Value of the FITSROT keyword if found (set to ' ' if not found).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     MBT: Mark Taylor (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1999 (MBT):
*        Original version.
*     3-JUL-2000 (MBT):
*        Modified to allow comment lines at the start of the file.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO error constants
      INCLUDE 'CCD1_PAR'         ! CCDPACK private constants

*  Local Constants:
      INTEGER MXWRD              ! Maximum words in a line
      PARAMETER ( MXWRD = 4 )

*  Arguments Given:
      INTEGER FD

*  Arguments Returned:
      CHARACTER * ( * ) FITROT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( CCD1__BLEN ) LINE ! Line read from file
      CHARACTER * ( CCD1__BLEN ) WORDS( MXWRD ) ! Words in line
      INTEGER ICOMM              ! Position of comment character
      INTEGER LSTAT              ! Local status
      INTEGER NCHAR              ! Number of characters read
      INTEGER NWRD               ! Number of words in line
      INTEGER START( MXWRD )     ! Start positions of words in line
      INTEGER STOP( MXWRD )      ! End positions of words in line

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise return values.
      FITROT = ' '

*  Start of read loop.
 1    CONTINUE

*  Get input from file.
      LINE = ' '
      CALL FIO_READ( FD, LINE, NCHAR, STATUS )

*  Exit if end of file.
      IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
         GO TO 99
      END IF

*  Ignore if comment line.
      IF ( LINE( 1:1 ) .EQ. '#' ) GO TO 1

*  Exit if blank line.
      IF ( LINE .EQ. ' ' ) GO TO 99

*  Strip comments.
      ICOMM = INDEX( LINE, '#' )
      IF ( ICOMM .GT. 0 ) LINE( ICOMM: ) = ' '

*  Split line into words.
      CALL CHR_DCWRD( LINE, MXWRD, NWRD, START, STOP, WORDS, LSTAT )

*  Check line looks right.
      IF ( WORDS( 1 ) .NE. 'USE' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( ' ', '    AST file is corrupted', STATUS )
         GO TO 99
      END IF

*  Deal with words.
      IF ( WORDS( 2 ) .EQ. 'FITSROT' ) THEN
         IF ( NWRD .NE. 3 ) THEN
            CALL CCD1_MSG( ' ',
     :      '    Warning: wrong number of words in line:', STATUS )
            CALL MSG_SETC( 'LINE', LINE )
            CALL CCD1_MSG( ' ', '      "^LINE"', STATUS )
         END IF
         FITROT = WORDS( 3 )
         CALL CHR_UCASE( FITROT )
         CALL MSG_SETC( 'FITROT', FITROT )
         CALL CCD1_MSG( ' ',
     :   '    FITS header "^FITROT" used for rotation', STATUS )
      ELSE
         CALL CCD1_MSG( ' ',
     :      '    Warning: unknown modifier line:', STATUS )
         CALL MSG_SETC( 'LINE', LINE )
         CALL CCD1_MSG( ' ', '      "^LINE"', STATUS )
      END IF

*  Next line.
      GO TO 1

*  Exit routine.
 99   CONTINUE

      END
* $Id$
