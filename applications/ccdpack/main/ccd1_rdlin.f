      SUBROUTINE CCD1_RDLIN( FD, LENGTH, LINE, NCHAR, LINNUM, EOF,
     :                       STATUS )
*+
*  Name:
*     CCD1_RDLIN

*  Purpose:
*     To read a complete line from a CCDPACK formatted file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_RDLIN( FD, LENGTH, LINE, NCHAR, LINNUM, EOF, STATUS )

*  Description:
*     The routine reads a line from the file attached to the FIO stream
*     FD. If the line terminates in the continuation character '-' then
*     the next line is also written to the given line etc. until either
*     no more continuation characters are found at the end of the last
*     line, or the line character buffer is exceeded. If the possible
*     line length is exceeded then an error is reported together with
*     the offending line number. Comment lines are ignored and in line
*     comments are excluded. Non printing characters and leading
*     blanks are removed, as are any commas (this leaves fields aways
*     separated by spaces), not within parentheses (this leaves NDF
*     sections and TRANSFORM functions unchanged).

*  Arguments:
*     FD = INTEGER (Given)
*        FIO system file descriptor.
*     LENGTH = INTEGER (Given)
*        The maximum length of a line.
*     LINE = CHARACTER * ( LENGTH ) (Returned)
*        The extracted line of information.
*     NCHAR = INTEGER (Returned)
*        The actual number of characters in the output line.
*     LINNUM = INTEGER (Given and Returned)
*        The line number of the current line. Note that this value
*        should be initialised to 0 on the first call to this routine,
*        the value will be incremented on subsequent calls.
*     EOF = LOGICAL (Returned)
*        If true then the End-Of-File has been reached.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992-1993 Science & Engineering Research Council.
*     Copyright (C) 1995, 2001 Central Laboratory of the Research
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     5-FEB-1992 (PDRAPER):
*        Original Version.
*     7-DEC-1993 (PDRAPER):
*        Added removal of non-printing characters.
*     15-DEC-1993 (PDRAPER):
*        Removed removal of '=' in input and added suppression of
*        comma removal in ().
*     13-SEP-1995 (PDRAPER):
*        Removed fold to upper case. This is necessary when reading
*        file names on UNIX.
*     12-JUL-2001 (MBT):
*        Removed check for empty file.  This was questionable anyway,
*        since it only regstered a completely empty file not one
*        with only comments.
*     {enter_further_changes_here}

*  Bugs:
*     {note_new_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'FIO_ERR'          ! FIO status values

*  Arguments Given:
      INTEGER FD
      INTEGER LENGTH

*  Arguments Returned:
      CHARACTER * ( * ) LINE
      INTEGER NCHAR
      INTEGER LINNUM
      LOGICAL EOF

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN           ! Length of string exluding trailing
                                 ! blanks
      LOGICAL CCD1_ISCOM
      EXTERNAL CCD1_ISCOM        ! Set true if line is a comment line
      LOGICAL CCD1_HVCON
      EXTERNAL CCD1_HVCON        ! Set true if line is ended by a
                                 ! continuation character

*  Local Variables:
      CHARACTER * ( 132 ) BUFFER ! Local line read buffer
      INTEGER NBUFF              ! Number of characters in BUFFER
      INTEGER NPAREN             ! Number of open parentheses
      LOGICAL NEWLIN             ! Set if another line is to be read
      INTEGER J                  ! Loop variable
      INTEGER STRLEN             ! Length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialisations.
      LINE = ' '
      BUFFER = ' '
      EOF = .FALSE.
      NCHAR = 1
      NEWLIN = .TRUE.

*  Loop while have a continuation character to process and not the end
*  of file.
 1    CONTINUE ! Start of 'DO WHILE' loop
         IF ( NEWLIN .AND. .NOT. EOF .AND. STATUS .EQ. SAI__OK ) THEN

*  Read in a line
            BUFFER = ' '
            CALL FIO_READ( FD, BUFFER , NBUFF, STATUS )
            NBUFF = MAX( NBUFF, 1 )

*  Check that have not reached the EOF.
            IF ( STATUS .EQ. FIO__EOF ) THEN
               CALL ERR_ANNUL( STATUS )
               EOF = .TRUE.
            ELSE

*  Not EOF, record this an increment line counter
               EOF = .FALSE.
               LINNUM = LINNUM + 1
            END IF

*  Remove non-printing characters (tabs etc.)
            CALL CHR_CLEAN( BUFFER ( :NBUFF ) )

*  Remove leading blanks in situ
            CALL CHR_LDBLK( BUFFER( :NBUFF ) )

*  Is the line a comment or blank line ?
            IF ( CCD1_ISCOM( BUFFER, STATUS ) ) THEN

*  Have a comment line skip this
               NEWLIN =.TRUE.
            ELSE

*  Strip any in line comments.
               CALL CCD1_RMCOM( BUFFER, NBUFF, STATUS )

*  Check for a continuation character, if is one strip last character
*  trim string and set newlin flag.
               NEWLIN = CCD1_HVCON( BUFFER( :NBUFF ), STATUS )

*  If have have continuation character remove it.
               IF ( NEWLIN ) THEN
                  NBUFF = MAX( 1, CHR_LEN( BUFFER( :NBUFF ) ) )
                  NBUFF = NBUFF - 1
               END IF

*  Add the rest to the real line. Increment the character counter.
               IF ( NCHAR + NBUFF .LT. LENGTH ) THEN
                  LINE( NCHAR: )  = BUFFER( :NBUFF )
                  NCHAR = NCHAR + NBUFF
               ELSE

*  No room for rest of this line - error. Capture file name and line
*  number.
                  BUFFER = ' '
                  CALL FIO_FNAME( FD, BUFFER, STATUS )
                  STRLEN = MAX( 1, CHR_LEN( BUFFER ) )
                  CALL MSG_SETC( 'FILNAM', BUFFER( : STRLEN ) )
                  CALL MSG_SETI( 'LINNUM', LINNUM )
                  STATUS = SAI__ERROR
                  CALL ERR_REP( 'RDLIN_ERROR2',
     :            '  Line ^LINNUM  of file ^FILNAM exceeds internal'//
     :            '  storage - line too long' , STATUS )
                  GO TO 99
               END IF
            END IF

*  Go for next loop.
            GO TO 1
         END IF

*  Now that we have the whole line remove commas. Leave any within
*  parentheses untouched.
      NPAREN = 0
      DO 2 J = 1, NCHAR
         IF ( LINE( J : J ) .EQ. '(' ) THEN
            NPAREN = NPAREN + 1
         ELSE IF ( LINE( J : J ) .EQ. ')' ) THEN
            NPAREN = NPAREN - 1
         ELSE IF ( LINE( J : J )  .EQ. ',' ) THEN
            IF ( NPAREN .EQ. 0 ) THEN
               LINE( J : J ) = ' '
            END IF
         END IF
 2    CONTINUE

*  If NPAREN is non zero then have unbalanced parentheses.
      IF ( NPAREN .NE. 0 ) THEN
         BUFFER = ' '
         CALL FIO_FNAME( FD, BUFFER, STATUS )
         STRLEN = MAX( 1, CHR_LEN( BUFFER ) )
         CALL MSG_SETC( 'FILNAM', BUFFER( :STRLEN ) )
         CALL MSG_SETI( 'LINNUM', LINNUM )
         CALL CCD1_MSG( 'RDLIN_WARN_PAR',
     :   '  Warning - line ^LINNUM  of file ^FILNAM contains '//
     :   'unbalanced parentheses', STATUS )
      END IF

99    CONTINUE
      END
* $Id$
