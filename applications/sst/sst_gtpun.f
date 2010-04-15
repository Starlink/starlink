      SUBROUTINE SST_GTPUN( LINE, FOUND, NAME, TYPE, STATUS )
*+
*  Name:
*     SST_GTPUN

*  Purpose:
*     Get the name of a Fortran program unit from its declaration.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SST_GTPUN( LINE, FOUND, NAME, TYPE, STATUS )

*  Description:
*     The routine inspects the line of Fortran 77 code supplied to see
*     if it contains a BLOCK DATA, FUNCTION, PROGRAM or SUBROUTINE
*     keyword.  If so, then it extracts the following sequence of
*     contiguous alphanumeric characters and returns them as the
*     program unit name. It also returns a character indicating the
*     type of declaration found.  If no keyword is found, then FOUND is
*     returned as .FALSE. and no program unit name or type is returned.

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        The line of code to be tested (case insensitive). Only the
*        "statement" field should be supplied, the statement number,
*        etc. being omitted.
*     FOUND = LOGICAL (Returned)
*        Whether a program unit name was found.
*     NAME = CHARACTER * ( * ) (Returned)
*        The program unit name in upper case (not returned if FOUND is
*        .FALSE.). If the variable supplied to receive the result is
*        not long enough, then an ellipsis '...' will be appended.
*     TYPE = CHARACTER * ( * ) (Returned)
*        One of the characters 'B', 'F', 'P' or 'S' to indicate the
*        type of program unit declaration found (block data, function,
*        program or subroutine respectively). No value is returned if
*        FOUND is .FALSE..
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  Dollar ('$'), underscore ('_') and angle brackets ('<' & '>')
*     are recognised as alphanumeric characters by this routine (i.e.
*     they may appear in program unit names).
*     -  It is assumed that spaces are used sensibly in the source code
*     (i.e. they are not embedded in the keywords and program unit
*     name) and that the declaration does not span lines.

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
*     9-AUG-1989 (RFWS):
*        Original version.
*     8-AUG-1990 (RFWS):
*        Substantial change to remove a line length restriction and the
*        use of an internal character buffer.
*     4-SEP-1990 (RFWS):
*        Corrected wrong string length during comparisons which was
*        causing declarations not to be recognised.
*     6-SEP-1990 (RFWS):
*        Reverted to an earlier version (well nearly) because of
*        problems recognising function declarations with their
*        associated data types using the modified version.
*     7-SEP-1990 (RFWS):
*        Improved the detection of 'FUNCTION' keywords with preceding
*        type declarations.
*     10-SEP-1990 (RFWS):
*        Minor bug fixes on reviewing the code.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SST_PAR'          ! SST_ constants

*  Arguments Given:
      CHARACTER * ( * ) LINE

*  Arguments Returned:
      LOGICAL FOUND
      CHARACTER * ( * ) NAME
      CHARACTER * ( * ) TYPE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER * ( 1 ) TC       ! Program unit type character
      CHARACTER * ( SST__SZLIN ) TEMP ! Temporary storage for the line
      INTEGER FC                 ! First non-blank character position
      INTEGER I                  ! Character position variable
      INTEGER IEND               ! Position of program unit name end
      INTEGER ISTART             ! Position of program unit name start
      INTEGER LC                 ! Last non-blank character position
      INTEGER LSTAT              ! Local status variable
      INTEGER N                  ! Position of ellipsis
      INTEGER NC                 ! No. characters in non-blank section

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      FOUND = .FALSE.

*  Find the first and last non-blank characters in the line.
      CALL CHR_FANDL( LINE, FC, LC )

*  If the line is not blank, then extract the non-blank part, convert
*  it to upper case and remove multiple spaces.
      IF ( LC .GE. FC ) THEN
         NC = LC - FC + 1
         TEMP = LINE( FC : LC )
         CALL SST_CLEAN( TEMP( : NC ) )

*  Search for each type of program unit declaration keyword in turn,
*  deriving the position of the character which follows it in each
*  case.
         ISTART = 0

*  BLOCKDATA:
         IF ( TEMP( : 9 ) .EQ. 'BLOCKDATA' ) THEN
            TC = 'B'
            ISTART = 10

*  BLOCK DATA (alternative form):
         ELSE IF ( TEMP( : 10 ) .EQ. 'BLOCK DATA' ) THEN
            TC = 'B'
            ISTART = 11

*  SUBROUTINE:
         ELSE IF ( TEMP( : 10 ) .EQ. 'SUBROUTINE' ) THEN
            TC = 'S'
            ISTART = 11

*  PROGRAM:
         ELSE IF ( TEMP( : 7 ) .EQ. 'PROGRAM' ) THEN
            TC = 'P'
            ISTART = 8

*  FUNCTION:
*  This is more complicated, because The 'FUNCTION' keyword may have a
*  type specification in front of it. First check, just in case it
*  doesn't.
         ELSE IF ( TEMP( : 8 ) .EQ. 'FUNCTION' ) THEN
            TC = 'F'
            ISTART = 9

*  Otherwise, test for each possible preceding type declaration, setting
*  I to point at the character which follows.
         ELSE
            I = 0
            IF ( TEMP( : 4 ) .EQ. 'BYTE' ) THEN
               I = 5
            ELSE IF ( TEMP( : 9 ) .EQ. 'CHARACTER' ) THEN
               I = 10
            ELSE IF ( TEMP( : 16 ) .EQ. 'DOUBLE PRECISION' ) THEN
              I = 17
            ELSE IF ( TEMP( : 15 ) .EQ. 'DOUBLEPRECISION' ) THEN
               I = 16
            ELSE IF ( TEMP( : 7 ) .EQ. 'INTEGER' ) THEN
              I = 8
            ELSE IF ( TEMP( : 7 ) .EQ. 'LOGICAL' ) THEN
              I = 8
            ELSE IF ( TEMP( : 4 ) .EQ. 'REAL' ) THEN
              I = 5
            END IF

*  Skip over any length specifier and/or space characters which may
*  follow, setting I to point at the first character of the 'FUNCTION'
*  keyword, if it exists.
            CALL SST_SKCHR( ' *123456789', TEMP( : NC ), I )

*  If OK, then test for a 'FUNCTION' keyword, setting ISTART to point at
*  the character beyond it.
            IF ( ( I .GE. 1 ) .AND. ( ( I + 7 ) .LE. NC ) ) THEN
               IF ( TEMP( I : I + 7 ) .EQ. 'FUNCTION' ) THEN
                  TC = 'F'
                  ISTART = I + 8
               END IF
            END IF
         END IF

*  Skip over any blanks which follow the program unit declaration
*  keyword, setting ISTART to point at the next non-blank character
*  position, which is where the program unit name starts.
         CALL SST_SKCHR( ' ', TEMP( : NC ), ISTART )

*  Find the position of the next non-alphanumeric character, and derive
*  the position of the end of the program unit name.
         IEND = ISTART
         CALL SST_SKCHR( 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_$<>',
     :                   TEMP( : NC ), IEND )
         IEND = IEND - 1

*  If a name was found, then extract it.  If truncation occurs, then
*  append an ellipsis '...'.
         IF ( ( ISTART .GE. 1 ) .AND. ( ISTART .LE. NC ) .AND.
     :        ( IEND .GE. ISTART ) ) THEN
            FOUND = .TRUE.
            CALL CHR_COPY( TEMP( ISTART : IEND ), .FALSE., NAME, LSTAT )
            IF ( LSTAT .NE. 0 ) THEN
               N = MAX( 1, LEN( NAME ) - 2 )
               NAME( N : ) = '...'
            END IF

*  Return the program unit type.
            TYPE = TC
         END IF
      END IF

      END
* @(#)sst_gtpun.f   1.1   94/12/05 11:31:27   96/07/05 10:27:28
