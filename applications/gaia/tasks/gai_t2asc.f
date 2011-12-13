      SUBROUTINE GAI_T2ASC( IFIN, IFOUT, STATUS )
*+
*  Name:
*     GAI_T2ASC

*  Purpose:
*     Convert a tab table into an ASCII catalogue.

*  Description:
*     This routine reads a file attached to a given FIO identifier and
*     attempts to convert it into an ASCII_HEAD catalogue, under the
*     assumption that the attached file contains a "tab table".

*  Language:
*     Fortran-77

*  Invocation:
*     CALL GAI_T2CAT( FI, CI, STATUS )

*  Description:

*  Arguments:
*     IFIN = INTEGER (Given)
*        FIO identifier of the catalogue to be converted.
*     IFOUT = INTEGER (Given)
*        FIO identifier of the output tab-table.
*     STATUS = INTEGER (Given and Returned)
*        The global status on exit.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     16-OCT-1998 (PWD):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'FIO_ERR'         ! FIO error codes

*  Arguments Given:
      INTEGER IFIN
      INTEGER IFOUT

*  Status:
      INTEGER STATUS            ! Global status

*  Local Parameters:
      INTEGER MAXLEN
      PARAMETER ( MAXLEN = 1024 )

*  Local Variables:
      CHARACTER * ( 1 ) TAB     ! The <TAB> character
      CHARACTER * ( MAXLEN ) LINE ! Input/output buffer
      CHARACTER * ( MAXLEN ) COLS ! Input/output buffer
      INTEGER IAT               ! Position in string
      INTEGER ICOL              ! Number of columns
      INTEGER IEND              ! Position of end of string
      INTEGER NCHAR             ! Number of characters written
      LOGICAL OK                ! Loop for more lines flag
      INTEGER I                 ! Loop variable

*.

*  Check the global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialisation.
      TAB = CHAR( 9 )

*  First read the file header, until we get to the dash line. All
*  information before this is discarded, except the column name line
*  which is used to construct the header.
      OK = .TRUE.
 1    CONTINUE
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL FIO_READF( IFIN, LINE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            IF ( LINE( 1: 1 ) .EQ. '-' ) THEN
               OK = .FALSE.
            ELSE

*  Save line as columns names preceed dash line.
               COLS = LINE
            END IF
         END IF
         GO TO 1
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Decode column line creating the header region.
      ICOL = 0
      IAT = 1
      OK = .TRUE.
 2    CONTINUE
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL GAI1_NXTAB( COLS, IAT, IEND, STATUS )
         IF ( IEND .NE. 0 ) THEN
            ICOL = ICOL + 1

*  Add column description.
            LINE = '# '
            CALL CHR_ITOC( ICOL, LINE( 3: ), NCHAR )
            LINE( NCHAR + 4: ) = COLS( IAT: IEND )
            IAT = NCHAR + 4 + ( IEND - IAT + 1 )
            CALL FIO_WRITE( IFOUT, LINE( :IAT ), STATUS )

*  Increment position within COLS string for next word.
            IAT = IEND + 1
         ELSE

*  No more column names.
            OK = .FALSE.
         END IF
         GO TO 2
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Now convert data. This just consists of removing the <TAB> characters
*  and replacing with spaces.
      OK = .TRUE.
 3    CONTINUE
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL FIO_READ( IFIN, LINE, NCHAR, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            DO 4 I = 1, NCHAR
               IF ( LINE( I : I ) .EQ. TAB ) THEN
                  LINE( I : I ) = ' '
               END IF
 4          CONTINUE
            CALL FIO_WRITE( IFOUT, LINE( : NCHAR ), STATUS )
         END IF
         GO TO 3
      END IF
      IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Exit in error label.
 99   CONTINUE
      END
