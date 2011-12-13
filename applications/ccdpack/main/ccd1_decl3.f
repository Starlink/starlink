      SUBROUTINE CCD1_DECL3( LINE, LINNUM, NREC, NVAL, NROW, INDEXS,
     :                       VALUES, STATUS )
*+
*  Name:
*     CCD1_DECL3

*  Purpose:
*     Decodes a line of characters into an index and trailing values.
*     Entering the results in a given position in output arrays.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     SUBROUTINE CCD1_DECL3( LINE, LINNUM, NREC, NVAL, NROW, INDEXS,
*                            VALUES, STATUS )

*  Description:
*     This routine decodes a line of characters into numeric values.
*     It is assumed that the line only contains words separated by
*     blanks (lines read in by CCD1_RDLIN are recommended).
*     The first value is assumed to be an integer (the index or
*     identifier) the next values are the data values, these are
*     returned in double precision. The lines are parsed using word
*     extraction routines not via fortran formatting.  If no values can
*     be read from the file then status will be set by FIO as usual.
*     The output values are entered into the INDEXS and VALUES arrays
*     in a given row.

*  Arguments:
*     LINE = CHARACTER * ( * ) (Given)
*        Line of blank separated words to be decoded into an index and
*        trailing values.
*     LINNUM = INTEGER (Given)
*        A line number to be issued with error messages.
*     NREC = INTEGER (Given)
*        The number of records in arrays INDEXS and VALUES.
*     NVAL = INTEGER (Given)
*        The number of values which may be entered in a row of
*        VALUES. (I.e. its second dimension).
*     NROW = INTEGER (Given)
*        The row of INDEXS and VALUES in which the decoded values are to
*        be entered.
*     INDEXS( NREC ) = INTEGER (Given and Returned)
*        Array to hold the first column value interpreted as an integer.
*     VALUES( NREC, NVAL ) = DOUBLE PRECISION (Given and Returned)
*        Array to hold the values which are decoded from the input
*        string.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
*     Copyright (C) 1995 Central Laboratory of the Research Councils.
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUL-1992 (PDRAPER):
*        Original Version.
*     17-MAR-1995 (PDRAPER):
*        Removed unused NCHAR argument.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) LINE
      INTEGER LINNUM
      INTEGER NREC
      INTEGER NVAL
      INTEGER NROW

*  Arguments Given and Returned:
      INTEGER INDEXS( NREC )
      DOUBLE PRECISION VALUES( NREC, NVAL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION TRNVAL
      INTEGER FIRST
      INTEGER LAST
      LOGICAL NOTFND
      INTEGER IAT
      INTEGER NTRAN

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Look for index.
      IAT = 1
      CALL KPG_NXWRD( LINE, IAT, FIRST, LAST, NOTFND, STATUS )
      IF ( NOTFND .OR. STATUS .NE. SAI__OK ) THEN

*  No index - in fact nothing. Issue an error message and abort.
         STATUS = SAI__ERROR
         CALL MSG_SETI( 'LINNUM', LINNUM )
         CALL ERR_REP( 'CCD1_DECL3ERR1',
     :   '  Line ^LINNUM contains no valid values', STATUS )
            GO TO 99
      ELSE

*  Have an index decode it as an INTEGER.
         CALL CHR_CTOI( LINE( FIRST : LAST ), INDEXS( NROW ), STATUS )

*  Increment IAT past this word.
         IAT = LAST + 1

*  In this translation fails then say what we expected to happen.
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL MSG_SETI( 'LINNUM', LINNUM )
            CALL ERR_REP( 'CCD1_DECL3ERR2',
     :         '  Index not an integer - line ^LINNUM', STATUS )
            GO TO 99
         END IF
      END IF

*  Translate the rest of the values.
      NTRAN = 0
      NOTFND = .FALSE.

*  Loop while STATUS is ok and a word has been found
 1    CONTINUE                ! Start of 'DO WHILE' loop
      IF ( STATUS .EQ. SAI__OK .AND. .NOT. NOTFND ) THEN
         CALL KPG_NXWRD( LINE, IAT, FIRST, LAST, NOTFND, STATUS )

*  Change search to new position for next time around.
         IAT = LAST + 1
         IF ( STATUS .EQ. SAI__OK .AND. .NOT. NOTFND ) THEN

*  Translate the values.
            CALL CHR_CTOD( LINE( FIRST: LAST ), TRNVAL, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN

*  Could not translate the value.
               CALL MSG_SETI( 'LINNUM', LINNUM )
               CALL MSG_SETC( 'VALUE', LINE( FIRST : LAST ) )
               CALL ERR_REP( 'CCD1_DECL1ERR3',
     :'  Could not translate value (^VALUE) into floating point -'
     ://' line ^LINNUM ', STATUS )
            ELSE

*  Translation ok.
               NTRAN = NTRAN + 1
               IF ( NTRAN .LE. NVAL ) THEN
                  VALUES( NROW, NTRAN ) = TRNVAL
               END IF
            END IF
         END IF
         GO TO 1
      END IF

*  How many values have we translated.
      IF ( NTRAN .GT. NVAL .AND. STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETI( 'LINNUM', LINNUM )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_DECL3ERR4',
     :   '  File contains too many values - line ^LINNUM' ,STATUS )
      ELSE IF ( NTRAN .LT. NVAL .AND. STATUS .EQ. SAI__OK ) THEN
         CALL MSG_SETI( 'LINNUM', LINNUM )
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CCD1_DECL3ERR5',
     :   '  File contains too few values - line ^LINNUM' ,STATUS )
      END IF

 99   CONTINUE
      END

* $Id$
