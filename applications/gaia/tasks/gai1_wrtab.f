      SUBROUTINE GAI1_WRTAB( CI, FI, NUMCOL, NUMROW, NUMPAR, LINLEN,
     :                       NLINE, TABLE, STATUS )
*+
*  Name:
*     GAI1_WRTAB

*  Purpose:
*     Writes a tab table version of a CAT catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAI1_WRTAB( CI, FI, NUMCOL, NUMROW, NUMPAR, LINLEN,
*                      NLINE, TABLE, STATUS )

*  Description:
*     This routine writes a "tab table" version of a CAT catalogue into
*     a character array. Each element of the array contains a line of
*     the table, which are either parameters or values.
*
*     The catalogue parameters are stored in lines that have the format:
*
*        parameter: value
*
*      And the data values are stored in a region following this that
*      looks like:
*
*        column_name1 <tab> column_name2 <tab> ....
*        --------------
*        value1 <tab> value2 <tab> value3 <tab> ...
*
*      The special added to the table are:
*
*         ra_col, dec_col, x_col, y_col, id_col and symbol,
*
*      if suitable columns are located.

*  Arguments:
*     CI = INTEGER (Given)
*        The identifier of catalogue.
*     FI( NUMCOL ) = INTEGER (Given)
*        Column identifiers.
*     NUMCOL = INTEGER (Given)
*        The number of columns in the catalogue.
*     NUMROW = INTEGER (Given)
*        The number of rows in the catalogue.
*     NUMPAR = INTEGER (Given)
*        The number of parameters in the catalogue.
*     TABLE( NLINE ) = CHARACTER * ( LINLEN ) (Returned)
*        The extracted "tab table".
*     LINLEN = INTEGER (Given)
*        The length of each line/element in TABLE.
*     NLINE = INTEGER (Given)
*        The number of lines/elements in TABLE.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
*     PWD: Peter W. Draper (STARLINK, Durham University)
*     {enter_new_authors_here}

*  History:
*     22-SEP-1998 (PWD):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CAT_PAR'         ! CAT parameters

*  Arguments Given:
      INTEGER CI
      INTEGER NUMCOL
      INTEGER FI( NUMCOL )
      INTEGER NUMROW
      INTEGER NUMPAR
      INTEGER LINLEN
      INTEGER NLINE

*  Arguments Returned:
      CHARACTER * ( * ) TABLE( NLINE )

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of string

*  Local Variables:
      CHARACTER * ( 1 ) TAB     ! Tab character
      CHARACTER * ( CAT__SZCMP ) NAME ! Name of a component
      CHARACTER * ( CAT__SZUNI ) UNITS ! Units of column
      CHARACTER * ( CAT__SZVAL ) VALUE ! Value of a component
      INTEGER DECCOL            ! Index of X indices
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Current table element
      INTEGER ICUR              ! Current insertion position in line
      INTEGER ULEN              ! Used length of string
      INTEGER J                 ! Loop variable
      INTEGER QI                ! Parameter identifier
      INTEGER RACOL             ! Index of X indices
      INTEGER XCOL              ! Index of X indices
      INTEGER YCOL              ! Index of Y indices
      LOGICAL NULFLG            ! NULL value flag

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start inserting at beginning of table.
      IAT = 1

*  Set the TAB character
      TAB = CHAR( 9 )

*  Title:
*  ======
      TABLE( IAT ) = 'Cattable'

*  Parameters:
*  ===========

*  Loop for all the parameter getting their formatted values and writing
*  them, together with their names, to the table.
      DO 1 I = 1, NUMPAR
         CALL CAT_TNDNT( CI, CAT__QITYP, I, QI, STATUS )
         CALL CAT_TIQAC( QI, 'NAME', NAME, STATUS)
         CALL CAT_TIQAC( QI, 'VALUE', VALUE, STATUS)
         IAT = IAT + 1
         TABLE( IAT ) = NAME( :CHR_LEN( NAME ) )//': '//VALUE

*  XXX How to preserve comments and other information about parameter?
 1    CONTINUE


*  Column names:
*  =============

*  Check the columns for special significance.
      RACOL = 0
      DECCOL = 0
      XCOL = 0
      YCOL = 0
      DO 2 I = 1, NUMCOL

*  Get units and name of column.
         CALL CAT_TIQAC( FI( I ), 'UNITS', UNITS, STATUS )
         CALL CAT_TIQAC( FI( I ), 'NAME', NAME, STATUS )
         IF ( UNITS( :7 ) .EQ. 'RADIANS' ) THEN

*  Column with angle data. This is either an RA or DEC. If qualified by
*  {HOURS} then assume RA, otherwise it is a DEC. Note we need both of
*  these to have a valid match.
            IF ( UNITS( 8: ) .EQ. '{HOURS}' ) THEN
               IF ( RACOL .EQ. 0 ) RACOL = I
            ELSE
               IF ( DECCOL .EQ. 0 ) DECCOL = I
            END IF

*  Check for SExtractor specific names.
         ELSE IF ( NAME .EQ. 'X_WORLD' ) THEN
            RACOL = I
         ELSE IF ( NAME .EQ. 'Y_WORLD' ) THEN
            DECCOL = I
         ELSE IF ( NAME .EQ. 'X_IMAGE' ) THEN
            XCOL = I
         ELSE IF ( NAME .EQ. 'Y_IMAGE' ) THEN
            YCOL = I
         END IF
 2    CONTINUE

*  OK, if we have located special columns then add these to the header
*  section.
      IF ( RACOL .NE. 0 .AND. DECCOL .NE. 0 ) THEN
         IAT = IAT + 1
         CALL CHR_ITOC( RACOL - 1, VALUE, ICUR )
         TABLE( IAT ) = 'ra_col: '// VALUE( :ICUR )
         IAT = IAT + 1
         CALL CHR_ITOC( DECCOL - 1, VALUE, ICUR )
         TABLE( IAT ) = 'dec_col: '// VALUE( :ICUR )
      END IF
      IF ( XCOL .NE. 0 .AND. YCOL .NE. 0 ) THEN
         IAT = IAT + 1
         CALL CHR_ITOC( XCOL - 1, VALUE, ICUR )
         TABLE( IAT ) = 'x_col: '// VALUE( :ICUR )
         IAT = IAT + 1
         CALL CHR_ITOC( YCOL - 1, VALUE, ICUR )
         TABLE( IAT ) = 'y_col: '// VALUE( :ICUR )
      END IF

*  And the symbol.
      IAT = IAT + 1
      CALL CAT_TIQAC( FI( 1 ), 'NAME', NAME, STATUS )
      TABLE( IAT ) = 'symbol: '//NAME//' diamond  8.0'

*  Now add the column names.
      ICUR = 1
      IAT = IAT + 1
      DO 3 I = 1, NUMCOL
         CALL CAT_TIQAC( FI( I ), 'NAME', NAME, STATUS )
         IF ( I .NE. NUMCOL ) THEN
            TABLE( IAT )( ICUR: ) = NAME // TAB
            ICUR = ICUR + CAT__SZCMP + 1
         ELSE
            TABLE( IAT )( ICUR: ) = NAME
         END IF
         IF ( ICUR .GE. LINLEN ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ',
     :      'Failed to add column names, internal space exhaused',
     :                    STATUS )
            GO TO 99
         END IF
 3    CONTINUE

*  Add separator between header and values.
      IAT = IAT + 1
      TABLE( IAT ) = '----'

*  Values:
*  =======

*  Now add the tab table of values.
      DO 4 I = 1, NUMROW
         ICUR = 1
         IAT = IAT + 1
         CALL CAT_RGET( CI, I, STATUS )
         DO 5 J = 1, NUMCOL
            CALL CAT_EGT0F( FI( J ), VALUE, NULFLG, STATUS )
            IF ( J .NE. NUMCOL ) THEN
               ULEN = CHR_LEN( VALUE )
               TABLE( IAT )( ICUR: ) = VALUE( :ULEN )// TAB
               ICUR = ICUR + ULEN + 1
            ELSE
               TABLE( IAT )( ICUR: ) = VALUE
            END IF
            IF ( ICUR .GE. LINLEN ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( ' ',
     :         'Failed to add field value, internal space exhaused',
     :                     STATUS )
               GO TO 99
            END IF
 5       CONTINUE
 4    CONTINUE

*  End of routine.
 99   CONTINUE
      END
