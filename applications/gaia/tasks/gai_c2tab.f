      SUBROUTINE GAI_C2TAB( CI, FI, IDCOL, XCOL, YCOL, RACOL, DECCOL,
     :                      STATUS )
*+
*  Name:
*     GAI_C2TAB

*  Purpose:
*     Convert a CAT catalogue into a "tab table".

*  Language:
*     Fortran-77

*  Invocation:
*     CALL GAI_C2TAB( CI, FI, XCOL, YCOL, RACOL, DECCOL, STATUS )

*  Description:
*     This routine access a previously opened CAT catalogue and converts
*     it into a "tab table" representation. A "tab table" is a text
*     format that has the following format:
*
*        Title
*        #  comments
*        parameter1: value1
*        parameter2: value2
*        .
*        .
*        column_name1 <tab> column_name2 <tab> ....
*        --------------
*        value1 <tab> value2 <tab> value3 <tab> ...
*        .
*        .
*
*     The tab table returned by this routine may have the special
*     parameters, x_col, y_col, id_col, ra_col and dec_col, set
*     depending on what heuristics apply (there's a lot of guess work).

*  Arguments:
*     CI = INTEGER (Given)
*        Identifier of the catalogue to be converted.
*     FI = INTEGER (Given)
*        Fortran unit number of output file.
*     IDCOL = INTEGER (Given)
*        The position, within the catalogue, of the ID column. If this
*        doesn't exist and the first column is a positional one
*        (i.e. RA, DEC, X or Y) then a pseudo index will be generated.
*     XCOL = INTEGER (Given and Returned)
*        The position, within the catalogue, of the X column. This will
*        be guessed, if the value is set to -1.
*     YCOL = INTEGER (Given and Returned)
*        The position, within the catalogue, of the Y column. This will
*        be guessed, if the value is set to -1.
*     RACOL = INTEGER (Given and Returned)
*        The position, within the catalogue, of the RA column. This will
*        be guessed, if the value is set to -1.
*     DECCOL = INTEGER (Given and Returned)
*        The position, within the catalogue, of the DECCOL column. This
*        will be guessed, if the value is set to -1.
*     STATUS = INTEGER (Given and Returned)
*        The global status on exit.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council
*     Copyright (C) 2009 Science and Technology Facilities Council
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
*     22-SEP-1998 (PWD):
*        Original version.
*     04-JUN-1999 (PWD):
*        Added check if RA and DEC columns have these names. {HOURS}
*        isn't a strong enough check, these can also be qualified by
*        {+IHMS.3} type strings. In which case we cannot "guess" which
*        column is RA. To do this add a check if the name is RA.
*     {enter_changes_here}

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
      INTEGER FI
      INTEGER IDCOL
      INTEGER XCOL
      INTEGER YCOL
      INTEGER DECCOL
      INTEGER RACOL

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of a string

*  Local Parameters:
      INTEGER MAXLEN
      PARAMETER ( MAXLEN = 4096 )
      DOUBLE PRECISION PI
      PARAMETER ( PI = 3.141592653589793238462643 )

*  Local Variables:
      CHARACTER * ( 1 ) COMM    ! Column comments (lost)
      CHARACTER * ( 1 ) EXCEPT  ! Exception value (not used)
      CHARACTER * ( 1 ) EXPR    ! Virtual column expression (not used).
      CHARACTER * ( 1 ) TAB     ! Tab character
      CHARACTER * ( 30 ) CLASS ! Class of textual information
      CHARACTER * ( 80 ) SYMBOL( 10 ) ! Incoming symbol parameter (may span more than one-line)
      CHARACTER * ( CAT__SZCMP ) LNAME ! Name of a component
      CHARACTER * ( CAT__SZCMP ) NAME ! Name of a component
      CHARACTER * ( CAT__SZEXP ) EXTFMT ! Column external format
      CHARACTER * ( CAT__SZTXL ) TEXT ! Textual information
      CHARACTER * ( CAT__SZUNI ) UNITS ! Units of column
      CHARACTER * ( CAT__SZVAL ) VALUE ! Value of a component
      CHARACTER * ( MAXLEN ) LINE ! Line buffer for output
      DOUBLE PRECISION DATE     ! Column modification date
      DOUBLE PRECISION SCALE    ! Column scale factor
      DOUBLE PRECISION ZERO     ! Column zero point
      INTEGER COL( CAT__MXCOL ) ! Column identifiers
      INTEGER CSIZE             ! Characters in field
      INTEGER DIMS              ! Dimension of field (must be 0)
      INTEGER DTYPE             ! Column data type
      INTEGER GENUS             ! Genus of column
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! String position counter
      INTEGER ICUR              ! Current insertion position in line
      INTEGER IOSTAT            ! Internal read status
      INTEGER J                 ! Loop variable
      INTEGER N                 ! Counter
      INTEGER NCHAR             ! Number of characters used to encode value
      INTEGER NULL              ! NULL strategy
      INTEGER NUMCOL            ! Number of columns
      INTEGER NUMIND            ! Number of indices
      INTEGER NUMPAR            ! Number of parameters
      INTEGER NUMROW            ! Number of rows
      INTEGER NUMSYM            ! Number of symbol descriptions found
      INTEGER ORDER             ! Sorting order
      INTEGER QI                ! Parameter identifier
      INTEGER SIZEA( 1 )        ! Size of dimension (must be 0)
      INTEGER ULEN              ! Used length of string
      LOGICAL ADDIND            ! Whether to add a pseudo index
      LOGICAL AREDEG            ! TRUE if world coordinates are already in degrees.
      LOGICAL DONE              ! End of textual information.
      LOGICAL NULFLG            ! Value is NULL
      LOGICAL PRFDSP            ! Preferential display flag
      LOGICAL SKIP              ! Whether to skip parameter
*.

*  Check the global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First get the details of the catalogue.
      CALL CAT_TDETL( CI, CAT__GPHYS, NUMROW, NUMCOL, NUMIND, NUMPAR,
     :                DATE, STATUS )

*  Now get identifiers for all the columns.
      DO 1 I = 1, NUMCOL
         CALL CAT_TNDNT( CI, CAT__FITYP, I, COL( I ), STATUS )
 1    CONTINUE

*  Set the TAB character
      TAB = CHAR( 9 )

*  World coordinates are in degrees by default.
      AREDEG = .TRUE.

*  No symbols if found yet.
      NUMSYM = 0

*  Title:
*  ======
      WRITE( FI, '(A)' ) 'cat2tab'

*  Textual information:
*  ====================
*
*  Only want AST and COMMENT (AST for WCS information).
      WRITE( FI, '(A)' ) ' '
      WRITE( FI, '(A)' ) '# Textual information:'
      DONE = .FALSE.
 8    CONTINUE
      IF ( .NOT. DONE ) THEN
         CALL CAT_GETXT( CI, DONE, CLASS, TEXT, STATUS )
         IF ( .NOT. DONE .AND.
     :        ( CLASS .EQ. 'COMMENT' .OR. CLASS .EQ. 'AST' ) ) THEN

*  Write the line, note we skip "COMMENT" which is returned from the
*  FITS format and normalize on "#T".
 103        FORMAT( '#T', A )
            IF ( TEXT( : 8 ) .EQ. 'COMMENT ' ) THEN
               WRITE( FI, 103 ) TEXT( 9 : CHR_LEN( TEXT ) )
            ELSE
               WRITE( FI, 103 ) TEXT( 3 : CHR_LEN( TEXT ) )
            END IF
         END IF
         GO TO 8
      END IF

*  Parameters:
*  ===========

*  Loop for all the parameter getting their formatted values and writing
*  them, together with their names, to the table.
      WRITE( FI, '(A)' ) ' '
      WRITE( FI, '(A)' ) '# Parameters:'
      DO 2 I = 1, NUMPAR

*  Get an identifier for the parameter.
         CALL CAT_TNDNT( CI, CAT__QITYP, I, QI, STATUS )

*  Add the parameter details on a line, then follow this by the real
*  parameter statement. Note that this format is fixed so we can recover
*  this information if the catalogue is converted back.
         CALL CAT_PINQ( QI, 1, CI, NAME, DTYPE, CSIZE, DIMS, SIZEA,
     :                  UNITS, EXTFMT, PRFDSP, COMM, VALUE, DATE,
     :                  STATUS )
         CALL CAT_EGT0C( QI, VALUE, NULFLG, STATUS )
 101     FORMAT( '#P', A17, I3, I5, A22, A22, L2 )

*  Symbol may span more than one line (and are suffixed by an integer
*  from 1 to 9). No suffix is the same as 1.
         IF ( NAME( :6 ) .EQ. 'SYMBOL' ) THEN
            IF ( NUMSYM .EQ. 0 ) THEN
               WRITE( FI, 101 ) 'SYMBOL', DTYPE, CSIZE, UNITS, EXTFMT,
     :                          PRFDSP
            END IF

            NUMSYM = NUMSYM + 1
            IF ( NAME( 7: 7 ) .NE. ' ' ) THEN
               READ( NAME( 7: 7 ), '(I1)', IOSTAT = IOSTAT ) N
               IF ( IOSTAT .NE. 0 ) N = 1
            ELSE
               N = 1
            END IF
            SYMBOL( N ) = VALUE
         ELSE

*  Check for known parameters, which can be case folded and truncated by
*  conversion into CAT (esp CAT/FITS). SKIP positional parameters that
*  cannot be correct (CAT has no strict column order).
            SKIP = .FALSE.
            IF ( NAME .EQ. 'SHORT_NA' ) THEN
               NAME = 'short_name'
            ELSE IF ( NAME .EQ. 'SERV_TYP' ) THEN
               NAME = 'serv_type'
            ELSE IF ( NAME .EQ. 'LONG_NAM' ) THEN
               NAME = 'long_name'
            ELSE IF ( NAME .EQ. 'URL' ) THEN
               NAME = 'url'
            ELSE IF ( NAME .EQ. 'SEARCH_C' ) THEN
               NAME = 'search_cols'
            ELSE IF ( NAME .EQ. 'SORT_COL' ) THEN
               NAME = 'sort_cols'
            ELSE IF ( NAME .EQ. 'SORT_ORD' ) THEN
               NAME = 'sort_order'
            ELSE IF ( NAME .EQ. 'SHOW_COL' ) THEN
               NAME = 'show_cols'
            ELSE IF ( NAME .EQ. 'COPYRIGH' ) THEN
               NAME = 'copyright'
            ELSE IF ( NAME .EQ. 'ID_COL' ) THEN
               SKIP = .TRUE.
            ELSE IF ( NAME .EQ. 'RA_COL' ) THEN
               SKIP = .TRUE.
            ELSE IF ( NAME .EQ. 'DEC_COL' ) THEN
               SKIP = .TRUE.
            END IF

*  Write the parameter description line.
            IF ( .NOT. SKIP ) THEN
               WRITE( FI, 101 ) NAME, DTYPE, CSIZE, UNITS, EXTFMT,PRFDSP

*  Add the "parameter : value" line.
               WRITE( FI, '(A)') NAME( :CHR_LEN( NAME ) )//': '//
     :                           VALUE( :CHR_LEN( VALUE ) )
            END IF
         END IF
 2    CONTINUE

*  Column names:
*  =============

*  Check the columns for special significance.
      WRITE( FI, '(A)' ) ' '
      WRITE( FI, '(A)' ) '# Column details:'
      DO 3 I = 1, NUMCOL

*  Get the details of the column.
         CALL CAT_CINQ( COL( I ), 1, CI, NAME, GENUS, EXPR, DTYPE,
     :                  CSIZE, DIMS, SIZEA, NULL, EXCEPT, SCALE, ZERO,
     :                  ORDER, UNITS, EXTFMT, PRFDSP, COMM, DATE,
     :                  STATUS )

*  Check units for special significance, radians or degrees are assumed
*  to be possible sky coordinates. WFCAM has Radians, so make case insensitive.
         CALL CHR_LCASE( UNITS )
         CALL CHR_LDBLK( UNITS )
         IF ( UNITS( :7 ) .EQ. 'radians' .OR.
     :        UNITS( :6 ) .EQ. 'degree' ) THEN

*  Not degrees? Only change when still searching for any RA and DEC.
            IF ( RACOL .EQ. -1 .OR. DECCOL .EQ. -1 ) THEN
               IF ( UNITS( :7 ) .EQ. 'radians' ) THEN
                  AREDEG = .FALSE.
               ELSE
                  AREDEG = .TRUE.
               END IF
            END IF

*  Assuming this is a column with angle data. This is either an RA or
*  DEC. If qualified by {HOURS}, {HMS or the name is some variation of
*  RA/Ra/r.a./Rightxxx/alphaxxx, then assume RA, otherwise it is a
*  DEC. Note we need both of these to have a valid match, but we don't
*  check for that. Check for DAz which is JCMT AZEL coordinates.
            IF ( UNITS( 8: ) .EQ. '{hours}' .OR.
     :           UNITS( 8:11 ) .EQ. '{hms' ) THEN
               IF ( RACOL .EQ. -1 ) RACOL = I - 1
            ELSE
               LNAME = NAME
               CALL CHR_LCASE( LNAME )
               CALL CHR_LDBLK( LNAME )
               IF ( LNAME( :2 ) .EQ. 'ra' .OR.
     :              LNAME( :5 ) .EQ. 'right' .OR.
     :              LNAME( :4 ) .EQ. 'r.a.' .OR.
     :              LNAME( :7 ) .EQ. 'x_world' .OR.
     :              LNAME( :5 ) .EQ. 'alpha' .OR.
     :              LNAME       .EQ. 'daz' ) THEN
                  IF ( RACOL .EQ. -1 ) RACOL = I - 1
               ELSE

*  Assume this is declination, unless name starts with "pos", which is
*  most likely a position angle (for an ellipse).
                  IF ( DECCOL .EQ. -1 .AND.
     :                 LNAME( :3 ) .NE. 'pos' ) THEN
                     DECCOL = I - 1
                  END IF
               END IF
            END IF

*  Check for SExtractor specific names without units. SExtractor world
*  coordinates are in degrees, and shouldn't be converted.
         ELSE IF ( NAME .EQ. 'X_WORLD' ) THEN
            IF ( RACOL .EQ. -1 ) RACOL = I - 1
         ELSE IF ( NAME .EQ. 'Y_WORLD' ) THEN
            IF ( DECCOL .EQ. -1 ) DECCOL = I - 1
         ELSE IF ( NAME .EQ. 'X_IMAGE' ) THEN
            IF ( XCOL .EQ. -1 ) XCOL = I - 1
         ELSE IF ( NAME .EQ. 'Y_IMAGE' ) THEN
            IF ( YCOL .EQ. -1 ) YCOL = I - 1

*  POLPACK names for X and Y.
         ELSE IF ( NAME .EQ. 'X' ) THEN
            IF ( XCOL .EQ. -1 ) XCOL = I - 1
         ELSE IF ( NAME .EQ. 'Y' ) THEN
            IF ( YCOL .EQ. -1 ) YCOL = I - 1

*  Look for incoming IDCOL (as a column, seems odd?).
         ELSE IF ( NAME .EQ. 'ID_COL' ) THEN
            IF ( IDCOL .EQ. -1 ) IDCOL = I - 1
         END IF

*  Add these to the output file. Used a fixed format on a single line so
*  that we can recover this easily. Note we abandon the character values
*  that are not useful, in order to keep the line length under control.
         WRITE( FI, 102 ) NAME, DTYPE, CSIZE, NULL, SCALE, ZERO,
     :                    ORDER, UNITS, EXTFMT, PRFDSP
 102     FORMAT( '#C', A17, I3, I5, I2, G15.7, G15.7, I2,
     :               A22, A22, L2 )
 3    CONTINUE

*  If no IDCOL has been identified and the first column is a positional
*  one, then set up to add a simple index column.
      IF ( IDCOL .EQ. -1 .AND. ( RACOL .EQ. 0 .OR. DECCOL .EQ. 0 .OR.
     :                           XCOL .EQ. 0 .OR. YCOL .EQ. 0 ) ) THEN
         ADDIND = .TRUE.

*  Move the positional columns up one place.
         IF ( RACOL .NE. -1 ) RACOL = RACOL + 1
         IF ( DECCOL .NE. -1 ) DECCOL = DECCOL + 1
         IF ( XCOL .NE. -1 ) XCOL = XCOL + 1
         IF ( YCOL .NE. -1 ) YCOL = YCOL + 1
         IDCOL = 0

*  Add a fake column description.
         NAME = 'ID_COL'
         DTYPE = CAT__TYPEI
         CSIZE = 0
         NULL = CAT__NULLD
         SCALE = 1.0
         ZERO = 0.0
         ORDER = CAT__ASCND
         UNITS = ' '
         EXTFMT = 'I9'
         PRFDSP = .FALSE.
         WRITE( FI, 102 ) NAME, DTYPE, CSIZE, NULL, SCALE, ZERO,
     :                    ORDER, UNITS, EXTFMT, PRFDSP
      ELSE
         ADDIND = .FALSE.
      END IF

*  OK, if we have located special columns then add these to the header
*  section. Note that if world coordinates are not located then this is
*  recorded (as -1). This is necessary as information about the presence
*  of these coordinates may persist in GAIA.
      IF ( IDCOL .NE. -1 ) THEN
         CALL CHR_ITOC( IDCOL, VALUE, ICUR )
         WRITE( FI, '(A)') 'id_col: '// VALUE( :ICUR )
      END IF
      CALL CHR_ITOC( RACOL, VALUE, ICUR )
      WRITE( FI, '(A)' ) 'ra_col: '// VALUE( :ICUR )
      CALL CHR_ITOC( DECCOL, VALUE, ICUR )
      WRITE( FI, '(A)' ) 'dec_col: '// VALUE( :ICUR )
      IF ( XCOL .NE. -1 .AND. YCOL .NE. -1 ) THEN
         CALL CHR_ITOC( XCOL, VALUE, ICUR )
         WRITE( FI, '(A)' ) 'x_col: '// VALUE( :ICUR )
         CALL CHR_ITOC( YCOL, VALUE, ICUR )
         WRITE( FI, '(A)' ) 'y_col: '// VALUE( :ICUR )
      END IF

*  Move positional columns back to relative in input catalogue.
      IF ( ADDIND ) THEN
         IF ( RACOL .NE. -1 ) RACOL = RACOL - 1
         IF ( DECCOL .NE. -1 ) DECCOL = DECCOL - 1
         IF ( XCOL .NE. -1 ) XCOL = XCOL - 1
         IF ( YCOL .NE. -1 ) YCOL = YCOL - 1
      END IF

*  If no symbol has been found and we have some positional columns
*  then add a very simple symbol (a circle of size 4 pixels), otherwise
*  construct the line.
      IF ( NUMSYM .EQ. 0 .AND.
     :     ( ( XCOL .NE. -1 .AND. YCOL .NE. -1 ) .OR.
     :       ( RACOL .NE. -1 .AND. DECCOL .NE. -1 ) )
     :   ) THEN
         WRITE( FI, '(A)' )
     :      'symbol: {} {circle {} {} {} {} {}} {4.0 {}}'
      ELSE IF ( NUMSYM .GT. 0 ) THEN
         LINE = 'symbol: '
         IAT = 8
         DO 7 I = 1, NUMSYM
            LINE( IAT: ) = SYMBOL( I )
            IAT = IAT + CHR_LEN( SYMBOL( I ) )
 7       CONTINUE
         WRITE( FI, '(A)' ) LINE( :IAT )
      END IF

*  Now add the column names.
      WRITE( FI, '(A)' ) ' '
      IF ( ADDIND ) THEN
         NAME = 'ID_COL'
         LINE = NAME // TAB
         ICUR = CAT__SZCMP + 2
      ELSE
         LINE = ' '
         ICUR = 1
      END IF
      DO 4 I = 1, NUMCOL
         CALL CAT_TIQAC( COL( I ), 'NAME', NAME, STATUS )
         IF ( I .NE. NUMCOL ) THEN
            LINE( ICUR: ) = NAME // TAB
            ICUR = ICUR + CAT__SZCMP + 1
         ELSE
            LINE( ICUR: ) = NAME
         END IF
 4    CONTINUE
      WRITE( FI, '(A)' ) LINE( :CHR_LEN( LINE ) )

*  Add separator between header and values.
      WRITE( FI, '(A)' ) '----'

*  Values:
*  =======

*  Now add the tab table of values.
      DO 5 I = 1, NUMROW
         ICUR = 1
         CALL CAT_RGET( CI, I, STATUS )

*  In needed add the pseudo index.
         IF ( ADDIND ) THEN
            CALL CHR_ITOC( I, VALUE, ULEN )
            LINE( ICUR: ) = VALUE( :ULEN ) // TAB
            ICUR = ICUR + ULEN + 1
         END IF
         DO 6 J = 1, NUMCOL
            IF ( J .EQ. ( RACOL + 1 ) .OR. J .EQ. ( DECCOL + 1 ) ) THEN

*  Need to get double precision value and may need to convert to degrees
*  from radians.
               CALL CAT_EGT0D( COL( J ), SCALE, NULFLG, STATUS )
               IF ( .NOT. AREDEG ) THEN
                  SCALE = SCALE * 180.0D0 / PI
               END IF
               CALL CHR_DTOC( SCALE, VALUE, NCHAR )
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL ERR_REP( ' ',
     :            'Failed to extract angle column data', STATUS )
                  GO TO 99
               END IF
            ELSE
               CALL CAT_EGT0C( COL( J ), VALUE, NULFLG, STATUS )
            END IF

*  The NULL value is '\0', don't want that in a TST so convert to <NULL>.
            IF ( NULFLG ) THEN
               VALUE = '<NULL>'
            END IF

            IF ( J .NE. NUMCOL ) THEN
               ULEN = CHR_LEN( VALUE )
               LINE( ICUR: ) = VALUE( :ULEN ) // TAB
               ICUR = ICUR + ULEN + 1
            ELSE
               LINE( ICUR: ) = VALUE
            END IF
 6       CONTINUE
         WRITE( FI, '(A)' ) LINE( :CHR_LEN( LINE ) )
 5    CONTINUE

*  End of routine.
 99   CONTINUE
      END
