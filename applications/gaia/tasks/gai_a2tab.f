      SUBROUTINE GAI_A2TAB( IFIN, FI, IDCOL, XCOL, YCOL, RACOL,
     :                      DECCOL, STATUS )
*+
*  Name:
*     GAI_A2TAB

*  Purpose:
*     Convert an ASCII catalogue into a "tab table".

*  Language:
*     Fortran-77

*  Invocation:
*     CALL GAI_A2TAB( IFIN, FI, XCOL, YCOL, RACOL, DECCOL, STATUS )

*  Description:
*     This routine access a previously opened ASCII file and converts
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
*     parameters, x_col, y_col, id_col, ra_col and dec_col.
*
*     This routine attempts to detect SExtractor ASCII_HEAD format and
*     if no RA/DEC columns are given attempts to locate them in the 2nd
*     and 3rd positions.

*  Arguments:
*     IFIN = INTEGER (Given)
*        FIO file descriptor of input catalogue
*     FI = INTEGER (Given)
*        Fortran UNIT identifier of output file.
*     IDCOL = INTEGER (Given)
*        The position, within the catalogue, of the ID column.
*     XCOL = INTEGER (Given and Returned)
*        The position, within the catalogue, of the X column. This may
*        be guessed, if the value is set to -1.
*     YCOL = INTEGER (Given and Returned)
*        The position, within the catalogue, of the Y column. This may
*        be guessed, if the value is set to -1.
*     RACOL = INTEGER (Given and Returned)
*        The position, within the catalogue, of the RA column. This may
*        be guessed, if the value is set to -1.
*     DECCOL = INTEGER (Given and Returned)
*        The position, within the catalogue, of the DECCOL column. This
*        may be guessed, if the value is set to -1.
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
*     15-OCT-1998 (PWD):
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
      PARAMETER ( MAXLEN = 1024 )

*  Local Variables:
      CHARACTER * ( 1 ) TAB     ! Tab character
      CHARACTER * ( 20 ) VALUE  ! Integer value as character
      CHARACTER * ( MAXLEN ) COLS ! Line buffer for column names
      CHARACTER * ( MAXLEN ) LINE ! Line buffer for input/output
      DOUBLE PRECISION DVAL     ! Just a DBLE variable
      INTEGER I                 ! Loop variable
      INTEGER IA, IB, IC, ID, IAT ! String position indices
      INTEGER ICOL              ! Number of columns
      INTEGER ICUR              ! Current insertion position in line
      INTEGER IVAL              ! Just an integer value
      INTEGER NCHAR             ! Number of characters used to encode value
      LOGICAL FLAG              ! Conversion from RA/DEC string successful
      LOGICAL ISHEAD            ! Is an ASCII_HEAD file
      LOGICAL MORE              ! Loop control variable
      LOGICAL OK                ! Loop control variable

*.

*  Check the global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the TAB character
      TAB = CHAR( 9 )

*  Initialise to appease picky compilers
      ICOL = 0

*  First check if the catalogue is an ASCII_HEAD one. The signature for
*  this is a header section of the format:
*
*     # 1 NAME      Comment      [units]
*
*  Starting at the first line.
      ISHEAD = .FALSE.
      CALL FIO_READF( IFIN, LINE, STATUS )
      IF ( LINE( 1 : 1 ) .EQ. '#' ) THEN

*  Check next word which should be an integer.
         CALL ERR_MARK
         IA = 2
         CALL CHR_FIWS( LINE, IA, STATUS )
         IB = IA
         CALL CHR_FIWE( LINE, IB, STATUS)
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL CHR_CTOI( LINE( IA : IB ), IVAL, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN

*  Probably an ASCII_HEAD file, need at least one more word (the column
*  name).
               IA = IB  + 1
               CALL CHR_FIWS( LINE, IA, STATUS )
               IB = IA + 1
               CALL CHR_FIWE( LINE, IB, STATUS)
               IF ( STATUS .EQ. SAI__OK ) THEN
                  ISHEAD = .TRUE.
               END IF
            END IF
         END IF
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         END IF
         CALL ERR_RLSE
      END IF

*  Ok, if the file isn't an ASCII_HEAD one, we may need to sniff around
*  to see if any identifiable RA and DEC values are in columns 2 and 3.
*  We also need to count the number of columns.
      IF ( .NOT. ISHEAD ) THEN

*  Look for first non-comment line and check it out.
         MORE = .TRUE.
 1       CONTINUE
         IF ( MORE .AND. STATUS .EQ. SAI__OK ) THEN
            CALL FIO_READF( IFIN, LINE, STATUS )
            IF ( LINE( 1 : 1 ) .NE. '#' .AND. STATUS .EQ. SAI__OK ) THEN

*  Non-comment, make sure it isn't just blank.
               IB = CHR_LEN( LINE )
               IF ( IB .GT. 0 ) THEN
                  ICOL = 0
                  ID = 0
                  MORE = .FALSE.
                  IF ( RACOL .EQ. -1 .OR. DECCOL .EQ. -1 ) THEN

*  Get second and third words.
                     IA = 0
                     CALL CHR_FIWS( LINE, IA, STATUS )
                     IB = IA
                     CALL CHR_FIWE( LINE, IB, STATUS)

                     IA = IB + 1
                     CALL CHR_FIWS( LINE, IA, STATUS )
                     IB = IA
                     CALL CHR_FIWE( LINE, IB, STATUS)

                     IC = IB + 1
                     CALL CHR_FIWS( LINE, IC, STATUS )
                     ID = IC
                     CALL CHR_FIWE( LINE, ID, STATUS)
                     IF ( STATUS .EQ. SAI__OK ) THEN

*  Check these for ':' marks. If found attempt a conversion.
                        IAT = INDEX( LINE( IA : IB ), ':' )
                        IF ( IAT .NE. 0 ) THEN
                           IAT = INDEX( LINE( IC : ID ), ':' )
                           IF ( IAT .NE. 0 ) THEN

*  May be sexigesimal.
                              CALL GAI1_S2ANG( LINE( IA : IB ),
     :                                         DVAL, FLAG, STATUS )
                              IF ( FLAG ) THEN
                                 RACOL = 1
                                 DECCOL = 2
                              END IF
                           END IF
                        END IF

*  Don't re-read these columns for total count attempt.
                        ICOL = 3
                     ELSE

*  Error getting three words. Assume we just have less.
                        CALL ERR_ANNUL( STATUS )
                     END IF

                  END IF

*  Count the number of words in this line so we can generate enough
*  column names
                  IF ( STATUS .NE. SAI__OK ) GO TO 99
                  CALL ERR_MARK
                  IA = ID + 1
                  OK = .TRUE.
 2                CONTINUE
                  IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
                     CALL CHR_FIWS( LINE, IA, STATUS )
                     CALL CHR_FIWE( LINE, IA, STATUS)
                     IA = IA + 1
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        ICOL = ICOL + 1
                     ELSE
                        OK = .FALSE.
                     END IF
                     GO TO 2
                  END IF
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                  END IF
                  CALL ERR_RLSE
               END IF
            END IF

*  Back for next line, or exit.
            GO TO 1
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Rewind file back to beginning.
      CALL FIO_RWIND( IFIN, STATUS )

*  Title:
*  ======
      WRITE( FI, '(A)' ) 'asc2tab'

*  Now get or generate names for all the columns.
      COLS = ' '
      IAT = 1
      IF ( ISHEAD ) THEN
         ICOL = 0
         MORE = .TRUE.
 3       CONTINUE
         IF ( MORE .AND. STATUS .EQ. SAI__OK ) THEN
            CALL FIO_READF( IFIN, LINE, STATUS )
            IF ( LINE( 1 : 1 ) .EQ. '#' ) THEN

*  Read next two words. The second should be the column name.
               IA = 2
               CALL CHR_FIWS( LINE, IA, STATUS )
               IB = IA
               CALL CHR_FIWE( LINE, IB, STATUS)

               IC = IB + 1
               CALL CHR_FIWS( LINE, IC, STATUS )
               ID = IC
               CALL CHR_FIWE( LINE, ID, STATUS)
               COLS( IAT: ) = LINE( IC : ID )//TAB
               IAT = IAT + 1 + ( ID - IC + 1 )

*  Check for SExtractor special names (only used if RACOL and DECCOL are
*  not established).
               IF ( LINE( IC : ID ) .EQ. 'X_WORLD' ) THEN
                  IF ( RACOL .EQ. -1 ) RACOL = ICOL
               ELSE IF ( LINE( IC : ID )  .EQ. 'Y_WORLD' ) THEN
                  IF ( DECCOL .EQ. -1 ) DECCOL = ICOL
               ELSE IF ( LINE( IC : ID )  .EQ. 'X_IMAGE' ) THEN
                  IF ( XCOL .EQ. -1 ) XCOL = ICOL
               ELSE IF ( LINE( IC : ID )  .EQ. 'Y_IMAGE' ) THEN
                  IF ( YCOL .EQ. -1 ) YCOL = ICOL
               END IF
               ICOL = ICOL + 1
            ELSE

*  First none comment-line, so end of headers.
               MORE = .FALSE.
               CALL FIO_RWIND( IFIN, STATUS )
            END IF
            GO TO 3
         END IF
      ELSE

*  Automatically generate column names.
         DO 4 I = 1, ICOL
            CALL CHR_ITOC( I, VALUE, NCHAR )
            COLS( IAT: ) = 'COLUMN'//VALUE( :NCHAR )//TAB
            IAT = IAT + NCHAR + 9
 4       CONTINUE
      END IF

*  OK, if we have located special columns then add these to the header
*  section. Note that if world coordinates are not located then this is
*  recorded (as -1).
      IF ( IDCOL .NE. -1 ) THEN
         CALL CHR_ITOC( IDCOL, VALUE, ICUR )
         WRITE( FI, '(A)') 'id_col: '// VALUE( :ICUR )
      END IF
      CALL CHR_ITOC( RACOL, VALUE, ICUR )
      WRITE( FI, '(A)' ) 'ra_col: '// VALUE( :ICUR )
      CALL CHR_ITOC( DECCOL, VALUE, ICUR )
      WRITE( FI, '(A)' ) 'dec_col: '// VALUE( :ICUR )
      RACOL = RACOL + 1
      DECCOL = DECCOL + 1
      IF ( XCOL .NE. -1 .AND. YCOL .NE. -1 ) THEN
         CALL CHR_ITOC( XCOL, VALUE, ICUR )
         WRITE( FI, '(A)' ) 'x_col: '// VALUE( :ICUR )
         CALL CHR_ITOC( YCOL, VALUE, ICUR )
         WRITE( FI, '(A)' ) 'y_col: '// VALUE( :ICUR )
      END IF

*  Add separator between header and values.
      WRITE( FI, '(A)' ) COLS( : CHR_LEN( COLS ) )
      WRITE( FI, '(A)' ) '----'

*  Values:
*  =======
      IF ( STATUS .NE. SAI__OK ) GO TO 99
      MORE = .TRUE.
 5    CONTINUE
      IF ( MORE .AND. STATUS .EQ. SAI__OK ) THEN
         CALL FIO_READF( IFIN, LINE, STATUS )
         IF ( LINE( 1 : 1 ) .NE. '#' .AND. STATUS .EQ. SAI__OK ) THEN

*  Extract each word and append to buffer.
            COLS = ' '
            IA = 1
            IAT = 1
            DO 6 I = 1, ICOL
               CALL CHR_FIWS( LINE, IA, STATUS )
               IB = IA
               CALL CHR_FIWE( LINE, IB, STATUS)
               COLS( IAT: ) = LINE( IA: IB )//TAB
               IAT = IAT + 1 + ( IB - IA + 1 )
               IA = IB + 1
 6          CONTINUE
            WRITE( FI, '(A)' ) COLS( :IAT )
         ELSE

*  Probably end-of-file.
            IF ( STATUS .EQ. FIO__EOF ) THEN
               CALL ERR_ANNUL( STATUS )
               MORE = .FALSE.
            END IF
         END IF
         GO TO 5
      END IF

*  End of routine.
 99   CONTINUE
      END
