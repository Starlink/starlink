      SUBROUTINE GAI1_T2CG( FI, CI, NLOOK, STATUS )
*+
*  Name:
*     GAI1_T2CG

*  Purpose:
*     Convert a arbitrary tab table into a CAT catalogue.

*  Description:
*     This routine reads a file attached to a given FIO identifier and
*     attempts to convert it into a CAT catalogue, under the assumption
*     that the attached file contains a "tab table".
*
*     Since tab-tables do not, in general, have any type information this
*     must be derived for each of the columns of the table. Parameters
*     are always stored as character strings.
*
*     The types used are limited to CHARACTER, INTEGER and DOUBLE
*     PRECISION, plus an attempt at a sexagesimal conversion (if the
*     standard ra_col or dec_col parameters are missing then the 2 and 3
*     columns are checked). The first NLOOK lines of data section of the
*     catalogue are used to determine this information, so it is
*     possible to get it badly wrong.

*  Language:
*     Fortran-77

*  Invocation:
*     CALL GAI1_T2CG( FI, CI, STATUS )

*  Arguments:
*     FI = INTEGER (Given)
*        FIO identifier of the catalogue to be converted.
*     CI = INTEGER (Given)
*        Fortran unit number of output file.
*     NLOOK = INTEGER (Given)
*        The number of lines to "look ahead" in the data section of
*        catalogue to determine what data types are being used.
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
*     02-OCT-1998 (PWD):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants
      INCLUDE 'CAT_PAR'         ! CAT parameters
      INCLUDE 'FIO_ERR'         ! FIO error codes

*  Arguments Given:
      INTEGER FI
      INTEGER CI
      INTEGER NLOOK

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of a string

*  Local Parameters:
      INTEGER MAXLEN
      PARAMETER ( MAXLEN = 4096 )

*  Local Variables:
      CHARACTER * ( CAT__SZCMP ) NAME ! Name of a component
      CHARACTER * ( CAT__SZEXF ) EXTFMT( 10 ) ! Default external formats
      CHARACTER * ( CAT__SZUNI ) UNITS ! Units of column
      CHARACTER * ( MAXLEN ) COLS ! Line buffer for column names
      CHARACTER * ( MAXLEN ) LINE ! Line buffer for output
      CHARACTER * ( MAXLEN ) VALUE ! Value of a component
      DOUBLE PRECISION DVAL     ! Dummy value
      INTEGER COL( CAT__MXCOL ) ! Column identifiers
      INTEGER CSIZE( CAT__MXCOL ) ! Characters in fields
      INTEGER DECCOL            ! Position of DEC column
      INTEGER DLINE             ! Line at which data starts
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position within string
      INTEGER ICOL              ! Number of columns
      INTEGER IEND              ! End character of word
      INTEGER IVAL              ! Dummy value
      INTEGER NDONE             ! Number of lines read from file.
      INTEGER NLINE             ! Data line counter
      INTEGER QI                ! Parameter identifier
      INTEGER RACOL             ! Position of RA column
      INTEGER TYPES( CAT__MXCOL ) ! Data types of the columns
      INTEGER VALLEN            ! VALUE length
      LOGICAL FLAG              ! Conversion success flag
      LOGICAL ISWCS             ! True when table has RA and DEC columns
      LOGICAL OK                ! Loop control flag

*.

*  Check the global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Set the default external formats.
      EXTFMT( CAT__TYPEI ) = 'I9'
      EXTFMT( CAT__TYPED ) = 'G14.7'
      EXTFMT( CAT__TYPEC ) = ' '

*  Default positions of the RA and DEC columns.
      RACOL = 2
      DECCOL = 3
      ISWCS = .FALSE.

*  Now read the input file until we get to a line that starts with
*  '-'. This delimits the end of the parameters.
      OK = .TRUE.
      DLINE = 1
 1    CONTINUE
      IF ( OK ) THEN
         CALL FIO_READF( FI, LINE, STATUS )
         DLINE = DLINE + 1
         IF ( LINE( 1: 1 ) .NE. '-' .AND. STATUS .EQ. SAI__OK ) THEN
            IF ( LINE( 1 : 1 ) .NE. '#' ) THEN

*  Look for parameter : value, separation.
               IAT = INDEX( LINE, ':' )
               IF ( IAT .NE. 0 ) THEN

*  Get the parameter name and value.
                  NAME = LINE( : IAT - 1 )
                  VALUE = LINE( IAT + 1 : )


*  The 'symbol' parameter may be quite long so split this over several
*     records.
                  IF ( NAME( : 6 ) .EQ. 'symbol' ) THEN
                     VALLEN = CHR_LEN( VALUE )
                     I = 1
                     DO 10 IAT = 1, VALLEN, 68
                        WRITE( NAME( 7:7 ), '(I1)' ) I
                        CALL CAT_PPTSC( CI, NAME, VALUE( IAT:IAT+68 ),
     :                                  ' ',QI, STATUS )
                        CALL CAT_TATTI( QI, 'CSIZE', 68, STATUS )
                        I = I + 1
 10                  CONTINUE
                  ELSE

*  Create the parameter. This will just have character format.
                     CALL CAT_PPTSC( CI, NAME, VALUE, ' ', QI, STATUS )
                     CALL CAT_TATTI( QI, 'CSIZE', CHR_LEN( VALUE ),
     :                               STATUS )

*  Check for special parameters. These indicate which, if any, columns
*  contain sky coordinates.
                     IF ( NAME .EQ. 'ra_col' .OR.
     :                    NAME .EQ. 'RA_COL' ) THEN
                        CALL CHR_CTOI( VALUE, RACOL, STATUS )
                        RACOL = RACOL + 1
                     ELSE IF ( NAME .EQ. 'dec_col' .OR.
     :                         NAME .EQ. 'DEC_COL' ) THEN
                        CALL CHR_CTOI( VALUE, DECCOL, STATUS )
                        DECCOL = DECCOL + 1
                     END IF
                  END IF
               ELSE

*  Unknown line type. Should be the column line (immediately before the
*   '-' line ).
                  COLS = LINE
               END IF
            END IF
         ELSE

*  Read a '-' as the first character, or something horrible
*  happened.
            OK = .FALSE.
         END IF
 2       CONTINUE
         GO TO 1
      END IF

*  If an error occurred while scaning for parameters then exit.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Ok, should now be positioned at the first row of the catalogue
*  data. We need to process the first NLOOK lines and get some idea of
*  the data types, so first we need to know how many columns they
*  are. Do this by processing the column header line.
      ICOL = 0
      IAT = 1
      OK = .TRUE.
 3    CONTINUE
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL GAI1_NXTAB( COLS, IAT, IEND, STATUS )
         NDONE = NDONE + 1
         IF ( IEND .NE. 0 ) THEN
            ICOL = ICOL + 1
            IAT = IEND + 1

*  Set basic type to one we will not use.
            TYPES( ICOL ) = CAT__TYPEB

*  The size, in characters, of each column is 0.
            CSIZE( ICOL ) = 0
         ELSE

*  No more column names.
            OK = .FALSE.
         END IF
         GO TO 3
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Now read the next NLOOK lines and attempt to convert the types. Each
*  conversion is attempted in the order integer-double precision,
*  falling back to character if both of these fail. The length of each
*  "word" is also recorded as CSIZE for this type.
*
*  The special cases of RA and DEC columns (which should contain strings
*  colon separated fields, is also tested.
      OK = .TRUE.
      NDONE = 0
 5    CONTINUE
      IF ( OK .AND. STATUS .EQ. SAI__OK .AND. NDONE .LT. NLOOK ) THEN
         CALL FIO_READF( FI, LINE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Extract words on this line
            IAT = 1
            DO 4 I = 1, ICOL
               CALL GAI1_NXTAB( LINE, IAT, IEND, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN

*  Attempt conversion.
                  CALL ERR_MARK
                  CALL CHR_CTOI( LINE( IAT : IEND - 1 ), IVAL, STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN
                     CALL ERR_ANNUL( STATUS )
                     CALL CHR_CTOD( LINE( IAT : IEND - 1 ), DVAL,
     :                              STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_ANNUL( STATUS )

*  Is character or sexagesimal. Only check for sexagesimal if not
*  already assigned a type and the correct column and have a colon.
                        IF ( I .EQ. RACOL .OR. I .EQ. DECCOL .AND.
     :                       TYPES( I ) .EQ. CAT__TYPEB ) THEN

*  Sexagesimals must be "xx:xx:xx.xx" strings, so check for a colon. If
*  found then go for a full translation.
                           IVAL = INDEX( LINE( IAT : IEND - 1 ), ':' )
                           IF ( IVAL .EQ. 0 ) THEN
                              CSIZE( I ) = IEND - IAT
                              TYPES( I ) = CAT__TYPEC

*  Switch off further checking.
                              IF ( I .EQ. RACOL ) RACOL = 0
                              IF ( I .EQ. DECCOL ) DECCOL = 0
                              ISWCS = .FALSE.
                           ELSE

*  Attempt the translation. If fails then type is character.
                              CALL GAI1_S2ANG( LINE( IAT : IEND - 1 ),
     :                                         DVAL, FLAG, STATUS )
                              IF ( FLAG ) THEN
                                 ISWCS = .TRUE.
                              ELSE
                                 CSIZE( I ) = IEND - IAT
                                 TYPES( I ) = CAT__TYPEC

*  Switch off further checking.
                                 IF ( I .EQ. RACOL ) RACOL = 0
                                 IF ( I .EQ. DECCOL ) DECCOL = 0
                                 ISWCS = .FALSE.
                              END IF
                           END IF
                        ELSE
                           CSIZE( I ) = IEND - IAT
                           TYPES( I ) = CAT__TYPEC
                        END IF
                     ELSE

*  Is double precision. Only allow if no type is already set, or a
*  an integer type is already set.
                        IF ( TYPES( I ) .EQ. CAT__TYPEB .OR.
     :                       TYPES( I ) .EQ. CAT__TYPEI ) THEN
                           TYPES( I ) = CAT__TYPED
                        END IF
                     END IF
                  ELSE

*  Is integer. This is only allowed if type isn't set.
                     IF ( TYPES( I ) .EQ. CAT__TYPEB ) THEN
                        TYPES( I ) = CAT__TYPEI
                     END IF
                  END IF
                  CALL ERR_RLSE
               END IF
               IF ( IEND .NE. 0 ) THEN
                  IAT = IEND + 1
               END IF
 4          CONTINUE
         END IF
         NDONE = NDONE + 1
         GO TO 5
      END IF
      IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Rewind the file back to the start of the data.
      CALL FIO_RWIND( FI, STATUS )
      DO 6 I = 1, DLINE
         CALL FIO_READF( FI, LINE, STATUS )
 6    CONTINUE

*  If haven't found some RA and DEC columns (which must be characters in
*  form hh/dd:mm:ss.ss) then switch off any angle transformations.
      IF ( .NOT. ISWCS ) THEN
         RACOL = 0
         DECCOL = 0
      END IF

*  Create the columns.
      IAT = 1
      DO 7 I = 1, ICOL
         CALL GAI1_NXTAB( COLS, IAT, IEND, STATUS )

*  Check sexagesimal columns. These are stored as double precision
*  radians, but we need to set appropriate units.
         UNITS = ' '
         IF ( I .EQ. RACOL ) THEN
            UNITS = 'RADIANS{HOURS}'
            TYPES( I ) = CAT__TYPED
         ELSE IF ( I .EQ. DECCOL ) THEN
            UNITS = 'RADIANS{DEGREES}'
            TYPES( I ) = CAT__TYPED
         ELSE
            UNITS = ' '
         END IF
         CALL CAT_CNEWS( CI, COLS( IAT : IEND - 1 ), TYPES( I ),
     :                   CSIZE( I ), UNITS, EXTFMT( TYPES( I ) ), ' ',
     :                   COL( I ), STATUS )
         IAT = IEND + 1
 7    CONTINUE
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Now read the data for real and enter it into the catalogue.
      OK = .TRUE.
      NLINE = 1
 8    CONTINUE
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL FIO_READF( FI, LINE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop for all the expected values.
            IAT = 1
            DO 9 I = 1, ICOL
               CALL GAI1_NXTAB( LINE, IAT, IEND, STATUS )
               IF ( IEND .NE. 0 .AND. STATUS .EQ. SAI__OK ) THEN
                  IF ( I .EQ. RACOL .OR. I .EQ. DECCOL ) THEN

*  Right ascension or declination.  Convert this into radians.
                     CALL GAI1_S2ANG( LINE( IAT : IEND - 1 ), DVAL,
     :                                FLAG, STATUS )
                     IF ( I.EQ. RACOL ) DVAL = DVAL * 15.0 D0
                     CALL CAT_PUT0D( COL( I ), DVAL, .FALSE., STATUS )
                  ELSE
                     CALL CAT_PUT0C( COL( I ), LINE( IAT : IEND - 1 ),
     :                               .FALSE., STATUS )
                  END IF
                  IAT = IEND + 1
               ELSE

*  Missing field or [EOD]?
                  IF ( LINE( 1:5 ) .NE. '[EOD]' ) THEN
                     IF ( STATUS .EQ. SAI__OK ) THEN
                        STATUS = SAI__ERROR
                        CALL MSG_SETI( 'LINE', NLINE )
                        CALL ERR_REP( ' ',
     :                  'Data line no. ^LINE, contains too few fields',
     :                              STATUS )
                     END IF
                     GO TO 99
                  ELSE
                     OK = .FALSE.
                     GO TO 8
                  END IF
               END IF
               IF ( STATUS .NE. SAI__OK ) GO TO 8
 9          CONTINUE

*  Append this row to catalogue.
            CALL CAT_RAPND( CI, STATUS )
            NLINE = NLINE + 1
         END IF
         GO TO 8
      END IF
      IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  Exit in error label.
 99   CONTINUE
      END
