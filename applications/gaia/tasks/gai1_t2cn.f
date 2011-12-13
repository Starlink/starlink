      SUBROUTINE GAI1_T2CN( FI, CI, STATUS )
*+
*  Name:
*     GAI1_T2CN

*  Purpose:
*     Convert a GAIA native tab table into a CAT catalogue.

*  Description:
*     This routine reads a file attached to a given FIO identifier and
*     attempts to convert it into a CAT catalogue, under the assumption
*     that the attached file contains a "tab table". This tab table must
*     have been written by the related "cat2tab" application, or follow
*     the convention about storing parameter and column information that
*     cat2tab uses (i.e. a description of all the parameters and columns
*     must be written in hidden statements and the title must be "cat2tab").
*
*     One special case is the SYMBOL parameter. This may be a long
*     string and will span the SYMBOL[1-9] names (FITS truncates to 68
*     characters).

*  Language:
*     Fortran-77

*  Invocation:
*     CALL GAI_T2CN( FI, CI, STATUS )

*  Description:

*  Arguments:
*     FI = INTEGER (Given)
*        FIO identifier of the catalogue to be converted.
*     CI = INTEGER (Given)
*        Fortran unit number of output file.
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
*     28-SEP-1998 (PWD):
*        Original version.
*     07-JUN-1999 (PWD):
*        Added changes to support long symbols.
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

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of a string

*  Local Parameters:
      INTEGER MAXLEN
      PARAMETER ( MAXLEN = 2048 )
      DOUBLE PRECISION PI       ! PI
      PARAMETER ( PI =  3.141592653589793238462643 )

*  Local Variables:
      BYTE BVAL                 ! Dummy value
      CHARACTER * ( 1 ) COMM    ! Column comments (lost)
      CHARACTER * ( 1 ) CVAL    ! Dummy value
      CHARACTER * ( 2 ) SKIP    ! Dummy value
      CHARACTER * ( CAT__SZCMP ) NAME ! Name of RA and DEC columns
      CHARACTER * ( CAT__SZEXP ) EXTFMT ! Column external format
      CHARACTER * ( CAT__SZUNI ) UNITS ! Units of column
      CHARACTER * ( MAXLEN ) COLS ! Line buffer for column names
      CHARACTER * ( MAXLEN ) LINE ! Line buffer for output
      CHARACTER * ( MAXLEN ) VALUE ! Value of a component
      DOUBLE PRECISION DVAL     ! Dummy value
      DOUBLE PRECISION SCALE    ! Column scale factor
      DOUBLE PRECISION ZERO     ! Column zero point
      INTEGER COL( CAT__MXCOL ) ! Column identifiers
      INTEGER CSIZE             ! Characters in field
      INTEGER DECCOL            ! DEC column
      INTEGER DIMS              ! Dimension of field (must be 0)
      INTEGER DTYPE             ! Column data type
      INTEGER I                 ! Loop variable
      INTEGER IAT               ! Position within string
      INTEGER ICOL              ! Number of columns
      INTEGER IEND              ! End character of word
      INTEGER IVAL              ! Dummy value
      INTEGER NLINE             ! Number of line read
      INTEGER NULL              ! NULL strategy
      INTEGER ORDER             ! Sorting order
      INTEGER QI                ! Parameter identifier
      INTEGER RACOL             ! RA column
      INTEGER SIZEA( 1 )        ! Size of dimension (must be 0)
      INTEGER VALLEN            ! VALUE length
      INTEGER*2 WVAL            ! Dummy value
      LOGICAL CONV              ! Converted
      LOGICAL DECRAD            ! Need DEC converting to RADIANS
      LOGICAL DECSEX            ! Column is sexagesimal
      LOGICAL OK                ! Loop control flag
      LOGICAL PRFDSP            ! Preferential display flag
      LOGICAL RARAD             ! Need RA converting to RADIANS
      LOGICAL RASEX             ! Column is sexagesimal
      REAL RVAL                 ! Dummy value
*.

*  Check the global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise dummy parameters.
      BVAL = 0
      CVAL = ' '
      DVAL = 0.0D0
      IVAL = 0
      RVAL = 0.0
      WVAL = 0
      RACOL = -1
      DECCOL = -1
      DECRAD = .FALSE.
      RARAD = .FALSE.

*  Now read the input file until we get to a line that starts with '-'.
      OK = .TRUE.
 1    CONTINUE
      IF ( OK ) THEN
         CALL FIO_READF( FI, LINE, STATUS )
         IF ( LINE( 1: 1 ) .NE. '-' .AND. STATUS .EQ. SAI__OK ) THEN

*  Check for '#P', '#C' ot '#T', which indicate hidden information.
            IF ( LINE( 1: 2 ) .EQ. '#P' ) THEN

*  Hidden parameter statement, read back the details and create the
*  parameter.
               READ( LINE, 101 ) SKIP, NAME, DTYPE, CSIZE, UNITS,
     :                           EXTFMT, PRFDSP
 101           FORMAT( A2, A17, I3, I5, A22, A22, L2 )
               IF ( DTYPE .EQ. CAT__TYPEUB .OR. DTYPE .EQ. CAT__TYPEB )
     :         THEN
                  CALL CAT_PPTAB( CI, NAME, CSIZE, DIMS, SIZEA,
     :                            UNITS, EXTFMT, PRFDSP, COMM,
     :                            BVAL, QI, STATUS )
               ELSE IF ( DTYPE .EQ. CAT__TYPEUW .OR.
     :                  DTYPE .EQ. CAT__TYPEUW ) THEN
                  CALL CAT_PPTAW( CI, NAME, CSIZE, DIMS, SIZEA,
     :                            UNITS, EXTFMT, PRFDSP, COMM,
     :                            WVAL, QI, STATUS )
               ELSE IF ( DTYPE .EQ. CAT__TYPER ) THEN
                  CALL CAT_PPTAR( CI, NAME, CSIZE, DIMS, SIZEA,
     :                            UNITS, EXTFMT, PRFDSP, COMM,
     :                            RVAL, QI, STATUS )
               ELSE IF ( DTYPE .EQ. CAT__TYPED ) THEN
                  CALL CAT_PPTAD( CI, NAME, CSIZE, DIMS, SIZEA,
     :                            UNITS, EXTFMT, PRFDSP, COMM,
     :                            DVAL, QI, STATUS )
               ELSE IF ( DTYPE .EQ. CAT__TYPEI ) THEN
                  CALL CAT_PPTAI( CI, NAME, CSIZE, DIMS, SIZEA,
     :                            UNITS, EXTFMT, PRFDSP, COMM,
     :                            IVAL, QI, STATUS )
               ELSE
                  CALL CAT_PPTAC( CI, NAME, CSIZE, DIMS, SIZEA,
     :                            UNITS, EXTFMT, PRFDSP, COMM,
     :                            CVAL, QI, STATUS )
               END IF

*  Skip to next loop.
               GO TO 2

*  Re-create a column, if hidden one is recorded.
            ELSE IF ( LINE ( 1 : 2 ) .EQ. '#C' ) THEN
               READ( LINE, 102 ) SKIP, NAME, DTYPE, CSIZE, NULL,
     :                           SCALE, ZERO, ORDER, UNITS, EXTFMT,
     :                           PRFDSP
 102           FORMAT( A2, A17, I3, I5, I2, G15.7, G15.7, I2, A22,
     :                 A22, L2 )
               CALL CAT_CNEWA( CI, NAME, ' ', DTYPE, CSIZE,
     :                         CAT__SCALR, 1, NULL, ' ', SCALE, ZERO,
     :                         ORDER, UNITS, EXTFMT, PRFDSP, ' ',
     :                         QI, STATUS )

*  Skip to next loop.
               GO TO 2

*  Write a textual line if found.
            ELSE IF ( LINE ( 1 : 2 ) .EQ. '#T' ) THEN
               CALL CAT_PUTXT( CI, 'COMMENT',
     :                         LINE( 3 : CHR_LEN( LINE ) ), STATUS )

*  Skip to next loop.
               GO TO 2
            END IF

*  Skip any other comment-lines.
            IF ( LINE( 1 : 1 ) .EQ. '#' ) GO TO 2

*  Arrive here, if a hidden column or parameter has not been
*  found. These lines should, in general, now be valid, existing
*  parameters that only need a correct value assigning.
            IAT = INDEX( LINE, ':' )
            IF ( IAT .NE . 0 ) THEN

*  Get the parameter name and value.
               NAME = LINE( : IAT - 1 )
               VALUE = LINE( IAT + 1 : )

*  Treat SYMBOL differently to others.
               IF ( NAME( : 6 ) .EQ. 'symbol' ) THEN

*  Get length of symbol string and split into 68 character parts.
                  VALLEN = CHR_LEN( VALUE )
                  I = 1
                  DO 6 IAT = 1, VALLEN, 68
                     WRITE( NAME( 7:7 ), '(I1)' ) I
                     CALL CAT_PPTSC( CI, NAME, VALUE( IAT:IAT+68 ), ' ',
     :                               QI, STATUS )
                     CALL CAT_TATTI( QI, 'CSIZE', 68, STATUS )
                     I = I + 1
 6                CONTINUE
               ELSE

*  See if this is a positional parameter which we should skip.
                  IF ( NAME .NE. 'id_col' .AND. NAME .NE. 'ra_col' .AND.
     :                 NAME .NE. 'dec_col' ) THEN

*  Now see if the parameter exists (as stored ones should).
                     CALL ERR_MARK
                     QI = CAT__NOID
                     CALL CAT_TIDNT( CI, NAME, QI, STATUS )
                     IF ( STATUS .NE. SAI__OK ) THEN
                        CALL ERR_ANNUL( STATUS )

*  Create the parameter anyway. This will just have character format.
                        CALL CAT_PPTSC( CI, NAME, VALUE, ' ', QI,
     :                                  STATUS )
                     ELSE

*  Parameter exists, just set the value (rely on internal conversion).
                        CALL ERR_MARK
                        CALL CAT_TATTC( QI, 'VALUE', VALUE, STATUS )
                        IF ( STATUS .NE. SAI__OK )
     :                       CALL ERR_ANNUL( STATUS )
                        CALL ERR_RLSE
                     END IF
                     CALL ERR_RLSE
                  ELSE

*  If this ra_col or dec_col then record for format conversion.
                     IF ( NAME .EQ. 'ra_col' ) THEN
                        CALL CHR_CTOI( VALUE, RACOL, STATUS )
                        RACOL = RACOL + 1
                     ELSE IF ( NAME .EQ. 'dec_col' ) THEN
                        CALL CHR_CTOI( VALUE, DECCOL, STATUS )
                        DECCOL = DECCOL + 1
                     END IF
                  END IF
               END IF
            ELSE

*  Unknown line type. Should be the column line.
               COLS = LINE
            END IF
         ELSE

*  Read a '-' as the first character, or something horrible
*  happened. If we're at
            OK = .FALSE.
         END IF
 2       CONTINUE
         GO TO 1
      END IF

*  If an error occurred while scaning for parameters then exit.
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Ok, should now be positioned at the first row of the catalogue
*  data. Need to check for the column names, which should be stored in
*  COLS. To process this we get each <TAB> separated word and access the
*  related identifier. This serves as a test on the catalogue as all
*  columns should exist.
      IAT = 1
      OK = .TRUE.
      ICOL = 0

 3    CONTINUE
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
         CALL GAI1_NXTAB( COLS, IAT, IEND, STATUS )
         IF ( IEND .NE. 0 ) THEN

*  Check column name and get identifier.
            CALL CAT_TIDNT( CI, COLS( IAT : IEND - 1 ), QI, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               ICOL = ICOL + 1
               COL( ICOL ) = QI
               IAT = IEND + 1
            END IF
         ELSE

*  No more column names.
            OK = .FALSE.
         END IF
         GO TO 3
      END IF
      IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Deal with possible degrees to radian conversion and sexagesimal
*  formats.
      CALL FIO_READF( FI, LINE, STATUS )
      IF ( RACOL .NE. -1 .AND. DECCOL .NE. -1 ) THEN

*  Extract the RA and DEC strings.
         DO 7 I = 1, ICOL
            CALL GAI1_NXTAB( LINE, IAT, IEND, STATUS )
            IF ( I .EQ. RACOL ) THEN

*  See if this is sexagesimal
               CALL GAI1_S2ANG( LINE( IAT : IEND - 1 ), DVAL, RASEX,
     :                          STATUS )

*  See if units are RADIANS, in which case we need to convert.
               CALL CAT_TIQAC( COL( I ), 'UNITS', UNITS, STATUS )
               IF ( UNITS( :7 ) .EQ. 'RADIANS' ) THEN
                  RARAD = .TRUE.
               ELSE
                  RARAD = .FALSE.
               END IF
            ELSE IF ( I .EQ. DECCOL ) THEN
               CALL GAI1_S2ANG( LINE( IAT : IEND - 1 ), DVAL, DECSEX,
     :                          STATUS )
               IF ( UNITS( :7 ) .EQ. 'RADIANS' ) THEN
                  DECRAD = .TRUE.
               ELSE
                  DECRAD = .FALSE.
               END IF
            END IF
 7       CONTINUE
      ELSE

*  Reset in case only one is found.
         RACOL = -1
         DECCOL = -1
      END IF

*  Now we can start to read in the data.
      CALL ERR_MARK
      OK = .TRUE.
      NLINE = 1
 4    CONTINUE
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN
         IF ( STATUS .EQ. SAI__OK ) THEN

*  Loop reading all data lines (the first of which has already been
*  read), adding these to the catalogue and converting any RA/DEC
*  columns to radians.
            IAT = 1
            DO 5 I = 1, ICOL
               CALL GAI1_NXTAB( LINE, IAT, IEND, STATUS )
               IF ( IEND .NE. 0 ) THEN
                  IF ( I .EQ. RACOL ) THEN

*  RA column, convert from sexigesimal or degrees.
                     IF ( RASEX ) THEN
                        CALL GAI1_S2ANG( LINE( IAT : IEND - 1 ), DVAL,
     :                                   CONV, STATUS )
                     ELSE
                        CALL CHR_CTOD( LINE( IAT : IEND - 1 ), DVAL ,
     :                                 STATUS )
                        IF ( RARAD ) DVAL = DVAL * PI / 180.0D0
                     END IF
                     CALL CAT_PUT0D( COL( I ), DVAL, .FALSE., STATUS )
                  ELSE IF ( I .EQ. DECCOL ) THEN

*  DEC column, convert from sexigesimal or degrees.
                     IF ( DECSEX ) THEN
                        CALL GAI1_S2ANG( LINE( IAT : IEND - 1 ), DVAL,
     :                                   CONV, STATUS )
                     ELSE
                        CALL CHR_CTOD( LINE( IAT : IEND - 1 ), DVAL ,
     :                                 STATUS )
                        IF ( DECRAD ) DVAL = DVAL * PI / 180.0D0
                     END IF
                     CALL CAT_PUT0D( COL( I ), DVAL, .FALSE., STATUS )
                  ELSE

*  ordinary value, just add as a character.
                     CALL CAT_PUT0C( COL( I ), LINE( IAT : IEND - 1 ),
     :                            .FALSE., STATUS )
                  END IF
                  IAT = IEND + 1
               ELSE

*  Missing field, or [EOD]?
                  IF ( LINE( 1:5 ) .NE. '[EOD]' ) THEN

                     STATUS = SAI__ERROR
                     CALL MSG_SETI( 'LINE', NLINE )
                     CALL ERR_REP( ' ',
     :               'Data line no. ^LINE, contains too few fields',
     :                           STATUS )
                     GO TO 99
                  ELSE
                     OK = .FALSE.
                     GO TO 4
                  END IF
               END IF
               IF ( STATUS .NE. SAI__OK ) GO TO 4
 5          CONTINUE

*  Append this row to catalogue.
            CALL CAT_RAPND( CI, STATUS )
            NLINE = NLINE + 1

*  Read next line.
            CALL FIO_READF( FI, LINE, STATUS )
         END IF
         GO TO 4
      END IF
      IF ( STATUS .EQ. FIO__EOF ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Exit in error label.
 99   CONTINUE
      END
