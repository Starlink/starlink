      SUBROUTINE GAI1_T2CN( FI, CI, STATUS )
*+
*  Name:
*     GAI!_T2CN

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

*  Authors:
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     28-SEP-1998 (PDRAPER):
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

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of a string

*  Local Parameters:
      INTEGER MAXLEN
      PARAMETER ( MAXLEN = 1024 )

*  Local Variables:
      BYTE BVAL                 ! Dummy value
      CHARACTER * ( 1 ) COMM    ! Column comments (lost)
      CHARACTER * ( 1 ) CVAL    ! Dummy value
      CHARACTER * ( CAT__SZCMP ) NAME ! Name of a component
      CHARACTER * ( CAT__SZEXP ) EXTFMT ! Column external format
      CHARACTER * ( CAT__SZUNI ) UNITS ! Units of column
      CHARACTER * ( CAT__SZVAL ) VALUE ! Value of a component
      CHARACTER * ( MAXLEN ) COLS ! Line buffer for column names
      CHARACTER * ( MAXLEN ) LINE ! Line buffer for output
      DOUBLE PRECISION DVAL     ! Dummy value
      DOUBLE PRECISION SCALE    ! Column scale factor
      DOUBLE PRECISION ZERO     ! Column zero point
      INTEGER COL( CAT__MXCOL ) ! Column identifiers
      INTEGER CSIZE             ! Characters in field
      INTEGER DIMS              ! Dimension of field (must be 0)
      INTEGER DTYPE             ! Column data type
      INTEGER I                 ! Loop variable
      INTEGER IVAL              ! Dummy value
      INTEGER NULL              ! NULL strategy
      INTEGER ORDER             ! Sorting order
      INTEGER QI                ! Parameter identifier
      INTEGER SIZEA( 1 )        ! Size of dimension (must be 0)
      INTEGER*2 WVAL            ! Dummy value
      LOGICAL PRFDSP            ! Preferential display flag
      REAL RVAL                 ! Dummy value
      LOGICAL OK                ! Loop control flag
      INTEGER IAT               ! Position within string
      INTEGER ICOL              ! Number of columns
      INTEGER IEND              ! End character of word

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

*  Now read the input file until we get to a line that starts with '-'.
      OK = .TRUE.
 1    CONTINUE
      IF ( OK ) THEN
         CALL FIO_READF( FI, LINE, STATUS )
         IF ( LINE( 1: 1 ) .NE. '-' .AND. STATUS .EQ. SAI__OK ) THEN

*  Check for '#P' or '#C', which indicate hidden information.
            IF ( LINE( 1: 2 ) .EQ. '#P' ) THEN

*  Hidden parameter statement, read back the details and create the
*  parameter.
               READ( LINE, 101 ) NAME, DTYPE, CSIZE, UNITS, EXTFMT,
     :                           PRFDSP
 101           FORMAT( '#P', A17, I3, I5, A22, A22, L2 )
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
               READ( LINE, 102 ) NAME, DTYPE, CSIZE, NULL,
     :                           SCALE, ZERO, ORDER, UNITS, EXTFMT,
     :                           PRFDSP
 102           FORMAT( '#C', A17, I3, I5, I2, G15.7, G15.7, I2, A22,
     :                 A22, L2 ) 
               CALL CAT_CNEWA( CI, NAME, ' ', DTYPE, CSIZE, 
     :                         CAT__SCALR, 1, NULL, ' ', SCALE, ZERO, 
     :                         ORDER, UNITS, EXTFMT, PRFDSP, ' ', 
     :                         QI, STATUS )

*  Skip to next loop.
               GO TO 2
            END IF

*  Skip any other comment-lines.
            IF ( LINE( 1 : 1 ) .EQ. '#' ) GO TO 2

*  Arrive here, if a hidden column or parameter has not been
*  found. These lines should now be valid, existing parameters
*  that only need a correct value assigning.
            IAT = INDEX( LINE, ':' )
            IF ( IAT .NE . 0 ) THEN 

*  Get the parameter name and value.
               NAME = LINE( : IAT - 1 )
               VALUE = LINE( IAT + 1 : )

*  Now see if the parameter exists (as it normally should). 
               CALL ERR_MARK
               QI = CAT__NOID
               CALL CAT_TIDNT( CI, NAME, QI, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN 
                  CALL ERR_ANNUL( STATUS )

*  Create the parameter anyway. This will just have character format.
                  CALL CAT_PPTSC( CI, NAME, VALUE, ' ', QI, STATUS )
               ELSE

*  Parameter exists, just set the value (rely on internal conversion).
                  CALL CAT_TATTC( QI, 'VALUE', VALUE, STATUS )
               END IF
               CALL ERR_RLSE
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

*  Now we can start to read in the data.
      CALL ERR_MARK
      OK = .TRUE.
 4    CONTINUE
      IF ( OK .AND. STATUS .EQ. SAI__OK ) THEN 
         CALL FIO_READF( FI, LINE, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN 

*  Loop for all the expected values.
            IAT = 1
            DO 5 I = 1, ICOL
               CALL GAI1_NXTAB( LINE, IAT, IEND, STATUS )
               IF ( IEND .NE. 0 ) THEN
                  CALL CAT_PUT0C( COL( I ), LINE( IAT : IEND - 1 ), 
     :                            .FALSE., STATUS )
                  IAT = IEND + 1
               ELSE

*  Missing field
                  STATUS = SAI__ERROR
                  CALL MSG_SETI( 'LINE', I )
                  CALL ERR_REP( ' ', 
     :            'Data line no. ^LINE, contains too few fields', 
     :                          STATUS )
                  GO TO 99
               END IF
               IF ( STATUS .NE. SAI__OK ) GO TO 4
 5          CONTINUE

*  Append this row to catalogue.
            CALL CAT_RAPND( CI, STATUS )
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
