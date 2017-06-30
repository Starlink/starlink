      SUBROUTINE POL1_CPTAB( CIIN, CIOUT, NUMCOL, FIIN, FIOUT, STATUS )
*+
*  Name:
*     POL1_CPTAB

*  Purpose:
*     Copy a table from an input to an output catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPTAB( CIIN, CIOUT, NUMCOL, FIIN, FIOUT, STATUS )

*  Description:
*     Copy the table of an input catalogue to an output catalogue.

*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier to the input catalogue.  Note that this identifier
*        may be a catalogue or index or a selection.
*     CIOUT  =  INTEGER (Given)
*        Identifier to the output catalogue.
*     NUMCOL  =  INTEGER (Given)
*        Number of columns in the input (and hence output) catalogue.
*     FIIN(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns in the input catalogue.
*     FIOUT(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns in the output catalogue.
*     STATUS  =  INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2017 East Asian Observatory.

*  Authors:
*     DSB: David S. Berry (EAO)
*     ACD: A C Davenhall (Edinburgh)
*     {enter_new_authors_here}

*  History:
*     29-JUN-2017 (DSB):
*        Original version, based on cap_cptab.f by ACD.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing



*  Global Constants:
      INCLUDE 'SAE_PAR'     ! Standard SAE symbolic constants
      INCLUDE 'CAT_PAR'     ! CAT symbolic constants
      INCLUDE 'CAT_ERR'     ! CAT error codes

*  Arguments Given:
      INTEGER CIIN
      INTEGER CIOUT
      INTEGER NUMCOL
      INTEGER FIIN(NUMCOL)
      INTEGER FIOUT(NUMCOL)

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      INTEGER
     :  FIINC,   ! Identifier to current input  column.
     :  FIOUTC,  !     "      "     "    output   "   .
     :  IDTYPE,  ! Type of the identifier to current input column.
     :  CURCOL,  ! Number of the current column.
     :  NUMID,   ! Total number of identifiers (inc. vector elements).
     :  CURID,   ! Current identifier (inc. vector elements).
     :  FIINI(CAT__MXCOL),  ! List of input  identifiers (inc. vectors).
     :  FIOUTI(CAT__MXCOL), !  "   "  output      "      ( " .    "   ).
     :  FDTYPI(CAT__MXCOL), ! Data types corresponding to identifiers.
     :  LFNAME   ! Length of FNAME (excl. trail. blanks).

      INTEGER
     :  DTYPE,  ! Data type of current column.
     :  DIMS,   ! Dimensionality of the current column.
     :  SIZE,   ! Size (number of elements) of the current column.
     :  ELEM,   ! Current vector element.
     :  ENMPOS, ! Current position in ENAME.
     :  FIINE,  ! Identifier to current input  scalar or vector element.
     :  FIOUTE, !     "      "     "    output   "    "    "       "   .
     :  ROWS,   ! Number of rows in the input catalogue.
     :  ROW     ! Current row.

      INTEGER
     :  CIINT,  ! Type of input catalogue: catalogue or index.
     :  CIINB   ! Identifier for base input catalogue.

      CHARACTER
     :  FNAME*(CAT__SZCMP),  ! Name of the current column.
     :  ENAME*(CAT__SZCMP)   ! Name of the current vector element.

      LOGICAL
     :  NULFLG  ! Flag; is the current field null?

*  The following values hold the value read (and written) for the current
*  field.  There are values for each of the data types supported by SCAR/ADC.
      BYTE             VALUEB
      INTEGER*2        VALUEW
      INTEGER          VALUEI
      REAL             VALUER
      DOUBLE PRECISION VALUED
      LOGICAL          VALUEL
      CHARACTER        VALUEC*(CAT__SZVAL)
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

      NUMID = 0

*  Determine the data type and dimensionality of every column in the input
*  catalogue.  If the column is a vector then get identifiers for all the
*  individual elements.
      DO CURCOL = 1, NUMCOL
         FIINC = FIIN(CURCOL)
         FIOUTC = FIOUT(CURCOL)

*  Get the data type and dimensionality of the column.  Note that expressions
*  are scalars with a data type of DOUBLE PRECISION.
         CALL CAT_TIDTP( FIINC, IDTYPE, STATUS )

         IF( IDTYPE .NE. CAT__EITYP) THEN
            CALL CAT_TIQAI( FIINC, 'DTYPE', DTYPE, STATUS )
            CALL CAT_TIQAI( FIINC, 'DIMS', DIMS, STATUS )
         ELSE
            DTYPE = CAT__TYPED
            DIMS = CAT__SCALR
         END IF

*  Check whether the column is a scalar or a vector.
         IF( DIMS .EQ. CAT__SCALR ) THEN

*  Add the identifier to the list of columns to copy.
            IF( STATUS .EQ. SAI__OK ) THEN
               IF( NUMID .LT. CAT__MXCOL ) THEN
                  NUMID = NUMID + 1

                  FIINI(NUMID) = FIINC
                  FIOUTI(NUMID) = FIOUTC
                  FDTYPI(NUMID) = DTYPE
               END IF
            END IF
         ELSE

*  The column is a vector; determine the number of elements and its name,
*  then get an identifier for each element.
            CALL CAT_TIQAI( FIINC, 'SIZE', SIZE, STATUS )
            CALL CAT_TIQAC( FIINC, 'NAME', FNAME, STATUS )

            DO ELEM = 1, SIZE
               ENAME = ' '
               ENMPOS = 0

               IF( FNAME .NE. ' ') THEN
                  LFNAME = CHR_LEN(FNAME)
               ELSE
                  LFNAME = 1
               END IF

               CALL CHR_PUTC( FNAME(1 : LFNAME), ENAME, ENMPOS)
               CALL CHR_PUTC( '[', ENAME, ENMPOS)
               CALL CHR_PUTI( ELEM, ENAME, ENMPOS)
               CALL CHR_PUTC( ']', ENAME, ENMPOS)

               CALL CAT_TIDTP( CIIN, CIINT, STATUS )

               IF( CIINT .EQ. CAT__IITYP  .OR.
     :             CIINT .EQ. CAT__SITYP) THEN
                  CALL CAT_TIDPR( CIIN, CIINB, STATUS )
               ELSE
                  CIINB = CIIN
               END IF

               CALL CAT_TIDNT( CIINB, ENAME, FIINE, STATUS )
               CALL CAT_TIDNT( CIOUT, ENAME, FIOUTE, STATUS )

               IF( STATUS .EQ. SAI__OK ) THEN
                  IF( NUMID .LT. CAT__MXCOL) THEN
                     NUMID = NUMID + 1

                     FIINI(NUMID) = FIINE
                     FIOUTI(NUMID) = FIOUTE
                     FDTYPI(NUMID) = DTYPE
                  END IF
               END IF
            END DO

         END IF
      END DO

*  Determine the number of rows in the input catalogue.
      CALL CAT_TROWS( CIIN, ROWS, STATUS )

*  For every row in the input catalogue copy all the columns.
      DO ROW = 1, ROWS

*  Read the next row from the input catalogue.
         CALL CAT_RGET( CIIN, ROW, STATUS )

         DO CURID = 1, NUMID

*  Check the data type of the column, and for the appropriate data type:
*  Get the value for the current field from the input catalogue. Put the
*  value to the current field in the output catalogue.
            IF( FDTYPI(CURID) .EQ. CAT__TYPEB ) THEN
               CALL CAT_EGT0B( FIINI(CURID), VALUEB, NULFLG, STATUS )
               CALL CAT_PUT0B( FIOUTI(CURID), VALUEB, NULFLG, STATUS )

            ELSE IF( FDTYPI(CURID) .EQ. CAT__TYPEW ) THEN
               CALL CAT_EGT0W( FIINI(CURID), VALUEW, NULFLG, STATUS )
               CALL CAT_PUT0W( FIOUTI(CURID), VALUEW, NULFLG, STATUS )

            ELSE IF( FDTYPI(CURID) .EQ. CAT__TYPEI ) THEN
               CALL CAT_EGT0I( FIINI(CURID), VALUEI, NULFLG, STATUS )
               CALL CAT_PUT0I( FIOUTI(CURID), VALUEI, NULFLG, STATUS )

            ELSE IF( FDTYPI(CURID) .EQ. CAT__TYPER ) THEN
               CALL CAT_EGT0R( FIINI(CURID), VALUER, NULFLG, STATUS )
               CALL CAT_PUT0R( FIOUTI(CURID), VALUER, NULFLG, STATUS )

            ELSE IF( FDTYPI(CURID) .EQ. CAT__TYPED ) THEN
               CALL CAT_EGT0D( FIINI(CURID), VALUED, NULFLG, STATUS )
               CALL CAT_PUT0D( FIOUTI(CURID), VALUED, NULFLG, STATUS )

            ELSE IF( FDTYPI(CURID) .EQ. CAT__TYPEL ) THEN
               CALL CAT_EGT0L( FIINI(CURID), VALUEL, NULFLG, STATUS )
               CALL CAT_PUT0L( FIOUTI(CURID), VALUEL, NULFLG, STATUS )

            ELSE IF( FDTYPI(CURID) .EQ. CAT__TYPEC ) THEN
               CALL CAT_EGT0C( FIINI(CURID), VALUEC, NULFLG, STATUS )
               CALL CAT_PUT0C( FIOUTI(CURID), VALUEC, NULFLG, STATUS )

            ELSE
               STATUS = CAT__INVDT

            END IF
         END DO

*  Append the current row to the output catalogue.
         CALL CAT_RAPND( CIOUT, STATUS )
      END DO

*  If the status is not ok then report an error.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'Error copying catalogue table.', STATUS )
      END IF

      END
