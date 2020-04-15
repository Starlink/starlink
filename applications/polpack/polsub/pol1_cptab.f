      SUBROUTINE POL1_CPTAB( CIIN, CIOUT, NUMCOL, FIIN, FIOUT, TWCS,
     :                       STATUS )
*+
*  Name:
*     POL1_CPTAB

*  Purpose:
*     Append an input table to the end of an output catalogue.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_CPTAB( CIIN, CIOUT, NUMCOL, FIIN, FIOUT, TWCS, STATUS )

*  Description:
*     Append the table of an input catalogue to the end of an output
*     catalogue.

*  Arguments:
*     CIIN  =  INTEGER (Given)
*        Identifier to the input catalogue.  Note that this identifier
*        may be a catalogue or index or a selection.
*     CIOUT  =  INTEGER (Given)
*        Identifier to the output catalogue.
*     NUMCOL  =  INTEGER (Given)
*        Number of columns to copy. If this is zero, then the number of
*        columns copied is the minimum of the number of columns in the
*        input and output catalogue.
*     FIIN(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns in the input catalogue. If NUMCOL is
*        zero, FIIN is ignored and the column identifiers are obtained
*        directly from CIIN.
*     FIOUT(NUMCOL)  =  INTEGER (Given)
*        Identifiers for the columns in the output catalogue. If NUMCOL is
*        zero, FIOUT is ignored and the column identifiers are obtained
*        directly from CIOUT.
*     TWCS = INTEGER (Given)
*        An identifier for an AST FrameSet, or AST__NULL. If not AST__NULL,
*        and if the input catalogue contains columns named "X" and "Y",
*        and if the input catalogue contains a WCS FrameSet, the input
*        X and Y values will be transformed so that they refer to
*        equivalent sky positions within the PIXEL frame in the supplied
*        TWCS FrameSet.
*
*        In addition, if TWCS is not AST__NULL, and if the input catalogue
*        contains columns named "Q", "U" and "ANG", and if the input catalogue
*        contains a WCS FrameSet, the input Q, U and ANG values (together
*        with their errors) will be transformed so that they refer to the
*        reference direction specified by the POLANAL Frame in the TWCS
*        FrameSet.
*
*        If AST__NULL is supplied for TWCS, all values are copied from
*        input to output without any change.
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
*     28-SEP-2017 (DSB):
*        Added argument TWCS.
*     14-APR-2020 (DSB):
*        Allow NUMCOL to be zero.
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
      INCLUDE 'CNF_PAR'     ! For CNF_PVAL function
      INCLUDE 'AST_PAR'     ! AST__ constants
      INCLUDE 'PRM_PAR'     ! VAL__ constants

*  Arguments Given:
      INTEGER CIIN
      INTEGER CIOUT
      INTEGER NUMCOL
      INTEGER FIIN(NUMCOL)
      INTEGER FIOUT(NUMCOL)
      INTEGER TWCS

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
     :  IPT,    ! Pointer to table of modified polpack values
     :  NCIN,   ! Number of columns in input
     :  NCOUT,  ! Number of columns in output
     :  NUMUSE, ! Number of columns to copy
     :  ROWS,   ! Number of rows in the input catalogue.
     :  ROW,    ! Current row.
     :  TROW    ! Index of row in IPT holding modified column values

      INTEGER
     :  CIINT,  ! Type of input catalogue: catalogue or index.
     :  CIINB   ! Identifier for base input catalogue.

      INTEGER
     :  ANGID,  ! The identifier for the ANG column
     :  DANGID, ! The identifier for the DANG column
     :  DQID,   ! The identifier for the DQ column
     :  DUID,   ! The identifier for the DU column
     :  QID,    ! The identifier for the Q column
     :  UID,    ! The identifier for the U column
     :  XID,    ! The identifier for the X column
     :  YID     ! The identifier for the Y column

      CHARACTER
     :  FNAME*(CAT__SZCMP),  ! Name of the current column.
     :  ENAME*(CAT__SZCMP)   ! Name of the current vector element.

      LOGICAL
     :  GETFI,  ! Flag; get the column identifiers here?
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

*  Initialise the identifiers for selected POLPACK columns to indicate that
*  the columns have not yet been found.
      XID = CAT__NOID
      YID = CAT__NOID
      QID = CAT__NOID
      UID = CAT__NOID
      ANGID = CAT__NOID
      DQID = CAT__NOID
      DUID = CAT__NOID
      DANGID = CAT__NOID

*  If NUMCOL is supplied as zero, find the minimum of the number of
*  columns in input and output.
      IF( NUMCOL .EQ. 0 ) THEN
         GETFI = .TRUE.
         CALL CAT_TCOLS( CIIN, CAT__GPHYS, NCIN, STATUS )
         CALL CAT_TCOLS( CIOUT, CAT__GPHYS, NCOUT, STATUS )
         NUMUSE = MIN( NCIN, NCOUT )
      ELSE
         GETFI = .FALSE.
         NUMUSE = NUMCOL
      END IF

*  Determine the data type and dimensionality of every column in the input
*  catalogue.  If the column is a vector then get identifiers for all the
*  individual elements.
      NUMID = 0
      DO CURCOL = 1, NUMUSE

*  Get the column identifiers. Either get them directly from the
*  catalogues, or use the values supplied in the FIIN and FIOUT arrays.
         IF( GETFI ) THEN
            CALL CAT_TNDNT( CIIN, CAT__FITYP, CURCOL, FIINC, STATUS )
            CALL CAT_TNDNT( CIOUT, CAT__FITYP, CURCOL, FIOUTC, STATUS )
         ELSE
            FIINC = FIIN(CURCOL)
            FIOUTC = FIOUT(CURCOL)
         END IF

*  Get the column name.
         CALL CAT_TIQAC( FIINC, 'NAME', FNAME, STATUS )

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

*  Record the indices of the required scalar POLPACK columns.
                  IF( FNAME .EQ. 'X' ) THEN
                     XID = FIINC
                  ELSE IF( FNAME .EQ. 'Y' ) THEN
                     YID = FIINC
                  ELSE IF( FNAME .EQ. 'Q' ) THEN
                     QID = FIINC
                  ELSE IF( FNAME .EQ. 'U' ) THEN
                     UID = FIINC
                  ELSE IF( FNAME .EQ. 'ANG' ) THEN
                     ANGID = FIINC
                  ELSE IF( FNAME .EQ. 'DQ' ) THEN
                     DQID = FIINC
                  ELSE IF( FNAME .EQ. 'DU' ) THEN
                     DUID = FIINC
                  ELSE IF( FNAME .EQ. 'DANG' ) THEN
                     DANGID = FIINC
                  END IF
               END IF
            END IF
         ELSE

*  The column is a vector; determine the number of elements, then get
*  an identifier for each element.
            CALL CAT_TIQAI( FIINC, 'SIZE', SIZE, STATUS )

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

*  If a FrameSet has been supplied (TWCS) we want to modify the input
*  values so that they refer to the pixel coordinate system and
*  reference direction specified by the FrameSet. The modified values are
*  returned in a dynamically allocated 2D array of _REALs in which each
*  column corresponds to a row of the input catalogue, and each row
*  corresponds to one of the required polpack columns, in the order
*  X,Y,Q,DQ,U,DU,ANG,DANG
      IPT = 0
      IF( TWCS .NE. AST__NULL ) THEN
         IF( XID .NE. CAT__NOID .AND.
     :       YID .NE. CAT__NOID .AND.
     :       QID .NE. CAT__NOID .AND.
     :       UID .NE. CAT__NOID .AND.
     :       ANGID .NE. CAT__NOID ) THEN

            CALL PSX_CALLOC( 8*ROWS, '_REAL', IPT, STATUS )
            CALL POL1_CPMOD( CIIN, TWCS, XID, YID, QID, UID,
     :                       ANGID, DQID, DUID, DANGID, ROWS,
     :                       %VAL( CNF_PVAL(IPT) ), STATUS )

         END IF
      END IF

*  For every row in the input catalogue copy all the columns to a new row
*  appended to the end of the output catalogue.
      DO ROW = 1, ROWS

*  Read the next row from the input catalogue into the input row buffer.
         CALL CAT_RGET( CIIN, ROW, STATUS )

*  Loop over each column to be copied.
         DO CURID = 1, NUMID

*  If the current column holds modified polpack values, get the index of
*  the corresponding row within the array returned by POL1_CPMOD.
            TROW = 0
            IF( IPT .NE. 0 ) THEN
               IF( FIINI(CURID) .EQ. XID ) THEN
                  TROW = 1
               ELSE IF( FIINI(CURID) .EQ. YID ) THEN
                  TROW = 2
               ELSE IF( FIINI(CURID) .EQ. QID ) THEN
                  TROW = 3
               ELSE IF( FIINI(CURID) .EQ. DQID ) THEN
                  TROW = 4
               ELSE IF( FIINI(CURID) .EQ. UID ) THEN
                  TROW = 5
               ELSE IF( FIINI(CURID) .EQ. DUID ) THEN
                  TROW = 6
               ELSE IF( FIINI(CURID) .EQ. ANGID ) THEN
                  TROW = 7
               ELSE IF( FIINI(CURID) .EQ. DANGID ) THEN
                  TROW = 8
               END IF
            END IF

*  If this is a modified polpack column value, get the modified value from
*  the array returned by POL1_CPMOD (a REAL) and store it in the
*  current row buffer for the output catalogue.
            IF( TROW .GT. 0 ) THEN
               CALL POL1_GET2D( ROWS, 8, %VAL( CNF_PVAL(IPT) ), ROW,
     :                          TROW, VALUER, STATUS )
               CALL CAT_PUT0R( FIOUTI(CURID), VALUER,
     :                         (VALUER .EQ. VAL__BADR), STATUS )

*  For other columns, check the data type of the column, and for the
*  appropriate data type: Get the value for the current field from the input
*  row buffer. Put the value to the current field in the output catalogue.
            ELSE IF( FDTYPI(CURID) .EQ. CAT__TYPEB ) THEN
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

*  Release the table created by POL1_CPMOD.
      IF( IPT .NE. 0 ) CALL PSX_FREE( IPT, STATUS )

*  If the status is not ok then report an error.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'Error copying catalogue table.', STATUS )
      END IF

      END
