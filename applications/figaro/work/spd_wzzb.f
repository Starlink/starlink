      SUBROUTINE SPD_WZZB( FILENO, BADVAL, LINES,
     :   COLAXE, COLWID, COLDAT,
     :   NELM, AXIS, WIDTH, DATA, DVAR, STATUS )
*+
*  Name:
*     SPD_WZZB

*  Purpose:
*     ASCIN's second pass through table (1-D case).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZZB( FILENO, BADVAL, LINES, COLAXE, COLWID, COLDAT,
*        NELM, AXIS, WIDTH, DATA, DVAR, STATUS )

*  Description:
*     This routine is designed to be called by ASCIN. It reads a certain
*     range of lines from a file with an ASCII table. From these lines
*     it extracts numbers from certain columns. The numbers found are
*     taken as axis value, pixel width, data value, data error. These
*     are stored into the arrays AXIS, WIDTH, DATA, DVAR (the error is
*     squared to store the variance in DVAR). If either data or error
*     is BADVAL, it is replaced by VAL__BADR.

*  Arguments:
*     FILENO = INTEGER (Given)
*        Fortran unit number of the table file.
*     BADVAL = REAL (Given)
*        The table's bad value. A data or error value found in the
*        table to equal BADVAL will be transformed into a data or
*        variance value of VAL__BADR in the output.
*     LINES( 2 ) = INTEGER (Given)
*        The start and end number of lines to be used.
*     COLAXE = INTEGER (Given)
*        The column number where in the table to find the axis data.
*     COLWID = INTEGER (Given)
*        The column number where in the table to find the pixel width.
*     COLDAT( 2 ) = INTEGER (Given)
*        The column numbers where in the table to find the data and the
*        data errors. COLDAT(2).EQ.0 signals that no error are to be
*        looked for.
*     NELM = INTEGER (Given)
*        The total number of pixels, i.e. the length of the arrays.
*     AXIS( NELM ) = REAL (Returned)
*        The output axis array (taken from the column specified by
*        COLAXE).
*     WIDTH( NELM ) = REAL (Returned)
*        The output width array (taken from the column specified by
*        COLWID). This array is referenced only if COLWID.NE.0.
*     DATA( NELM ) = REAL (Returned)
*        The output data array (taken from the column specified by
*        COLDAT(1)).
*     DVAR( NELM ) = REAL (Returned)
*        The output variance array (taken as square of the values from
*        the column specified by COLDAT(2)). This array is referenced
*        only if COLDAT(2).NE.0.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     22 Nov 1991 (hme):
*        Original version.
*     24 Nov 1994 (hme):
*        Renamed from ASCIN4.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values

*  Arguments Given:
      INTEGER FILENO
      REAL BADVAL
      INTEGER LINES( 2 )
      INTEGER COLAXE
      INTEGER COLWID
      INTEGER COLDAT( 2 )
      INTEGER NELM

*  Arguments Returned:
      REAL AXIS(  NELM )
      REAL WIDTH( NELM )
      REAL DATA(  NELM )
      REAL DVAR(  NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXVAL             ! Maximum number of values read
      PARAMETER ( MAXVAL = 4 )   ! axis + width + data + error

*  Local Variables:
      INTEGER I, J               ! Loop indices
      INTEGER RDSTAT             ! Status returned from SPD_UAAK
      INTEGER GETVAL             ! Number of columns to be read
      INTEGER MAXCOL             ! Rightmost column used
      INTEGER COLUMN( MAXVAL )   ! Column numbers to be used
      REAL VALI(      MAXVAL )   ! Values read
      CHARACTER * ( 132 ) LINE   ! A line of text from table file

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the column numbers and find the highest column number.
*  First comes the axis column and the width column, then the data
*  column, then the error column.
      GETVAL = 1
      COLUMN(1) = COLAXE
      MAXCOL = COLUMN(1)
      IF ( COLWID .NE. 0 ) THEN
         GETVAL = GETVAL + 1
         COLUMN(GETVAL) = COLWID
         MAXCOL = MAX( MAXCOL, COLUMN(GETVAL) )
      END IF
      GETVAL = GETVAL + 1
      COLUMN(GETVAL) = COLDAT(1)
      MAXCOL = MAX( MAXCOL, COLUMN(GETVAL) )
      IF ( COLDAT(2) .GT. 0 ) THEN
         GETVAL = GETVAL + 1
         COLUMN(GETVAL) = COLDAT(2)
         MAXCOL = MAX( MAXCOL, COLUMN(GETVAL) )
      END IF

*  Check number of requested values against maximum supported.
      IF ( GETVAL .GT. MAXVAL ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASCIN_MAXVAL',
     :      'ASCIN: Pass 2: Cannot handle that many columns.', STATUS )
      END IF

*  Forward to first selected line.
      IF ( LINES(1) .GT. 1 ) THEN
         DO 1 I = 2, LINES(1)
            READ ( FILENO, '(A)', END = 501 ) LINE
 1       CONTINUE
      END IF

*  Loop through selected part of file.
      J = 0
      DO 6 I = LINES(1), LINES(2)

*     Read the values in this line.
         CALL SPD_UAAK( FILENO, MAXCOL, GETVAL, VAL__BADR, COLUMN, VALI,
     :      LINE, RDSTAT )

*     If EOF found, the previous line was the last.
         IF ( RDSTAT .EQ. 1 ) THEN
            GO TO 7

*     Else if this was a comment line, goto next line.
         ELSE IF ( RDSTAT .EQ. 2 ) THEN
            GO TO 6

*     Else if this line had less columns than we need, abort.
         ELSE IF ( RDSTAT .EQ. 3 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ASCIN_TOOFEW',
     :         'ASCIN: Pass 2: Line with too few columns found.',
     :         STATUS )
            GO TO 500

*     Else if SPD_UAAK cannot handle that high a column number, abort.
         ELSE IF ( RDSTAT .EQ. 4 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ASCIN_MAXCOL',
     :         'ASCIN: Pass 2: ' //
     :         'Cannot access such high a column number.',
     :         STATUS )
            GO TO 500
         END IF

*     Increment the array index.
         J = J + 1

*     If array too small, abort.
         IF ( J .GT. NELM ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ASCIN_OUTSIZ',
     :         'ASCIN: Output arrays too small for data found.',
     :         STATUS )
            GO TO 500

*     Else, copy from table into arrays.
*     Any VALI may be VAL__BADR due to some failure of SPD_UAAK. This
*     needs being filtered only for the error/variance.
*     But BADVAL must be filtered for data and error.
         ELSE

*        The axis value is the first item returned by SPD_UAAK.
            AXIS(J) = VALI(1)

*        If requested, the width is the second item.
            IF ( COLWID .GT. 0 ) WIDTH(J) = VALI(2)

*        If variance was requested, variance is the last and data the
*        last but one item.
            IF ( COLDAT(2) .GT. 0 ) THEN
               DATA(J) = VALI(GETVAL-1)
               IF ( VALI(GETVAL-1) .EQ. BADVAL ) DATA(J) = VAL__BADR
               IF ( VALI(GETVAL)   .EQ. BADVAL .OR.
     :              VALI(GETVAL)   .EQ. VAL__BADR ) THEN
                  DVAR(J) = VAL__BADR
               ELSE
                  DVAR(J) = VALI(GETVAL) ** 2
               END IF

*        Else (if no variance requested), data is the last item.
            ELSE
               DATA(J) = VALI(GETVAL)
               IF ( VALI(GETVAL) .EQ. BADVAL ) DATA(J) = VAL__BADR
            END IF
         END IF
 6    CONTINUE
 7    CONTINUE

*  If array too long, abort.
      IF ( J .NE. NELM ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASCIN_OUTSIZ',
     :      'ASCIN: Too few data found to fill arrays.', STATUS )
         GO TO 500
      END IF

*  Return without error.
      GO TO 500

*  EOF encountered while forwarding to first selected line.
 501  CONTINUE
      STATUS = SAI__ERROR
      CALL ERR_REP( 'ASCIN_BNDEOF',
     :   'ASCIN: Pass 2: First selected line is beyond EOF.',
     :   STATUS )

*  Return.
 500  CONTINUE

      END
