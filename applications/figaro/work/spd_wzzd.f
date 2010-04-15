      SUBROUTINE SPD_WZZD( FILENO, INFO, EPS, BADVAL, LINES, COLAXE,
     :   COLDAT, NDIM, DIMS, START, ENDV, NELM, DATA, DVAR, STATUS )
*+
*  Name:
*     SPD_WZZD

*  Purpose:
*     ASCIN's second pass through table (N-D case).

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_WZZD( FILENO, INFO, EPS, BADVAL, LINES, COLAXE, COLDAT,
*        NDIM, DIMS, START, ENDV, NELM, DATA, DVAR, STATUS )

*  Description:
*     This routine is designed to be called by ASCIN. It reads a certain
*     range of lines from a file with an ASCII table. From these lines
*     it extracts numbers from certain columns. The first NDIM numbers
*     are taken as coordinates. If they match a pixel in the grid
*     specified by START, ENDV and DIMS, the last one or two extracted
*     numbers are taken as data and error and stored as data and
*     variance in the appropriate element of the arrays DATA and DVAR.
*     If either data or error is BADVAL, it is replaced by VAL__BADR.

*  Arguments:
*     FILENO = INTEGER (Given)
*        Fortran unit number of the table file.
*     INFO = LOGICAL (Given)
*        If true, issue a warning for disregarded table values.
*     EPS = REAL (Given)
*        Pixel match tolerance. The fraction of pixel size by which the
*        pixel centre may be missed by the position as found in the
*        table. If the position read from the table misses by more than
*        EPS in any direction, that line from the table is disregarded.
*     BADVAL = REAL (Given)
*        The table's bad value. A data or error value found in the
*        table to equal BADVAL will be transformed into a data or
*        variance value of VAL__BADR in the output.
*     LINES( 2 ) = INTEGER (Given)
*        The start and end number of lines to be used.
*     COLAXE( MAXDIM ) = INTEGER (Given)
*        The column numbers where in the table to find the various axis
*        data.
*     COLDAT( 2 ) = INTEGER (Given)
*        The column numbers where in the table to find the data and the
*        data errors. COLDAT(2).EQ.0 signals that no error are to be
*        looked for.
*     NDIM = INTEGER (Given)
*        Number of axes in output array.
*     DIMS( NDIM ) = INTEGER (Given)
*        The number of pixels along each axis.
*     START( NDIM ) = REAL (Given)
*        Coordinates of the start pixel.
*     ENDV( NDIM ) = REAL (Given)
*        Coordinates of the end pixel.
*     NELM = INTEGER (Given)
*        The total number of pixels, i.e. the length of arrays DATA and
*        DVAR.
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
*     HME: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     18-MAY-1991 (HME):
*        Original version.
*     05-JUL-1991 (HME):
*        STATUS added to argument list.
*     22-NOV-1991 (HME):
*        Convert to N-D version.
*     09-JUL-1992 (HME):
*        Set the real token each time it is needed.
*        Avoid NINT.
*     03-MAY-1993 (HME):
*        Need no longer initialise data and variance to be bad. This is
*        done by NDF_MAP at a higher level.
*     24 Nov 1994 (hme):
*        Renamed from ASCIN2.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad values

*  Local constants:
      INTEGER MAXVAL             ! Maximum number of values read
      PARAMETER ( MAXVAL = 9 )   ! 7 axes + data + error
      INTEGER MAXDIM             ! Maximum supported no. of axes
      PARAMETER ( MAXDIM = 7 )

*  Arguments Given:
      INTEGER FILENO
      LOGICAL INFO
      REAL EPS
      REAL BADVAL
      INTEGER LINES(  2 )
      INTEGER COLAXE( MAXDIM )
      INTEGER COLDAT( 2 )
      INTEGER NDIM
      INTEGER DIMS( NDIM )
      REAL START( NDIM )
      REAL ENDV(  NDIM )
      INTEGER NELM

*  Arguments Returned:
      REAL DATA( NELM )
      REAL DVAR( NELM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      LOGICAL TAKE               ! True if table value to go into pixel
      INTEGER I, J, K            ! Loop indices
      INTEGER RDSTAT             ! Status returned from SPD_UAAK
      INTEGER GETVAL             ! Number of columns to be read
      INTEGER MAXCOL             ! Rightmost column used
      INTEGER COLUMN( MAXVAL )   ! Column numbers to be used
      REAL VALI(      MAXVAL )   ! Values read
      REAL PIXEL(     MAXVAL )   ! Pixel numbers of coordinates
      CHARACTER * ( 132 ) LINE   ! A line of text from table file

*.

*  Check inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the column numbers and find the highest column number.
*  First come all axis columns, then the data column, then the error
*  column.
      COLUMN(1) = COLAXE(1)
      MAXCOL = COLUMN(1)
      DO 1 I = 2, NDIM
         COLUMN(I) = COLAXE(I)
         MAXCOL = MAX( MAXCOL, COLUMN(I) )
 1    CONTINUE
      COLUMN(NDIM+1) = COLDAT(1)
      MAXCOL = MAX( MAXCOL, COLUMN(NDIM+1) )
      GETVAL = NDIM+1
      IF ( COLDAT(2) .GT. 0 ) THEN
         COLUMN(NDIM+2) = COLDAT(2)
         MAXCOL = MAX( MAXCOL, COLUMN(NDIM+2) )
         GETVAL = NDIM+2
      END IF

*  Check number of requested values against maximum supported.
      IF ( GETVAL .GT. MAXVAL ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASCIN_MAXVAL',
     :      'ASCIN: Pass 2: Cannot handle that many columns.', STATUS )
      END IF

*  Forward to first selected line.
      IF ( LINES(1) .GT. 1 ) THEN
         DO 2 I = 2, LINES(1)
            READ ( FILENO, '(A)', END = 501 ) LINE
 2       CONTINUE
      END IF

*  Loop through selected part of file.
      DO 6 I = LINES(1), LINES(2)

*     Read the values in this line.
         CALL SPD_UAAK( FILENO, MAXCOL, GETVAL, VAL__BADR, COLUMN, VALI,
     :      LINE, RDSTAT )

*     If EOF found, the previous line was the last.
         IF ( RDSTAT .EQ. 1 ) THEN
            GO TO 5

*     Else if this was a comment line, goto next line.
         ELSE IF ( RDSTAT .EQ. 2 ) THEN
            GO TO 5

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

*     Hit which pixel and how well. TAKE remains true, if hit is well
*     enough.
*     We require that VALI within [START,ENDV], thus PIXEL is positive
*     and we can use INT(...+0.5).
         TAKE = .TRUE.
         DO 3 J = 1, NDIM
            PIXEL(J) = ( VALI(J) - START(J) ) / ( ENDV(J) - START(J) )
     :               * ( DIMS(J) - 1 ) + 1.
            IF ( ABS( PIXEL(J) - INT(PIXEL(J)+.5) ) .GT. EPS .OR.
     :           PIXEL(J) .LT. (1.-EPS) .OR.
     :           PIXEL(J) .GT. (DIMS(J)+EPS) ) TAKE = .FALSE.
            PIXEL(J) = INT( PIXEL(J) + 0.5 )
 3       CONTINUE

*     If the pixel is hit well enough, ...
         IF ( TAKE ) THEN

*        Find out the corresponding array index.
            K = PIXEL(NDIM) - 1
            DO 4 J = NDIM-1, 1, -1
               K = K * DIMS(J) + PIXEL(J) - 1
 4          CONTINUE
            K = K + 1

*        Copy from table to output arrays.
*        Any VALI may be VAL__BADR due to some failure of SPD_UAAK. This
*        needs being filtered only for the error/variance.
*        But BADVAL must be filtered for data and error.
            DATA(K) = VALI(NDIM+1)
            IF ( VALI(NDIM+1) .EQ. BADVAL ) DATA(K) = VAL__BADR
            IF ( COLDAT(2) .GT. 0 ) THEN
               IF ( VALI(NDIM+2) .EQ. BADVAL .OR.
     :              VALI(NDIM+2) .EQ. VAL__BADR ) THEN
                  DVAR(K) = VAL__BADR
               ELSE
                  DVAR(K) = VALI(NDIM+2) ** 2
               END IF
            END IF

*     Else (if the pixel was not hit well enough), give a warning.
         ELSE IF ( INFO ) THEN
            CALL MSG_SETR( 'REAL', 1E2*EPS )
            CALL MSG_SETI( 'INT', I )
            CALL MSG_OUT( 'ASCIN_PIXMSD',
     :         'ASCIN: Line no. ^INT: ' //
     :         'Pixel missed by more than ^REAL percent.',
     :         STATUS )
         END IF
 5    CONTINUE
 6    CONTINUE

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
