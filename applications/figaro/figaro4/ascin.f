      SUBROUTINE ASCIN( STATUS )
*+
*  Name:
*     ASCIN

*  Purpose:
*     Read a 1-D or N-D data set from an ASCII table.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASCIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine reads axis values, pixel widths, data values, and
*     data errors from an ASCII table into an NDF data structure.
*     Most of these items are optional, mandatory are only
*     axis values for each axis and data values. Pixel widths can be
*     read only in the one-dimensional case.
*
*     The user specifies in which columns the different items are to be
*     found. A range of line numbers to be used can be specified.
*     Comment lines may be interspersed in this line range, if they are
*     marked by an exclamation mark in the first or second character.
*     All columns leftward of the rightmost used column must be
*     numeric, non-numeric data may follow in further columns.
*     Up to 132 characters are read from table lines. Numbers are read
*     as _REAL.
*
*     If the result is one-dimensional, the axis values will be taken
*     literally to define a grid, which in general may be non-linear and
*     non-monotonic. If the result is multi-dimensional, the routine
*     will guess from the table a grid that is linear in all directions.
*     The parameter system is consulted to confirm or modify the
*     suggested grid.
*
*     The data value read from a line will be stored into exactly one
*     output pixel, if and only if the table coordinates match that
*     pixel's coordinate to within a specified fraction of the pixel
*     step. Pixels for which no data are in the table are assigned the
*     bad value. Table data equal to a specified "alternative bad value"
*     are replaced by the bad value before insertion into the data set.
*     Where more than one table line corresponds to the same pixel, the
*     pixel is assigned the last value from the table. That is, later
*     specifications of the same pixel override previous ones.

*  Usage:
*     ascin in lines colaxes=? coldata=? [start=? step=? end=?] out=?

*  ADAM Parameters:
*     INFO = _LOGICAL (Read)
*        If false, the routine will issue only error messages and no
*        informational messages. This parameter is of significance only
*        if the output is multi-dimensional. [YES]
*     TOL = _REAL (Read)
*        The tolerated fraction of the pixel size by which the table
*        coordinates may deviate from the pixel coordinates. For a line
*        read from the ASCII table, if any one of the axis values
*        deviates by more than TOL times the pixel step, then the
*        information from the table is disregarded. This parameter is of
*        no significance, if the output is one-dimensional, since in
*        that case the axis values found will define the exact
*        (non-linear) grid. [0.2]
*     BAD = _REAL (Read)
*        The alternative bad value, i.e. the bad value used in the
*        table. Any data or error value found in the table that is equal
*        to BAD, is replaced by the bad value before insertion into the
*        output. [-999999.]
*     IN = FILENAME (Read)
*        The file containing the ASCII table.
*     LINES( 2 ) = _INTEGER (Read)
*        The line numbers of the first and last lines to be used from
*        the table file. [1,9999]
*     COLAXES( 7 ) = _INTEGER (Read)
*        The column numbers where the axis values are to be found. All
*        axes must be specified, i.e. at least one. The number of
*        leading non-zero elements defines the number of axes in the
*        output. [1,2]
*     COLWIDTH = _INTEGER (Read)
*        The column numbers where the pixel width values are to be
*        found. This parameter is of significance only if the output is
*        one-dimensional. Enter a 0 if no width information is
*        available. [0]
*     COLDATA( 2 ) = _INTEGER (Read)
*        The column numbers where the data values (first element) and
*        their associated error values (second element) are to be
*        found. If no error information is available, enter 0 as second
*        element. [3,0]
*     START( 7 ) = _REAL (Read)
*        The coordinates of the first pixel. This parameter is of
*        no significance, if the output is one-dimensional, since in
*        that case the axis values found will define the exact
*        (non-linear) grid.
*     STEP( 7 ) = _REAL (Read)
*        The coordinate increments per pixel. This parameter is of
*        no significance, if the output is one-dimensional, since in
*        that case the axis values found will define the exact
*        (non-linear) grid.
*     END( 7 ) = _REAL (Read)
*        The coordinates of the last pixel. This parameter is of
*        no significance, if the output is one-dimensional, since in
*        that case the axis values found will define the exact
*        (non-linear) grid.
*     OUT = NDF (Read)
*        The NDF where to store the data.

*  Implementation Status:
*     It is not possible to read axis values from the table in double
*     precision or create a double precision axis array.

*  Examples:
*     ascin in [1,9999] colaxes=[1,2] coldata=[3,4]
*     start=[0,0] end=[2.5,5] step=[0.1,1] out=out
*        This will read the data from the ASCII file IN, using line
*        numbers 1 to 9999 (or till end of file if there are less lines
*        in IN). The 1st axis data are taken from the first column, the
*        2nd axis data from the second column. The image data are taken
*        from the 3rd column and their errors from the 4th column. The
*        routine tries to store the table data into a grid with the 1st
*        axis running from 0 to 2.5 in steps of 0.1 (26 pixels) and the
*        2nd axis running from 0 to 5 in steps of 1 (6 pixels). If a
*        coordinate pair from columns 1&2 matches any pixel centre well
*        enough, the data from columns 4&5 are entered into the
*        corresponding element of the data and errors array. The data
*        file is OUT.
*     ascin in out [25,39] colaxes=5 coldata=[3,0]
*        Here the output is one-dimensional and without errors array
*        (thus the zero in COLDATA). Only lines 25 to 39 from IN are
*        used. The axis data are from the 5th column and the spectrum
*        data from the 3rd column. (Note that columns 1, 2 and 4 must
*        contain numeric data.) The axis grid need not be specified. The
*        axis values from the table will be taken literally to form a
*        grid that is in general non-linear and non-monotonic.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18 May 1991 (hme):
*        Original version.
*     22 Nov 1991 (hme):
*        Rewrite as N-D version. If 1-D, use table's axis values for
*        non-linear grid. This is the first application to use
*        MAXDIM = 7  rather than 10.
*     15 Dec 1991 (hme):
*        Suppress Starlink error messages arising from DSA-calls.
*     05 May 1992 (hme):
*        Port to NDF and Unix.
*     09 Sep 1992 (hme):
*        Don't set TITLE.
*     03 May 1993 (hme):
*        Initialise data and variance as bad. Before only data was set
*        bad before filling from table and variance partly contained
*        garbage.
*     24 Nov 1994 (hme):
*        Use new libraries.
*     15 Nov 1995 (hme):
*        Open the ASCII file in list mode, not Fortran.
*     2005 June 1 (MJC):
*        Use CNF_PVAL for pointers to mapped data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! Standard NDF constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER MAXVAL             ! Maximum number of values read
      PARAMETER ( MAXVAL = 16 )  ! 7 axes + 7 width + data + errors

*  Local Variables:
      INTEGER LINES( 2 )
      INTEGER COLAXE( NDF__MXDIM )
      INTEGER COLWID
      INTEGER COLDAT( 2 )
      REAL START( NDF__MXDIM )
      REAL STEP(  NDF__MXDIM )
      REAL ENDV(  NDF__MXDIM )
      REAL EPS
      REAL BADVAL
      LOGICAL INFO
      LOGICAL VARUSE             ! True if errors to be read from table
      INTEGER I, J               ! Loop index
      INTEGER NDF                ! Output NDF identifier
      INTEGER NDIM               ! Number of axes in OUT
      INTEGER NELM               ! Number of pixels in OUT
      INTEGER LBND(  NDF__MXDIM ) ! Lower bounds of NDF
      INTEGER UBND(  NDF__MXDIM ) ! Upper bounds of NDF
      INTEGER AXIS(  NDF__MXDIM ) ! Axis pointers
      INTEGER DATA               ! Pointer to data values
      INTEGER DVAR               ! Pointer to data variances
      INTEGER FD                 ! FIO File descriptor
      INTEGER FILENO             ! Fortran unit number
      INTEGER NRET               ! No. of array elements given

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Startup.
      CALL NDF_BEGIN

*  Modal parameters.
      CALL PAR_GET0L( 'INFO', INFO,   STATUS )
      CALL PAR_GET0R( 'TOL',  EPS,    STATUS )
      CALL PAR_GET0R( 'BAD',  BADVAL, STATUS )

*  Get input file.
      CALL FIO_ASSOC( 'IN', 'READ', 'LIST', 0, FD, STATUS )
      CALL FIO_UNIT( FD, FILENO, STATUS )

*  Line number range.
      CALL PAR_GET1I( 'LINES', 2, LINES, NRET, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF (  LINES(1) .LE. 0 .OR. LINES(2) .LE. 0 .OR.
     :      LINES(2) .LT. LINES(1) ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASCIN_LINCOL',
     :      'ASCIN: Error in line numbers.', STATUS )
         GO TO 500
      ENDIF

*  Columns where to find axis data.
*  The only illegal case is when the first element is 0 or negative.
      CALL PAR_GET1I( 'COLAXES', NDF__MXDIM, COLAXE, NRET, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( COLAXE(1) .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASCIN_LINCOL',
     :      'ASCIN: No axis columns specified.', STATUS )
         GO TO 500
      END IF

*  The dimensionality of the output is the number of returned elements,
*  or the number of the first 0-element minus 1.
      NDIM = NRET
      DO 1 I = 2, NRET
         IF ( COLAXE(I) .LE. 0 ) THEN
            NDIM = I - 1
            GO TO 2
         END IF
 1    CONTINUE
 2    CONTINUE

*  If 1-D, column where to find widths.
      IF ( NDIM .EQ. 1 ) CALL PAR_GET0I( 'COLWIDTH', COLWID, STATUS )

*  Columns where to find data and errors.
*  The only illegal case is when the first element is 0 or negative.
      CALL PAR_GET1I( 'COLDATA', 2, COLDAT, NRET, STATUS )
      IF ( STATUS .NE. SAI__OK ) GO TO 500
      IF ( COLDAT(1) .LE. 0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ASCIN_LINCOL',
     :      'ASCIN: No data column specified.', STATUS )
         GO TO 500
      END IF

*  Fill up the arrays and change negative column numbers to 0.
*  COLDAT(2).EQ.0 also indicates that no variances are to be stored in
*  the output.
      IF ( NRET .LT. 2 ) COLDAT(2) = 0
      IF ( COLWID .LT. 0 ) COLWID = 0
      DO 3 I = 1, NDF__MXDIM
         IF ( COLAXE(I) .LT. 0 .OR. I .GT. NDIM ) COLAXE(I) = 0
 3    CONTINUE
      VARUSE = ( COLDAT(2) .NE. 0 )

*  If output is 1-D, take axis data literally.
      IF ( NDIM .EQ. 1 ) THEN

*     Work out the size of the output data set. This includes a first
*     pass through the table to find the actual number of non-comment
*     lines between LINES(1) and LINES(2) or EOF.
         CALL SPD_WZZA( FILENO, LINES, COLAXE(1), COLWID, COLDAT,
     :      NELM, STATUS )
         CALL FIO_RWIND( FD, STATUS )
         LBND(1) = 1
         UBND(1) = NELM
         IF ( NELM .LT. 1 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'ASCIN_NODATA',
     :         'ASCIN: No data found in table.', STATUS )
         END IF

*     Get output NDF.
         CALL NDF_CREAT( 'OUT', '_REAL', NDIM, LBND, UBND, NDF, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Map the axis and width.
         CALL NDF_AMAP( NDF, 'CENTRE', 1, '_REAL', 'WRITE', AXIS(1), J,
     :      STATUS )
         IF ( COLWID .NE. 0 ) CALL NDF_AMAP( NDF, 'WIDTH', 1, '_REAL',
     :      'WRITE', AXIS(2), J, STATUS )

*     Map the data and error arrays.
*     No initialisation is necessary, since for 1-D all information is
*     in the ASCII table.
         CALL NDF_MAP( NDF, 'DATA', '_REAL', 'WRITE', DATA, J, STATUS )
         IF ( VARUSE ) CALL NDF_MAP( NDF, 'VARIANCE', '_REAL', 'WRITE',
     :      DVAR, J, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Copy axis, width, data, variance from table to output.
         CALL SPD_WZZB( FILENO, BADVAL, LINES, COLAXE, COLWID, COLDAT,
     :                  NELM, %VAL( CNF_PVAL(AXIS(1)) ),
     :                  %VAL( CNF_PVAL(AXIS(2)) ),
     :                  %VAL( CNF_PVAL(DATA) ), %VAL( CNF_PVAL(DVAR) ),
     :                  STATUS )

*  Else (if output is N-D, do it the complicated way).
      ELSE

*     Pass 1 through data: Find defaults for START, END, STEP.
         CALL SPD_WZZC( FILENO, NDIM, LINES, COLAXE, START, ENDV, STEP,
     :      STATUS )
         CALL FIO_RWIND( FD, STATUS )
         CALL PAR_DEF1R( 'START', NDIM, START, STATUS )
         CALL PAR_DEF1R(  'STEP', NDIM,  STEP, STATUS )
         CALL PAR_DEF1R(   'END', NDIM,  ENDV, STATUS )

*     Confirm the coordinate grid.
         CALL PAR_GET1R( 'START', NDIM, START, NRET, STATUS )
         CALL PAR_GET1R(  'STEP', NDIM,  STEP, NRET, STATUS )
         CALL PAR_GET1R(   'END', NDIM,  ENDV, NRET, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Work out the shape of the output data set.
         NELM = 1
         DO 4 I = 1, NDIM

*        If grid specification is invalid, abort.
            IF ( STEP(I) * ( ENDV(I) - START(I) ) .EQ. 0. ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP( 'ASCIN_NOGRID',
     :            'ASCIN: Extent or step is zero along some axis.',
     :            STATUS )
               GO TO 500
            ENDIF

*        The grid along any axis can be:
*        forward: ENDV .GT. START .AND. STEP .GT. 0
*        reverse: ENDV .LT. START .AND. STEP .LT. 0
*        We accept the current information about START and ENDV as
*        correct and make STEP consistent.
            IF ( STEP(I) * ( ENDV(I) - START(I) ) .LT. 0. )
     :         STEP(I) = -STEP(I)

*        Now we can be sure to calculate a positive number of pixels.
*        The following will prefer an extra pixel over one missing.
            LBND(I) = 1
            UBND(I) = 1 + INT( ( ENDV(I) - START(I) ) / STEP(I) + 0.5 )
            NELM = NELM * UBND(I)

*        We stick to the given start, but adjust the end to fall onto a
*        pixel.
            ENDV(I) = START(I) + ( UBND(I) - LBND(I) ) * STEP(I)
 4       CONTINUE

*     Fill the shape array for unsused axes.
         DO 5 I = NDIM+1, NDF__MXDIM
            LBND(I) = 1
            UBND(I) = 1
 5       CONTINUE

*     Get output NDF.
         CALL NDF_CREAT( 'OUT', '_REAL', NDIM, LBND, UBND, NDF, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Map the axes and fill them with values.
         DO 6 I = 1, NDIM
            CALL NDF_AMAP( NDF, 'CENTRE', I, '_REAL', 'WRITE', AXIS(I),
     :          J, STATUS )
            IF ( STATUS .NE. SAI__OK ) GO TO 500
            CALL SPD_UAAJR( START(I), ENDV(I), UBND(I),
     :                      %VAL( CNF_PVAL(AXIS(I)) ), STATUS )
 6       CONTINUE

*     Map the data and error arrays.
*     These are initialised as bad. Both of them.
         CALL NDF_MAP( NDF, 'DATA', '_REAL', 'WRITE/BAD',
     :      DATA, J, STATUS )
         IF ( VARUSE ) CALL NDF_MAP( NDF, 'VARIANCE', '_REAL',
     :      'WRITE/BAD', DVAR, J, STATUS )
         IF ( STATUS .NE. SAI__OK ) GO TO 500

*     Copy from table to data and variance arrays.
         CALL SPD_WZZD( FILENO, INFO, EPS, BADVAL, LINES, COLAXE,
     :                  COLDAT, NDIM, UBND, START, ENDV, NELM,
     :                  %VAL( CNF_PVAL(DATA) ),
     :                  %VAL( CNF_PVAL(DVAR) ), STATUS )
      END IF

*  Close down.
 500  CONTINUE
      CALL NDF_ANNUL( NDF, STATUS )
      CALL NDF_END( STATUS )
      CALL FIO_CANCL( 'IN', STATUS )
      CALL FIO_DEACT( STATUS )

      END
