	SUBROUTINE CUT_LINEPLOT( NUMBER_POINTS, X_VALUES, CUT_VALUES,
     :	                         X_LO, X_HI, Y_LO, Y_HI, XST, XEN, YST,
     :	                         YEN, CUT_MAGNIF, STATUS)

* Plot cut on workstation

* History
*  20-Jul-1994 Changed STR$ to CHR_ and LIB$ to FIO_ (SKL@JACH)
*  26-OCT-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'ADAM_DEFNS'
        INCLUDE 'SAE_PAR'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'
        INCLUDE 'CHR_ERR'
!        INCLUDE 'FIO_ERR'
	INCLUDE 'PLT2DCOM'

	INTEGER
     :	  J,
     :	  CUT_MARKINC,
     :	  LUN,
     :	  MARKER_TYPE,
     :	  NUMBER_POINTS,
     :	  STATUS

	REAL
     :    CUT_MAGNIF,
     :	  CUT_MARKSIZ,
     :	  CUT_VALUES( NUMBER_POINTS),
     :	  XEN,
     :	  X_HALF,
     :	  X_HI,
     :	  X_LO,
     :	  X_POS,
     :	  XST,
     :	  X_VALUES( NUMBER_POINTS),
     :	  YEN,
     :	  Y_HI,
     :	  Y_LO,
     :	  Y_POS,
     :	  YST

	CHARACTER
     :	  CUT_LINETYPE*1,
     :	  CUT_MARKER*1,
     :	  FF_FILE*80,
     :	  OUTFILE*80

* ===========================================================================

* test status on entry

	IF( STATUS .NE. SAI__OK) THEN

	  RETURN

	END IF

* check that the plot area is on the device

	IF( XST .LT. 0.0) XST = 0.0

	IF( XEN .GT. MAX_X) XEN = MAX_X

	IF( YST .LT. 0.0) YST = 0.0

	IF( YEN .GT. MAX_Y) YEN = MAX_Y

* get cut line option

	CALL PAR_GET0C( 'CUT_LINETYPE', CUT_LINETYPE, STATUS)

* if want markers then get marker parameters

	IF( CUT_LINETYPE .EQ. 'M') THEN

	  CALL PAR_GET0C( 'CUT_MARKTYPE', CUT_MARKER, STATUS)

	  CALL PAR_GET0I( 'CUT_MARKINC', CUT_MARKINC, STATUS)

	  CALL PAR_GET0R( 'CUT_MARKSIZ', CUT_MARKSIZ, STATUS)

* interpret marker type

	  IF( CUT_MARKER .EQ. '*') THEN

	    MARKER_TYPE = 1

	  ELSE IF( CUT_MARKER .EQ. '+') THEN

	    MARKER_TYPE = 2

	  ELSE IF( CUT_MARKER .EQ. '.') THEN

	    MARKER_TYPE = 3

	  ELSE IF( CUT_MARKER .EQ. 'O') THEN

	    MARKER_TYPE = 4

	  ELSE IF( CUT_MARKER .EQ. 'X') THEN

	    MARKER_TYPE = 5

	  ELSE

	    MARKER_TYPE = 1

	  END IF

	END IF

* get option to put cut data to free format file

	CALL PAR_GET0C( 'FF_FILE', FF_FILE, STATUS)

* if user wants ff file output of cut then get lun and open file

	IF( FF_FILE( 1:1) .EQ. 'Y') THEN

* get the name of the file to be created

	  CALL PAR_GET0C( 'CUT_OUTFF', OUTFILE, STATUS)
	  LUN = 143
	  OPEN( UNIT=LUN, FILE=OUTFILE, STATUS='UNKNOWN')

* write cut to ff file if user wants it ...

	  DO J = 1, NUMBER_POINTS
	    WRITE( LUN, *) X_VALUES( J), CUT_VALUES( J)
	  END DO

* close the file and release the lun

	  CLOSE( LUN)
	END IF

* scale the X and Y data to the raster coordinate system in use

	DO J = 1, NUMBER_POINTS

	  X_VALUES( J) = XST +
     :	   ( X_VALUES( J) - X_LO)/( X_HI - X_LO)*( XEN - XST)

	  CUT_VALUES( J) = YST +
     :	   ( CUT_VALUES( J) - Y_LO)/( Y_HI - Y_LO)*( YEN - YST)

	END DO

* test if user selected anything other than B,H or M and plot LINE

	IF( CUT_LINETYPE .NE. 'B' .AND.
     :	    CUT_LINETYPE .NE. 'H' .AND.
     :	    CUT_LINETYPE .NE. 'M') THEN

* start a polyline with first point

	  X_POS = X_VALUES( 1)
	  Y_POS = MIN( CUT_VALUES( 1), YEN)
	  Y_POS = MAX( Y_POS, YST)
	  CALL SGS_BPOLY( X_POS, Y_POS)

* loop to form polyline

	  DO J = 2, NUMBER_POINTS

	    X_POS = X_VALUES( J)
	    Y_POS = MIN( CUT_VALUES( J), YEN)
	    Y_POS = MAX( Y_POS, YST)
	    CALL SGS_APOLY( X_POS, Y_POS)

	  END DO

	END IF

* test if want BLOCK type line

	IF( CUT_LINETYPE .EQ. 'B') THEN

* define half a box width

	  X_HALF = ( X_VALUES( 2) - X_VALUES( 1))/2.0

* start a polyline with first point

	  X_POS = X_VALUES( 1)
	  Y_POS = MIN( CUT_VALUES( 1), YEN)
	  Y_POS = MAX( Y_POS, YST)
	  CALL SGS_BPOLY( X_POS, Y_POS)

* loop to form polyline

	  DO J = 2, NUMBER_POINTS

	    X_POS = X_VALUES( J) - X_HALF
	    Y_POS = MIN( CUT_VALUES( J - 1), YEN)
	    Y_POS = MAX( Y_POS, YST)
 	    CALL SGS_APOLY( X_POS, Y_POS)

	    X_POS = X_VALUES( J) - X_HALF
	    Y_POS = MIN( CUT_VALUES( J), YEN)
	    Y_POS = MAX( Y_POS, YST)
	    CALL SGS_APOLY( X_POS, Y_POS)

 	  END DO

	  X_POS = X_VALUES( NUMBER_POINTS)
	  Y_POS = MIN( CUT_VALUES( NUMBER_POINTS), YEN)
	  Y_POS = MAX( Y_POS, YST)
	  CALL SGS_APOLY( X_POS, Y_POS)

	END IF

* test if HISTOGRAM type plot

	IF( CUT_LINETYPE .EQ. 'H') THEN

* define half a box width

	  X_HALF = ( X_VALUES( 2) - X_VALUES( 1))/2.0

* start a polyline with first point

	  X_POS = XST
	  Y_POS = YST
	  CALL SGS_BPOLY( X_POS, Y_POS)

* loop to form polyline

	  DO J = 2, NUMBER_POINTS

	    X_POS = MAX( ( X_VALUES( J - 1) - X_HALF), XST)
	    Y_POS = MIN( CUT_VALUES( J - 1), YEN)
	    Y_POS = MAX( Y_POS, YST)
	    CALL SGS_APOLY( X_POS, Y_POS)

	    X_POS = X_VALUES( J) - X_HALF
	    Y_POS = MIN( CUT_VALUES( J - 1), YEN)
	    Y_POS = MAX( Y_POS, YST)
	    CALL SGS_APOLY( X_POS, Y_POS)

	    X_POS = X_VALUES( J) - X_HALF
	    Y_POS = YST
	    CALL SGS_APOLY( X_POS, Y_POS)

	  END DO

	END IF

* test if want MARKER plot

	IF( CUT_LINETYPE .EQ. 'M') THEN

* set marker size

	  CALL SGS_SHMK( CUT_MARKSIZ)

* DRAW polymarker

	  DO J = 1, NUMBER_POINTS, CUT_MARKINC

	    X_POS = X_VALUES( J)
	    Y_POS = MIN( CUT_VALUES( J), YEN)
	    Y_POS = MAX( Y_POS, YST)
	    CALL SGS_MARK( X_POS, Y_POS, MARKER_TYPE)

	  END DO

	END IF

* empty working buffer of graphics

	CALL SGS_FLUSH

	END
