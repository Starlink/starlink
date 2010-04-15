	SUBROUTINE CONTOUR_TICKS( TICK_XINTERVAL, TICK_YINTERVAL,
     :                            CONTOUR_MAGNIF,
     :	                          CONTOUR_AXRAT, STATUS)

* Description : Plots tick marks on the axes formed by an image border
*               at user definable intervals and starting from a 0,0 position

* History
*   26thOct1994 CHanged MAGNIF from INT to REAL (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'ADAM_DEFNS'
	INCLUDE 'SAE_PAR'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

	INCLUDE 'PLT2DCOM'

* define local variables

	INTEGER J
	INTEGER NUMBER_X_TICKS
	INTEGER NUMBER_Y_TICKS
	INTEGER STATUS

	REAL CONTOUR_MAGNIF
	REAL CONTOUR_AXRAT
	REAL COSDELTA
	REAL DEC
	REAL REAL_NX
	REAL REAL_NY
	REAL TICKSCALE
	REAL TICK_XINTERVAL
	REAL TICK_YINTERVAL
	REAL X_ZERO
	REAL X1
	REAL X2
	REAL Y_ZERO
	REAL Y1
	REAL Y2

	CHARACTER*20 NUMBER_TYPE
	CHARACTER*20 TICK_LENGTH
	CHARACTER*20 TICK_INOUT

	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

*      get option to plot offset arcseconds/pixels or ra and dec

	CALL PAR_GET0C( 'NUMBER_TYPE', NUMBER_TYPE, STATUS)

*      test if want ra/dec numbers and if so get zero position values

	CALL PAR_GET0R( 'DEC_ZERO', DEC, STATUS)

	IF( NUMBER_TYPE .EQ. 'RA_DEC') THEN
	  COSDELTA = COS( ABS( DEC)*3.141592654/180.0)
!	type *, 'cosdelta = ', cosdelta
!	type *, 'contour_magnif = ', contour_magnif
	ELSE
	  COSDELTA = 1.0
	END IF

* get the X and Y zero point for first tick mark

	CALL PAR_GET0R( 'X_ZERO', X_ZERO, STATUS)
	CALL PAR_GET0R( 'Y_ZERO', Y_ZERO, STATUS)
	x_zero = x_zero+0.5
	y_zero = y_zero+0.5

*      get option to plot small or full ticks

	CALL PAR_GET0C( 'CONTOUR_TICKLEN', TICK_LENGTH, STATUS)

* get the scale factor for the ticks and whether want inside or out

	CALL PAR_GET0R( 'TICK_SCALE', TICKSCALE, STATUS)
	CALL PAR_GET0C( 'TICK_INOUT', TICK_INOUT, STATUS)
        CALL CHR_UCASE( TICK_INOUT)

* initialize local variables

	REAL_NX = FLOAT( NX)
	REAL_NY = FLOAT( NY)

	IF( COSDELTA .GT. 0.0) THEN
	  TICK_XINTERVAL = TICK_XINTERVAL*COSDELTA
	ELSE
	  TICK_XINTERVAL = TICK_XINTERVAL*0.001
	END IF

	NUMBER_X_TICKS = IFIX( REAL_NX/TICK_XINTERVAL)
	NUMBER_Y_TICKS = IFIX( REAL_NY/TICK_YINTERVAL)

* loop for both X axes tick marks

	J = 1
	X1 = 0

	DO WHILE ( X1 .LT. (IM_XEN-TICK_XINTERVAL*CONTOUR_MAGNIF))

* plot bottom X axis tick marks

	  X1 = IM_XST +
     :          ( X_ZERO + TICK_XINTERVAL*(J-1))*CONTOUR_MAGNIF
	  Y1 = IM_YST

	  IF( TICK_LENGTH .NE. 'FULL') THEN
	    IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	      Y2 = IM_YST + REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE
            ELSE
	      Y2 = IM_YST - REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE
            END IF
	  ELSE
	    Y2 = IM_YEN
	  END IF

	  CALL SGS_LINE( X1, Y1, X1, Y2)

* plot top X axis tick mark at same X position

	  Y1 = IM_YEN

	  IF( TICK_LENGTH .NE. 'FULL') THEN
	    IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	      Y2 = IM_YEN - REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE
            ELSE
	      Y2 = IM_YEN + REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE
            END IF
	  ELSE
	    Y2 = IM_YST
	  END IF

	  CALL SGS_LINE( X1, Y1, X1, Y2)

* increment tick mark number

	  J = J + 1
	END DO

* loop for both Y axis tick marks

	J = 1
	Y1 = 0

	DO WHILE ( Y1 .LT.
     :	         ( IM_YEN-TICK_YINTERVAL*CONTOUR_MAGNIF*CONTOUR_AXRAT))

* plot left Y axis tick marks

	  X1 = IM_XST

	  IF( TICK_LENGTH .NE. 'FULL') THEN
	    IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	      X2 = IM_XST + REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE
            ELSE
	      X2 = IM_XST - REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE
            END IF
	  ELSE
	    X2 = IM_XEN
	  END IF

	  Y1 = IM_YST +
     :	       ( Y_ZERO +
     :           TICK_YINTERVAL*(J-1))*CONTOUR_MAGNIF*CONTOUR_AXRAT

	  CALL SGS_LINE( X1, Y1, X2, Y1)

* plot right Y axis tick marks

	  X1 = IM_XEN

	  IF( TICK_LENGTH .NE. 'FULL') THEN
	    IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	      X2 = IM_XEN - REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE
            ELSE
	      X2 = IM_XEN + REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE
            END IF
	  ELSE
	    X2 = IM_XST
	  END IF

	  CALL SGS_LINE( X1, Y1, X2, Y1)

* increment the number of tick mark

	  J = J + 1

	END DO

* loop for both X axes tick marks negative from zero point

	IF( X_ZERO .GT. 0.0) THEN

	  J = 1
	  X1 = IM_XEN

	  DO WHILE ( X1 .GT. (IM_XST+TICK_XINTERVAL*CONTOUR_MAGNIF))

* plot bottom X axis tick marks

	    X1 = IM_XST +
     :           ( X_ZERO - TICK_XINTERVAL*(J-1))*CONTOUR_MAGNIF
	    Y1 = IM_YST

	    IF( TICK_LENGTH .NE. 'FULL') THEN
	      IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	        Y2 = IM_YST + REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE
              ELSE
	        Y2 = IM_YST - REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE
              END IF
	    ELSE
	      Y2 = IM_YEN
	    END IF

	    CALL SGS_LINE( X1, Y1, X1, Y2)

* plot top X axis tick mark at same X position

	    Y1 = IM_YEN

	    IF( TICK_LENGTH .NE. 'FULL') THEN
	      IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	        Y2 = IM_YEN - REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE
              ELSE
	        Y2 = IM_YEN + REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE
              END IF
	    ELSE
	      Y2 = IM_YST
	    END IF

	    CALL SGS_LINE( X1, Y1, X1, Y2)

* increment tick mark number

	    J = J + 1

	  END DO

	END IF

* loop for both Y axis tick marks negative from zero

	IF( Y_ZERO .GT. 0.0) THEN

	  J = 1
	  Y1 = IM_YEN

	  DO WHILE ( Y1 .GT.
     :	           ( IM_YST+
     :              TICK_YINTERVAL*CONTOUR_MAGNIF*CONTOUR_AXRAT))

* plot left Y axis tick marks

	    X1 = IM_XST

	    IF( TICK_LENGTH .NE. 'FULL') THEN

	      IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	        X2 = IM_XST + REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE
              ELSE
	        X2 = IM_XST - REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE
              END IF

	    ELSE

	      X2 = IM_XEN

	    END IF

	    Y1 = IM_YST +
     :	         ( Y_ZERO -
     :           TICK_YINTERVAL*(J-1))*CONTOUR_MAGNIF*CONTOUR_AXRAT

	    CALL SGS_LINE( X1, Y1, X2, Y1)

* plot right Y axis tick marks

	    X1 = IM_XEN

	    IF( TICK_LENGTH .NE. 'FULL') THEN

	      IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	        X2 = IM_XEN - REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE
              ELSE
	        X2 = IM_XEN + REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE
              END IF

	    ELSE

	      X2 = IM_XST

	    END IF

	    CALL SGS_LINE( X1, Y1, X2, Y1)

* increment the number of tick mark

	    J = J + 1

	  END DO

	END IF

	IF( COSDELTA .GT. 0.0) THEN

	  TICK_XINTERVAL = TICK_XINTERVAL/COSDELTA

	ELSE

	  TICK_XINTERVAL = TICK_XINTERVAL/0.001

	END IF

* empty plot buffer of remaining graphics

	CALL SGS_FLUSH

	END
