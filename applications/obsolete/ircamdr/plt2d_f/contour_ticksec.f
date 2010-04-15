	SUBROUTINE CONTOUR_TICKSEC( DIVISOR, TICK_XINTERVAL,
     :                              TICK_YINTERVAL,
     :	                            CONTOUR_MAGNIF, CONTOUR_AXRAT,
     :	                            STATUS)

* Description : Plots tick marks on the axes formed by an image border
*               at user definable intervals and starting from a 0,0 position

* History
*   26thOct94 Changed MAGNIF from INT to REAL (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'PLT2DCOM'

* define local variables

	REAL CONTOUR_MAGNIF
	INTEGER J
	INTEGER NUMBER_X_TICKS
	INTEGER NUMBER_Y_TICKS
	INTEGER STATUS

	REAL CONTOUR_AXRAT
	REAL COSDELTA
	REAL DEC
	REAL DIVISOR
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

	CHARACTER*20 TICK_INOUT
	CHARACTER*20 NUMBER_TYPE

	COSDELTA = 0.0

*     get the number type from interface

	CALL PAR_GET0C( 'NUMBER_TYPE', NUMBER_TYPE, STATUS)

*      test if want ra/dec numbers and if so get zero position values

	IF( NUMBER_TYPE .EQ. 'RA_DEC') THEN

	  CALL PAR_GET0R( 'DEC_ZERO', DEC, STATUS)

	  COSDELTA = COS( ABS( DEC)*3.1415927/180.0)

	END IF

* get the X and Y zero point for first tick mark

	CALL PAR_GET0R( 'X_ZERO', X_ZERO, STATUS)
	CALL PAR_GET0R( 'Y_ZERO', Y_ZERO, STATUS)
	x_zero = x_zero + 0.5
	y_zero = y_zero + 0.5

* get the scale factor for the ticks and whether want inside or outside

	CALL PAR_GET0R( 'TICK_SCALE', TICKSCALE, STATUS)
	CALL PAR_GET0C( 'TICK_INOUT', TICK_INOUT, STATUS)
	CALL CHR_UCASE( TICK_INOUT)

* initialize local variables

	REAL_NX = FLOAT( NX)

	REAL_NY = FLOAT( NY)

	IF( NUMBER_TYPE .EQ. 'RA_DEC') THEN

	  IF( COSDELTA .GT. 0.0) THEN

	    TICK_XINTERVAL = TICK_XINTERVAL*COSDELTA

	  ELSE

	    TICK_XINTERVAL = TICK_XINTERVAL*0.001

	  END IF

	END IF

	NUMBER_X_TICKS = IFIX( REAL_NX/TICK_XINTERVAL)

	NUMBER_Y_TICKS = IFIX( REAL_NY/TICK_YINTERVAL)

* loop for both X axes tick marks

	J = 1

	X1 = 0

	DO WHILE ( X1 .LT. (IM_XEN-TICK_XINTERVAL*CONTOUR_MAGNIF))

* plot bottom X axis tick marks

	  X1 = IM_XST + ( X_ZERO + TICK_XINTERVAL*(J-1))*CONTOUR_MAGNIF

	  Y1 = IM_YST

	  IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	    Y2 = IM_YST + REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	  ELSE
	    Y2 = IM_YST - REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	  END IF

	  CALL SGS_LINE( X1, Y1, X1, Y2)

* plot top X axis tick mark at same X position

	  Y1 = IM_YEN

	  IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	    Y2 = IM_YEN - REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	  ELSE
	    Y2 = IM_YEN + REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
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

	  IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	    X2 = IM_XST + REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	  ELSE
	    X2 = IM_XST - REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	  END IF

	  Y1 = IM_YST +
     :	       ( Y_ZERO +
     :         TICK_YINTERVAL*(J-1))*CONTOUR_MAGNIF*CONTOUR_AXRAT

	  CALL SGS_LINE( X1, Y1, X2, Y1)

* plot right Y axis tick marks

	  X1 = IM_XEN

	  IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	    X2 = IM_XEN - REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	  ELSE
	    X2 = IM_XEN + REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
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

	    X1 = IM_XST + ( X_ZERO - TICK_XINTERVAL*(J-1))*CONTOUR_MAGNIF

	    Y1 = IM_YST

	    IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	      Y2 = IM_YST + REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	    ELSE
	      Y2 = IM_YST - REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	    END IF

	    CALL SGS_LINE( X1, Y1, X1, Y2)

* plot top X axis tick mark at same X position

	    Y1 = IM_YEN

	    IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	      Y2 = IM_YEN - REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	    ELSE
	      Y2 = IM_YEN + REAL_NY*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
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
     :	           ( IM_YST+TICK_YINTERVAL*CONTOUR_MAGNIF*CONTOUR_AXRAT))

* plot left Y axis tick marks

	    X1 = IM_XST

	    IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	      X2 = IM_XST + REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	    ELSE
	      X2 = IM_XST - REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	    END IF

	    Y1 = IM_YST +
     :	         ( Y_ZERO
     :            - TICK_YINTERVAL*(J-1))*CONTOUR_MAGNIF*CONTOUR_AXRAT

	    CALL SGS_LINE( X1, Y1, X2, Y1)

* plot right Y axis tick marks

	    X1 = IM_XEN

	    IF( TICK_INOUT( 1:1) .NE. 'O') THEN
	      X2 = IM_XEN - REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	    ELSE
	      X2 = IM_XEN + REAL_NX*CONTOUR_MAGNIF/40.0/TICKSCALE/DIVISOR
	    END IF

	    CALL SGS_LINE( X1, Y1, X2, Y1)

* increment the number of tick mark

	    J = J + 1

	  END DO

	END IF

	IF( NUMBER_TYPE .EQ. 'RA_DEC') THEN

	  IF( COSDELTA .GT. 0.0) THEN

	    TICK_XINTERVAL = TICK_XINTERVAL/COSDELTA

	  ELSE

	    TICK_XINTERVAL = TICK_XINTERVAL/0.001

	  END IF

	END IF

* empty plot buffer of remaining graphics

	CALL SGS_FLUSH

	END
