	SUBROUTINE CUT_TICKS( X_LO, X_HI, Y_LO, Y_HI, XST, XEN, YST, YEN,
     :	                      CUT_MAGNIF, XTICK_START, XTICK_INTERVAL,
     :	                      CUT_NUMYTIC, CUT_NUMXTIC, STATUS)

* Description : Subroutine to plot tick marks along the axis of a CUT/SLICE
*               plot

* History
* 26-Oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
*

* ==========================================================================

	IMPLICIT NONE

	INCLUDE 'ADAM_DEFNS'
	INCLUDE 'SAE_PAR'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

	INCLUDE 'PLT2DCOM'

* define local variables

	INTEGER
     :	  CUT_NUMXTIC,
     :	  CUT_NUMYTIC,
     :	  J,
     :	  STATUS

	REAL
     :    CUT_MAGNIF,
     :	  DXTICK_INTERVAL,
     :	  DXTICK_START,
     :	  XEN,
     :	  X_HI,
     :	  X_LO,
     :	  XST,
     :	  XTICK_INTERVAL,
     :	  XTICK_START,
     :	  X1,
     :	  X2,
     :	  YEN,
     :	  Y_HI,
     :	  Y_LO,
     :	  YST,
     :	  YTICK_INTERVAL,
     :	  YTICK_START,
     :	  Y1,
     :	  Y2

* ======================================================================

* test status

	IF( STATUS .NE. SAI__OK) THEN

	  RETURN

	END IF

* calculate Y axis tick marks start and interval in device coordinates

	YTICK_START = YST

	YTICK_INTERVAL = ( YEN - YST)/CUT_NUMYTIC

* calculate number of X axis tick marks

	CUT_NUMXTIC = IFIX( ( X_HI - XTICK_START)/XTICK_INTERVAL) + 1

* scale the X tick start and interval parameters to device coordinates

	DXTICK_START = ( XTICK_START - X_LO)/( X_HI - X_LO)*( XEN - XST)

	DXTICK_INTERVAL = XTICK_INTERVAL/( X_HI - X_LO)*( XEN - XST)

* loop for all X tick marks

	DO J = 1, CUT_NUMXTIC

* bottom X axis tick marks

	  X1 = XST + DXTICK_START + DXTICK_INTERVAL*(J - 1)
	  Y1 = YST
	  Y2 = YST + ABS( YEN - YST)/50.0

* plot tick only if within cut box

	  IF( X1 .GE. XST .AND. X1 .LE. XEN) THEN

	    CALL SGS_LINE( X1, Y1, X1, Y2)

	  END IF

* top X axis tick mark at same X position

	  X1 = XST + DXTICK_START + DXTICK_INTERVAL*(J - 1)
	  Y1 = YEN
	  Y2 = YEN - ABS( YEN - YST)/50.0

* plot tick only if within cut box

	  IF( X1 .GE. XST .AND. X1 .LE. XEN) THEN

	    CALL SGS_LINE( X1, Y1, X1, Y2)

	  END IF

	END DO

* loop for both Y axis tick marks

	DO J = 1, CUT_NUMYTIC

* left Y axis tick marks

	  X1 = XST
	  X2 = XST + ABS( XEN - XST)/50.0
	  Y1 = YTICK_START + YTICK_INTERVAL*(J - 1)

* plot tick mark only if within cut box

	  IF( Y1 .GE. YST .AND. Y1 .LE. YEN) THEN

	    CALL SGS_LINE( X1, Y1, X2, Y1)

	  END IF

* right Y axis tick marks

	  X1 = XEN
	  X2 = XEN - ABS( XEN - XST)/50.0
	  Y1 = YTICK_START + YTICK_INTERVAL*(J - 1)

* plot tick mark only if within cut box

	  IF( Y1 .GE. YST .AND. Y1 .LE. YEN) THEN

	    CALL SGS_LINE( X1, Y1, X2, Y1)

	  END IF

	END DO

* empty plot buffer of remaining graphics

	CALL SGS_FLUSH

	END
