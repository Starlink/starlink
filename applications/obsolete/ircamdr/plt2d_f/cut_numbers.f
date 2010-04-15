	SUBROUTINE CUT_NUMBERS( X_LO, X_HI, Y_LO, Y_HI, XST, XEN, YST, YEN,
     :	                        CUT_MAGNIF, CUT_AXISRAT, XTICK_START,
     :	                        XTICK_INTERVAL, CUT_NUMYTIC, CUT_NUMXTIC,
     :	                        STATUS)

* Description : Routine to write numbers on the X and Y axes of a CUT plot

* Invocation :

* Parameters :

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
* 25-05-86 : REVA::CAA : added the histogram plot number option
* 20-JUL-1994 SKL@JACH Changed STR$ call to CHR_ and IFIX to INT
* 26-Oct-1994 SKL@JACH Changed MAGNIF from INT to REAL
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'
        INCLUDE 'SAE_PAR'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'
        INCLUDE 'CHR_ERR'

* Status :

	INTEGER STATUS

* Import :

	INTEGER
     :	  CUT_NUMXTIC,
     :	  CUT_NUMYTIC

	REAL
     :    CUT_MAGNIF,
     :	  CUT_AXISRAT,
     :	  XEN,
     :	  X_HI,
     :	  X_LO,
     :	  XTICK_INTERVAL,
     :	  XTICK_START,
     :	  XST,
     :	  YEN,
     :	  Y_HI,
     :	  Y_LO,
     :	  YST

* Import-Export :

* Export :

* External references :

* Global variables

	INCLUDE 'PLT2DCOM'

* Local Constants :

* Local variables :

	INTEGER
     :	  FONT,
     :	  J,
     :	  NPOINT,
     :	  NUMB_BEF,
     :	  NUMB_DIGITS,
     :	  PREC,
     :	  YHIEXP

	REAL
     :	  DXTICK_INTERVAL,
     :	  DXTICK_START,
     :	  TEXT_HT,
     :	  X1,
     :	  X_NUMBER,
     :	  Y_NUMBER,
     :	  Y1,
     :	  YTICK_INTERVAL,
     :	  YTICK_START

	CHARACTER
     :	  CUT_HISTOGRAM*20,
     :	  CUT_YNUMORI*10

* test status and return if no ok

	IF( STATUS .NE. SAI__OK) THEN

	  RETURN

	END IF

* get the flag to indicate whether this plot is a histogram or image cut

	CALL PAR_GET0C( 'CUT_HISTOGRAM', CUT_HISTOGRAM, STATUS)

* get the font code and the precision for plotting

	CALL PAR_GET0I( 'COMMENT_FONT', FONT, STATUS)

	CALL PAR_GET0I( 'COMMENT_PREC', PREC, STATUS)

* set the font and precision

	CALL GSTXFP( FONT, PREC)

* set text height

	TEXT_HT = ( MAX_Y/40.0)*( CUT_MAGNIF/16.0)

	CALL SGS_SHTX( TEXT_HT)

* set X number positioning to Centre in X Top in Y

	CALL SGS_STXJ( 'CC')

* get the option to change orientation of Y-axis numbers

	CALL PAR_GET0C( 'CUT_YNUMORI', CUT_YNUMORI, STATUS)

	CALL CHR_UCASE( CUT_YNUMORI )

* test if this is an image cut or a histogram plot

	IF( CUT_HISTOGRAM .EQ. 'YES') THEN

* JUST DO SAME AS IMAGE CUT AT THE MOMENT BUT THIS WILL NEED TO BE CHANGED
* FOR A REAL HISTOGRAM WITH BIN NUMBERS ALONG BOTTOM AXIS

* calculate Y axis tick marks start and interval in device coordinates

	  YTICK_START = YST

	  YTICK_INTERVAL = ( YEN - YST)/CUT_NUMYTIC

* calculate number of X axis tick marks

	  CUT_NUMXTIC = INT( ( X_HI - XTICK_START)/XTICK_INTERVAL) + 1

* scale the X tick start and interval parameters to device coordinates

	  DXTICK_START = ( XTICK_START - X_LO)/( X_HI - X_LO)*( XEN - XST)

	  DXTICK_INTERVAL = XTICK_INTERVAL/( X_HI - X_LO)*( XEN - XST)

* loop for X axis histogram numbers

	  DO J = 1, CUT_NUMXTIC

	    X1 = XST + DXTICK_START + DXTICK_INTERVAL*( J - 1)
	    Y1 = YST - ABS( YEN - YST)/30.0

	    X_NUMBER = XTICK_START + XTICK_INTERVAL*(J - 1)

	    IF( ABS( X_NUMBER) .LT. 1.0E-5) X_NUMBER = 1.0E-5

	    NUMB_BEF = INT( LOG10( ABS( X_NUMBER)))
	    NUMB_DIGITS = NUMB_BEF + 3

	    IF( X_NUMBER .LT. 0.0) NUMB_DIGITS = NUMB_DIGITS + 1

* plot bottom X axis number

	    IF( X1 .GE. XST .AND. X1 .LE. XEN) THEN

	      CALL SGS_BTEXT( X1, Y1)

	      CALL SGS_ATXR( X_NUMBER, -NUMB_DIGITS, 1)

	    END IF

	  END DO

* here if this is an image cut

	ELSE

* calculate Y axis tick marks start and interval in device coordinates

	  YTICK_START = YST

	  YTICK_INTERVAL = ( YEN - YST)/CUT_NUMYTIC

* calculate number of X axis tick marks

	  CUT_NUMXTIC = INT( ( X_HI - XTICK_START)/XTICK_INTERVAL) + 1

* scale the X tick start and interval parameters to device coordinates

	  DXTICK_START = ( XTICK_START - X_LO)/( X_HI - X_LO)*( XEN - XST)

	  DXTICK_INTERVAL = XTICK_INTERVAL/( X_HI - X_LO)*( XEN - XST)

* loop for X axis numbers

	  DO J = 1, CUT_NUMXTIC

	    X1 = XST + DXTICK_START + DXTICK_INTERVAL*( J - 1)
	    Y1 = YST - ABS( YEN - YST)/30.0

	    X_NUMBER = XTICK_START + XTICK_INTERVAL*(J - 1)

	    IF( ABS( X_NUMBER) .LT. 1.0E-5) X_NUMBER = 1.0E-5

	    NUMB_BEF = INT( LOG10( ABS( X_NUMBER)))
	    NUMB_DIGITS = NUMB_BEF + 3

	    IF( X_NUMBER .LT. 0.0) NUMB_DIGITS = NUMB_DIGITS + 1

* plot bottom X axis number

	    IF( X1 .GE. XST .AND. X1 .LE. XEN) THEN

	      CALL SGS_BTEXT( X1, Y1)

	      CALL SGS_ATXR( X_NUMBER, -NUMB_DIGITS, 1)

	    END IF

	  END DO

	END IF

* test option to specify Y axis number orientation

	IF( CUT_YNUMORI .EQ. 'UP') THEN

	  CALL SGS_SUPTX( -1.0, 0.0)

	ELSE

	  CALL SGS_SUPTX( 0.0, 1.0)

	END IF

* set Y number positioning to Centre in X Centre in Y

	CALL SGS_STXJ( 'CC')

* calculate number of exponents in Y_HI

	IF( ABS( Y_HI) .LT. 1.0E-5) Y_HI = 1.0E-5

	YHIEXP = INT( ALOG10( ABS( Y_HI)))

* loop for both Y axis numbers

	IF( CUT_YNUMORI .EQ. 'UP') THEN

	  X1 = XST - ( XEN - XST)/35.0

	ELSE

	  X1 = XST - ( XEN - XST)/14.0

	END IF

	DO J = 1, ( CUT_NUMYTIC + 1)

	  Y1 = YTICK_START + YTICK_INTERVAL*(J - 1)

	  Y_NUMBER = Y_LO + ( Y_HI - Y_LO)/CUT_NUMYTIC*(J - 1)
	  Y_NUMBER = Y_NUMBER/( 10**YHIEXP)

* plot left Y axis number

	  CALL SGS_BTEXT( X1, Y1)

	  IF( Y_NUMBER .LT. 0.0) THEN

	    CALL SGS_ATXR( Y_NUMBER, -6, 3)

	  ELSE

	    CALL SGS_ATXR( Y_NUMBER, -5, 3)

	  END IF

	END DO

* test option to reset Y axis number orientation

	IF( CUT_YNUMORI .EQ. 'UP') THEN

	  CALL SGS_SUPTX( 0.0, 1.0)

	END IF

* set Y number positioning to LEFT in X Centre in Y

	CALL SGS_STXJ( 'CL')

* reduce text height for exponent

	TEXT_HT = TEXT_HT*0.85

	CALL SGS_SHTX( TEXT_HT)

* plot exponent at top in Y

	X1 = XST + ( XEN - XST)*0.05
	Y1 = YEN - ( YEN - YST)*0.05

	CALL SGS_BTEXT( X1, Y1)
	CALL SGS_ATXL( 'x')
	CALL SGS_ATXI( 10, -2)
	CALL SGS_ATXL( '**')

	IF( ABS( YHIEXP) .LT. 10) THEN

	  NPOINT = -1

	ELSE IF( ABS( YHIEXP) .GE. 10 .AND. ABS( YHIEXP) .LT. 100) THEN

	  NPOINT = -2

	ELSE IF( ABS( YHIEXP) .GE. 100 .AND. ABS( YHIEXP) .LT. 1000) THEN

	  NPOINT = -3

	ELSE

	  NPOINT = -4

	END IF

	IF( YHIEXP .LT. 0) NPOINT = NPOINT - 1

	CALL SGS_ATXI( YHIEXP, NPOINT)

* set Y number positioning to Centre in X Centre in Y

	CALL SGS_STXJ( 'CC')

* empty plot buffer of remaining graphics

	CALL SGS_FLUSH

	END
