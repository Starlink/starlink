	SUBROUTINE CONTOUR_NUMBERS( NUMBER_XINTERVAL,
     :	                            NUMBER_YINTERVAL, CONTOUR_MAGNIF,
     :                              CONTOUR_AXRAT, STATUS)

* Description : Routine to write numbers on contour map. Numbers are either
*               offsets from a zero point or RA and DEC positions

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*   20-JUL-1994 Changed LIB$ calls to CHR_ and IFIX to INT (SKL@JACH)
*   26-Oct-1994 Changed MAGNIF from INT to REAL (SKL@JACH)
*
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

	REAL CONTOUR_MAGNIF

	REAL CONTOUR_AXRAT
	REAL NUMBER_XINTERVAL
	REAL NUMBER_YINTERVAL

* Import-Export :

* Export :

* External references :

* Global variables

	INCLUDE 'PLT2DCOM'

* Local Constants :

* Local variables :

	INTEGER FONT
	INTEGER J
	INTEGER NUMPTY
	INTEGER K
	INTEGER L1
	INTEGER NF
	INTEGER NPR
	INTEGER NUMB_CHAR
	INTEGER NUMBER_X_TICKS
	INTEGER NUMBER_Y_TICKS
	INTEGER POS_DEC( 5)
	INTEGER POS_RA( 5)
	INTEGER PREC
	INTEGER ST_EN
	INTEGER ST_ST

	REAL AR
	REAL ARCSEC_PIXEL
	REAL COSDELTA
	REAL DEC
	REAL HM
	REAL HT
	REAL LENTEXT
	REAL XSTTEXT
	REAL YSTTEXT
	REAL RA
	REAL SP
	REAL TEXT_HT
	REAL TEXT_HTS
	REAL X1
	REAL X2
	REAL XLIMIT
	REAL X_NUMBER
	REAL XOFF
	REAL XU
	REAL X_ZERO
	REAL Y1
	REAL Y2
	REAL YLIMIT
	REAL Y_NUMBER
	REAL YOFF
	REAL YU
	REAL Y_ZERO
	REAL NUMSCALE
	REAL NUMMULT

	CHARACTER*20 NUMBER_ORIENT
	CHARACTER*30 NUMBER_TYPE
	CHARACTER*20 NUMTYPE_DEC
	CHARACTER*20 NUMTYPE_RA
	CHARACTER*20 NUMBER_SIGN
	CHARACTER*80 STRING_RA
	CHARACTER*80 STRING_DEC
	CHARACTER*80 STRING
	CHARACTER*1 SUPERSCRIPT
	CHARACTER*2 TXJ

* Internal References :

* ============================================================================

*      get the X,Y number start pixels

	CALL PAR_GET0R( 'X_ZERO', X_ZERO, STATUS)
	CALL PAR_GET0R( 'Y_ZERO', Y_ZERO, STATUS)
	x_zero = x_zero+0.5
	y_zero = y_zero+0.5

*      get the orientation of the numbers on Y axis

	CALL PAR_GET0C( 'NUMBER_ORIENT', NUMBER_ORIENT, STATUS)

*      get option to plot offset arcseconds/pixels or ra and dec

	CALL PAR_GET0C( 'NUMBER_TYPE', NUMBER_TYPE, STATUS)

*      get zero position of ra and dec

	CALL PAR_GET0R( 'RA_ZERO', RA, STATUS)
	CALL PAR_GET0R( 'DEC_ZERO', DEC, STATUS)

*      calculate the cosine declination for spacing of DEC ticks

	IF( NUMBER_TYPE .EQ. 'RA_DEC') THEN
	  COSDELTA = COS( ABS( DEC)*3.141592654/180.0)
	  IF( COSDELTA .GT. 0.0) THEN
	    NUMBER_XINTERVAL = NUMBER_XINTERVAL*COSDELTA
	  ELSE
	    NUMBER_XINTERVAL = NUMBER_XINTERVAL*0.001
	  END IF
	ELSE
	  COSDELTA = 1.0
	END IF

*      get the number sign convention to be used

	CALL PAR_GET0C( 'NUMBER_SIGN', NUMBER_SIGN, STATUS)

	IF( NUMBER_SIGN .NE. 'ASTRO') THEN
	  NUMMULT = 1.0
	ELSE
	  NUMMULT = -1.0
	END IF

*      set text height

	TEXT_HT  = 12.0*CONTOUR_MAGNIF/6.0*NY/64*CONTOUR_AXRAT
	TEXT_HTS = 12.0*CONTOUR_MAGNIF/6.0*NY/64*CONTOUR_AXRAT/2.0

*      get the number size scaler and divide by it

	CALL PAR_GET0R( 'NUM_SCALE', NUMSCALE, STATUS)

	TEXT_HT  = TEXT_HT/NUMSCALE
	TEXT_HTS = TEXT_HTS/NUMSCALE

*      set text height

	CALL SGS_SHTX( TEXT_HT)

*      set text positioning on centre in Y and centre in X

	CALL SGS_STXJ( 'CC')

*      get text attributes from SGS

	CALL SGS_IMTX( HM, NF, NPR, HT, AR, XU, YU, SP, TXJ)

* get the font code and the precision for plotting

	CALL PAR_GET0I( 'COMMENT_FONT', FONT, STATUS)
	CALL PAR_GET0I( 'COMMENT_PREC', PREC, STATUS)

* set the font and precision

	CALL GSTXFP( FONT, PREC)

*      get arcseconds / pixel from parameter system

	CALL PAR_GET0R( 'ARCSEC_PIXEL', ARCSEC_PIXEL, STATUS)

*      calculate number tick marks = number numbers

	NUMBER_X_TICKS = INT( NX/NUMBER_XINTERVAL)
	NUMBER_Y_TICKS = INT( NY/NUMBER_YINTERVAL)

*      set the default text orientation

	CALL SGS_SUPTX( 0.0, 1.0)

* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*      loop for both X axis numbers NEGATIVE from zero point

	J = 1

	X1 = 0
	Y1 = IM_YST - 1.3*TEXT_HT

	XLIMIT = IM_XEN - NUMBER_XINTERVAL*CONTOUR_MAGNIF

	DO WHILE ( X1 .LE. XLIMIT)

	  X1 = IM_XST + ( X_ZERO +
     :         NUMBER_XINTERVAL*(J-1))*CONTOUR_MAGNIF

*        test if the user wants offsets or ra and dec positions

	  IF( NUMBER_TYPE .EQ. 'RA_DEC') THEN

* RA-RIGHT

*          numbers are ra/dec positions, calculate the values for this position

	    CALL RADEC_STRING( J, RA, DEC, 'POSITIVE',
     :	                       NUMBER_XINTERVAL, NUMBER_YINTERVAL,
     :                         ARCSEC_PIXEL, NUMTYPE_RA,
     :	                       NUMTYPE_DEC, POS_RA, POS_DEC,
     :	                       STRING_RA, STRING_DEC)

!            print *, 'string_ra = ', string_ra
            CALL CHR_CLEAN( STRING_RA )
            L1 = 0
	    CALL CHR_APPND( STRING_RA, STRING_RA, L1)

	    CALL SGS_BTEXT( X1, Y1)
	    CALL SGS_ATXL( STRING_RA( 1:L1))

*          set the text height for the superscripts

	    CALL SGS_SHTX( TEXT_HTS)

*          calculate HALF the length of the text string

	    LENTEXT = L1*TEXT_HT/2.0

*          calculate the start position of the text string

	    XSTTEXT = X1 - LENTEXT

*          test if the number to be plotted is a FULL RA or PARTIAL RA

	    IF( NUMTYPE_RA .EQ. 'FULL') THEN

*            if number characters even then add half a character to position

	      XOFF = ( POS_RA( 1) - 1)*TEXT_HT + 0.5*TEXT_HT

*            calculate the position of the superscript for h in RA

	      X2 = XSTTEXT + XOFF
	      Y2 = Y1 + 0.5*TEXT_HT

*            plot the character

	      CALL SGS_BTEXT( X2,Y2)
	      CALL SGS_ATXL( 'h')

	    END IF

	    XOFF = ( POS_RA( 2) - 1)*TEXT_HT + 0.5*TEXT_HT

*          calculate the position of the superscript for m in RA

	    X2 = XSTTEXT + XOFF
	    Y2 = Y1 + 0.5*TEXT_HT

*          plot the character

	    CALL SGS_BTEXT( X2,Y2)
	    CALL SGS_ATXL( 'm')

	    XOFF = ( POS_RA( 3) - 1)*TEXT_HT + 0.5*TEXT_HT

*          calculate the position of the superscript for s in RA

	    X2 = XSTTEXT + XOFF
	    Y2 = Y1 + 0.5*TEXT_HT

*          plot the character

	    CALL SGS_BTEXT( X2,Y2)
	    CALL SGS_ATXL( 's')

*          reset the text height to the original for main text

	    CALL SGS_SHTX( TEXT_HT)

	  ELSE

* OFFSETS-X-RIGHT

*          numbers are offsets in arcsecs

	    X_NUMBER = NUMMULT*NUMBER_XINTERVAL*(J-1)*ARCSEC_PIXEL

	    Y1 = IM_YST - 1.3*TEXT_HT

*          convert number to string for length test

            CALL CHR_ITOC( INT( X_NUMBER), STRING, NUMB_CHAR )

*          find number of digits in string i.e. number

	    ST_ST = 0
	    ST_EN = 0

	    DO K = 1, LEN( STRING)

*            get start of string

	      IF( ST_ST .EQ. 0 .AND. STRING( J:J) .NE. ' ' .AND.
     :	          STRING( K:K) .NE. CHAR( 0)) THEN

	        ST_ST = K

	      END IF

*            get end of string

	      IF( STRING( K:K) .NE. ' ' .AND.
     :	          STRING( K:K) .NE. CHAR( 0)) THEN

	        ST_EN = K

	      END IF

	    END DO

*          calculate number of characters in number

	    NUMB_CHAR = ST_EN - ST_ST + 1

*          plot bottom X axis number

	    CALL SGS_BTEXT( X1, Y1)

	    CALL SGS_ATXR( X_NUMBER, -NUMB_CHAR-2, 1)

	  END IF

	  J = J + 1

	END DO

*      set text positioning on centre in Y and centre in X

	CALL SGS_STXJ( 'CC')

	CALL SGS_FLUSH

* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*      loop for both X axis numbers POSITIVE from zero point

	J = 1

	X1 = IM_XEN
	Y1 = IM_YST - 1.3*TEXT_HT

	XLIMIT = IM_XST + NUMBER_XINTERVAL*CONTOUR_MAGNIF

	DO WHILE ( X1 .GT. XLIMIT)

	  X1 = IM_XST + ( X_ZERO - NUMBER_XINTERVAL*(J-1))*CONTOUR_MAGNIF

	  IF( NUMBER_TYPE .EQ. 'RA_DEC') THEN

* RA-LEFT

*          numbers are ra/dec positions, calculate the values for this position

	    CALL RADEC_STRING( J, RA, DEC, 'NEGATIVE',
     :	                       NUMBER_XINTERVAL, NUMBER_YINTERVAL,
     :                         ARCSEC_PIXEL, NUMTYPE_RA,
     :	                       NUMTYPE_DEC, POS_RA, POS_DEC,
     :	                       STRING_RA, STRING_DEC)

*          if this is the first negative number then don't plot anything
*          since first positive will have done it

	    IF( J .EQ. 1) THEN
	      STRING_RA = ' '
	    END IF

!            print *, 'string_ra = ', string_ra
            CALL CHR_CLEAN( STRING_RA )
            L1 = 0
	    CALL CHR_APPND( STRING_RA, STRING_RA, L1)

	    CALL SGS_BTEXT( X1, Y1)
	    CALL SGS_ATXL( STRING_RA( 1:L1))

*          calculate HALF the length of the text string

	    LENTEXT = L1*TEXT_HT/2.0

*          calculate the start position of the text string

	    XSTTEXT = X1 - LENTEXT

*          test if this is a j number greater than 1

	    IF( J .GT. 1) THEN

*            set the text height for the superscripts

	      CALL SGS_SHTX( TEXT_HTS)

*            test if the number to be plotted is a FULL RA or PARTIAL RA

	      IF( NUMTYPE_RA .EQ. 'FULL') THEN

	        XOFF = ( POS_RA( 1) - 1)*TEXT_HT + 0.5*TEXT_HT

*              calculate the position of the superscript for h in RA

	        X2 = XSTTEXT + XOFF
	        Y2 = Y1 + 0.5*TEXT_HT

*              plot the character

	        CALL SGS_BTEXT( X2,Y2)
	        CALL SGS_ATXL( 'h')

	      END IF

	      XOFF = ( POS_RA( 2) - 1)*TEXT_HT + 0.5*TEXT_HT

*            calculate the position of the superscript for m in RA

	      X2 = XSTTEXT + XOFF
	      Y2 = Y1 + 0.5*TEXT_HT

*            plot the character

	      CALL SGS_BTEXT( X2,Y2)
	      CALL SGS_ATXL( 'm')

	      XOFF = ( POS_RA( 3) - 1)*TEXT_HT + 0.5*TEXT_HT

*            calculate the position of the superscript for s in RA

	      X2 = XSTTEXT + XOFF
	      Y2 = Y1 + 0.5*TEXT_HT

*            plot the character

	      CALL SGS_BTEXT( X2,Y2)
	      CALL SGS_ATXL( 's')

*            reset the text height to the original for main text

	      CALL SGS_SHTX( TEXT_HT)

	    END IF

	  ELSE

* OFFSETS-X-LEFT

	    X_NUMBER = -1.0*NUMMULT*NUMBER_XINTERVAL*(J-1)*
     :                  ARCSEC_PIXEL

*          convert number to string for length test

            CALL CHR_ITOC( INT( X_NUMBER), STRING, NUMB_CHAR )

*          find number of digits in string i.e. number

	    ST_ST = 0
	    ST_EN = 0

	    DO K = 1, LEN( STRING)

*            get start of string

	      IF( ST_ST .EQ. 0 .AND. STRING( J:J) .NE. ' ' .AND.
     :	          STRING( K:K) .NE. CHAR( 0)) THEN

	        ST_ST = K

	      END IF

*            get end of string

	      IF( STRING( K:K) .NE. ' ' .AND.
     :	          STRING( K:K) .NE. CHAR( 0)) THEN

	        ST_EN = K

	      END IF

	    END DO

*          calculate number of characters

	    NUMB_CHAR = ST_EN - ST_ST + 1

*          plot bottom X axis number

	    CALL SGS_BTEXT( X1, Y1)

	    CALL SGS_ATXR( X_NUMBER, -NUMB_CHAR-2, 1)

	  END IF

	  J = J + 1

	END DO

*      test the number orientation and set it

	IF( NUMBER_ORIENT .EQ. 'UP') THEN

*        set the text orientation

	  CALL SGS_SUPTX( -1.0, 0.0)

*        set text positioning on centre in Y and centre in X

	  CALL SGS_STXJ( 'CC')

	ELSE

*        set the text orientation

	  CALL SGS_SUPTX( 0.0, 1.0)

*        set text positioning on centre in Y and right edge in X

	  CALL SGS_STXJ( 'CR')

	END IF

	CALL SGS_FLUSH

* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*      loop for both Y axis numbers POSITIVE from zero point

	J = 1
	Y1 = 0

	YLIMIT = IM_YEN -
     :           NUMBER_YINTERVAL*CONTOUR_MAGNIF*CONTOUR_AXRAT

	DO WHILE ( Y1 .LE. YLIMIT)

	  IF( NUMBER_TYPE .EQ. 'RA_DEC') THEN

* DEC-UP

	    IF( NUMBER_ORIENT .EQ. 'UP') THEN

	      X1 = IM_XST - 1.0*TEXT_HT

	    ELSE

	      X1 = IM_XST - 0.5*TEXT_HT

	    END IF

	    Y1 = IM_YST + ( Y_ZERO +
     :	         NUMBER_YINTERVAL*(J-1))*CONTOUR_MAGNIF*
     :           CONTOUR_AXRAT

	    CALL RADEC_STRING( J, RA, DEC, 'POSITIVE',
     :	                       NUMBER_XINTERVAL, NUMBER_YINTERVAL,
     :                         ARCSEC_PIXEL, NUMTYPE_RA,
     :	                       NUMTYPE_DEC, POS_RA, POS_DEC,
     :	                       STRING_RA, STRING_DEC)

            CALL CHR_CLEAN( STRING_DEC )
            L1 = 0
	    CALL CHR_APPND( STRING_DEC, STRING_DEC, L1)

	    CALL SGS_BTEXT( X1, Y1)
	    CALL SGS_ATXL( STRING_DEC( 1:L1))

*          calculate HALF the length of the text string

	    LENTEXT = L1*TEXT_HT/2.0

*          calculate the start position of the text string

	    YSTTEXT = Y1 - LENTEXT

*          set the text height for the superscripts

	    CALL SGS_SHTX( TEXT_HTS)

*          test if the number to be plotted is a FULL RA or PARTIAL RA

	    IF( NUMTYPE_RA .EQ. 'FULL') THEN

*            calculate the X,Y position for degrees sign

	      IF( NUMBER_ORIENT .EQ. 'UP') THEN

	        XOFF = -0.5*TEXT_HT

	        YOFF = ( POS_DEC( 1) - 1)*TEXT_HT + 0.5*TEXT_HT

	        X2 = X1 + XOFF
	        Y2 = YSTTEXT + YOFF

	      ELSE

	        XOFF = ( POS_DEC( 1) - L1)*TEXT_HT

	        X2 = X1 + XOFF
	        Y2 = Y1 + 0.5*TEXT_HT

	      END IF

*            plot the character

	      SUPERSCRIPT = 'o'

	      CALL SGS_BTEXT( X2,Y2)
	      CALL SGS_ATXL( SUPERSCRIPT)

	    END IF

*          calculate the X,Y position for minutes sign

	    IF( NUMBER_ORIENT .EQ. 'UP') THEN

	      XOFF = -0.5*TEXT_HT

	      YOFF = ( POS_DEC( 2) - 1)*TEXT_HT + 0.5*TEXT_HT

	      X2 = X1 + XOFF
	      Y2 = YSTTEXT + YOFF

	    ELSE

	      XOFF = ( POS_DEC( 2) - L1)*TEXT_HT

	      X2 = X1 + XOFF
	      Y2 = Y1 + 0.3*TEXT_HT

	    END IF

*          plot the character

	    SUPERSCRIPT = CHAR( 39)

	    CALL SGS_BTEXT( X2, Y2)
	    CALL SGS_ATXL( SUPERSCRIPT)

*          calculate the X,Y position for seconds sign

	    IF( NUMBER_ORIENT .EQ. 'UP') THEN

	      XOFF = -0.5*TEXT_HT

	      YOFF = ( POS_DEC( 3) - 1)*TEXT_HT + 0.5*TEXT_HT

	      X2 = X1 + XOFF
	      Y2 = YSTTEXT + YOFF

	    ELSE

	      XOFF = ( POS_DEC( 3) - L1)*TEXT_HT

	      X2 = X1 + XOFF
	      Y2 = Y1 + 0.3*TEXT_HT

	    END IF

*          plot the character

	    SUPERSCRIPT = CHAR( 34)

	    CALL SGS_BTEXT( X2, Y2)
	    CALL SGS_ATXL( SUPERSCRIPT)

*          reset the text height to the original for main text

	    CALL SGS_SHTX( TEXT_HT)

	  ELSE

* OFFSETS-Y-UP

	    IF( NUMBER_ORIENT .EQ. 'UP') THEN

*            reset the text positioning to centre in X and centre in Y

	      CALL SGS_STXJ( 'CC')

	      X1 = IM_XST - 1.3*TEXT_HT

	    ELSE

*            reset the text positioning to centre in X and centre in Y

	      CALL SGS_STXJ( 'CR')

	      X1 = IM_XST - 0.5*TEXT_HT

	    END IF

	    Y1 = IM_YST +
     :	         ( Y_ZERO + NUMBER_YINTERVAL*(J-1))
     :            *CONTOUR_MAGNIF*CONTOUR_AXRAT

	    Y_NUMBER = NUMBER_YINTERVAL*(J-1)*ARCSEC_PIXEL

	    IF( ABS( Y_NUMBER) .LT. 1.0E-5) THEN

	      NUMPTY = -3

	    ELSE IF( ABS( Y_NUMBER) .GT. 1.0E-5 .AND.
     :	             ABS( Y_NUMBER) .LT. 10.0) THEN

	      NUMPTY = -3

	    ELSE IF( ABS( Y_NUMBER) .GE. 10.0 .AND.
     :	             ABS( Y_NUMBER) .LT. 100.0) THEN

	      NUMPTY = -4

	    ELSE IF( ABS( Y_NUMBER) .GE. 100.0 .AND.
     :	             ABS( Y_NUMBER) .LT. 1000.0) THEN

	      NUMPTY = -5

	    ELSE IF( ABS( Y_NUMBER) .GE. 1000.0) THEN

	      NUMPTY = -6

	    END IF

	    IF( Y_NUMBER .LT. 0.0) NUMPTY = NUMPTY - 1

*          plot left Y axis number

	    CALL SGS_BTEXT( X1, Y1)

	    CALL SGS_ATXR( Y_NUMBER, NUMPTY, 1)

	  END IF

	  J = J + 1

	END DO

*      test the number orientation and set it

	IF( NUMBER_ORIENT .EQ. 'UP') THEN

*        set the text orientation

	  CALL SGS_SUPTX( -1.0, 0.0)

*        set text positioning to centre in X and centre in Y

	  CALL SGS_STXJ( 'CC')

	  CALL SGS_FLUSH

	ELSE

*        set the text orientation

	  CALL SGS_SUPTX( 0.0, 1.0)

*        set text positioning on centre in Y and right edge in X

	  CALL SGS_STXJ( 'CR')

	  CALL SGS_FLUSH

	END IF

* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*      loop for both Y axis numbers NEGATIVE from zero point

	J = 1

	IF( NUMBER_ORIENT .EQ. 'UP') THEN

	  X1 = IM_XST - 1.0*TEXT_HT

	ELSE

	  X1 = IM_XST - 0.5*TEXT_HT

	END IF

	Y1 = IM_YEN

	YLIMIT = IM_YST + NUMBER_YINTERVAL*CONTOUR_MAGNIF*
     :                    CONTOUR_AXRAT

	DO WHILE ( Y1 .GT. YLIMIT)

	  IF( NUMBER_TYPE .EQ. 'RA_DEC') THEN

* DEC-DOWN

	    Y1 = IM_YST +
     :	         ( Y_ZERO - NUMBER_YINTERVAL*(J-1))*CONTOUR_MAGNIF*
     :           CONTOUR_AXRAT

*          numbers are ra/dec positions, calculate the values for this position

	    CALL RADEC_STRING( J, RA, DEC, 'NEGATIVE',
     :	                       NUMBER_XINTERVAL, NUMBER_YINTERVAL,
     :                         ARCSEC_PIXEL, NUMTYPE_RA,
     :	                       NUMTYPE_DEC, POS_RA, POS_DEC,
     :	                       STRING_RA, STRING_DEC)

            CALL CHR_CLEAN( STRING_DEC )
            L1 = 0
	    CALL CHR_APPND( STRING_DEC, STRING_DEC, L1)

*          if this is the first negative number then don't plot anything
*          since first positive will have done it

	    IF( J .EQ. 1) THEN

	      STRING_DEC = ' '

	    END IF

	    CALL SGS_BTEXT( X1, Y1)

	    CALL SGS_ATXL( STRING_DEC( 1:L1))

*          calculate HALF the length of the text string

	    LENTEXT = L1*TEXT_HT/2.0

*          calculate the start position of the text string

	    YSTTEXT = Y1 - LENTEXT

*          test if this is a j number greater than 1

	    IF( J .GT. 1) THEN

*            set the text height for the superscripts

	      CALL SGS_SHTX( TEXT_HTS)

*            test if the number to be plotted is a FULL RA or PARTIAL RA

	      IF( NUMTYPE_RA .EQ. 'FULL') THEN

*              calculate the X,Y position for degrees sign

	        IF( NUMBER_ORIENT .EQ. 'UP') THEN

	          XOFF = -0.5*TEXT_HT

	          YOFF = ( POS_DEC( 1) - 1)*TEXT_HT + 0.5*TEXT_HT

	          X2 = X1 + XOFF
	          Y2 = YSTTEXT + YOFF

	        ELSE

	          XOFF = ( POS_DEC( 1) - L1)*TEXT_HT

	          X2 = X1 + XOFF
	          Y2 = Y1 + 0.5*TEXT_HT

	        END IF

*              plot the character

	        SUPERSCRIPT = 'o'

	        CALL SGS_BTEXT( X2,Y2)
	        CALL SGS_ATXL( SUPERSCRIPT)

	      END IF

*            calculate the X,Y position for minutes sign

	      IF( NUMBER_ORIENT .EQ. 'UP') THEN

	        XOFF = -0.5*TEXT_HT

	        YOFF = ( POS_DEC( 2) - 1)*TEXT_HT + 0.5*TEXT_HT

	        X2 = X1 + XOFF
	        Y2 = YSTTEXT + YOFF

	      ELSE

	        XOFF = ( POS_DEC( 2) - L1)*TEXT_HT

	        X2 = X1 + XOFF
	        Y2 = Y1 + 0.3*TEXT_HT

	      END IF

*            plot the character

	      SUPERSCRIPT = CHAR( 39)

	      CALL SGS_BTEXT( X2, Y2)
	      CALL SGS_ATXL( SUPERSCRIPT)

*            calculate the X,Y position for seconds sign

	      IF( NUMBER_ORIENT .EQ. 'UP') THEN

	        XOFF = -0.5*TEXT_HT

	        YOFF = ( POS_DEC( 3) - 1)*TEXT_HT + 0.5*TEXT_HT

	        X2 = X1 + XOFF
	        Y2 = YSTTEXT + YOFF

	      ELSE

	        XOFF = ( POS_DEC( 3) - L1)*TEXT_HT

	        X2 = X1 + XOFF
	        Y2 = Y1 + 0.3*TEXT_HT

	      END IF

*            plot the character

	      SUPERSCRIPT = CHAR( 34)

	      CALL SGS_BTEXT( X2, Y2)
	      CALL SGS_ATXL( SUPERSCRIPT)

*            reset the text height to the original for main text

	      CALL SGS_SHTX( TEXT_HT)

	    END IF

	  ELSE

* OFFSETS-Y-DOWN

	    IF( NUMBER_ORIENT .EQ. 'UP') THEN

*            set text positioning on centre in Y and centre in X

	      CALL SGS_STXJ( 'CC')

	      X1 = IM_XST - 1.3*TEXT_HT

	    ELSE

*            set text positioning on centre in Y and centre in X

	      CALL SGS_STXJ( 'CR')

	      X1 = IM_XST - 0.5*TEXT_HT

	    END IF

	    Y1 = IM_YST +
     :	         ( Y_ZERO - NUMBER_YINTERVAL*(J-1))*
     :            CONTOUR_MAGNIF*CONTOUR_AXRAT

	    Y_NUMBER = -1.0*NUMBER_YINTERVAL*(J-1)*ARCSEC_PIXEL

	    IF( ABS( Y_NUMBER) .LT. 1.0E-5) THEN

	      NUMPTY = -3

	    ELSE IF( ABS( Y_NUMBER) .GT. 1.0E-5 .AND.
     :	             ABS( Y_NUMBER) .LT. 10.0) THEN

	      NUMPTY = -3

	    ELSE IF( ABS( Y_NUMBER) .GE. 10.0 .AND.
     :	             ABS( Y_NUMBER) .LT. 100.0) THEN

	      NUMPTY = -4

	    ELSE IF( ABS( Y_NUMBER) .GE. 100.0 .AND.
     :	             ABS( Y_NUMBER) .LT. 1000.0) THEN

	      NUMPTY = -5

	    ELSE IF( ABS( Y_NUMBER) .GE. 1000.0) THEN

	      NUMPTY = -6

	    END IF

	    IF( Y_NUMBER .LT. 0.0) NUMPTY = NUMPTY - 1

*          plot left Y axis number

	    CALL SGS_BTEXT( X1, Y1)

	    CALL SGS_ATXR( Y_NUMBER, NUMPTY, 1)

	  END IF

	  J = J + 1

	END DO

*      set text positioning on centre in Y and centre in X

	CALL SGS_STXJ( 'CC')

	CALL SGS_FLUSH

*        set the text orientation

	CALL SGS_SUPTX( 0.0, 1.0)

*      empty plot buffer of remaining graphics

	CALL SGS_FLUSH

	IF( NUMBER_TYPE .EQ. 'RA_DEC') THEN

	  IF( COSDELTA .GT. 0.0) THEN

	    NUMBER_XINTERVAL = NUMBER_XINTERVAL/COSDELTA

	  ELSE

	    NUMBER_XINTERVAL = NUMBER_XINTERVAL/0.001

	  END IF

	END IF

	END
