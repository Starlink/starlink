	SUBROUTINE CUT_DISPLAY( STATUS)

* Description : Routine to PLOT a CUT/SLICE/STRIP using values

* =====================================================================

* Invocation : Invoked by PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  08-Sep-1988 : JACH::CAA : added CUT_PLOT actions to this subroutine
*  17-Feb-1994   SKL@JACH    changed DAT routines to NDF
*  20-JUL-1994   SKL@JACH    changed STR$ call to CHR_ and IFIX to INT
*  26-Jul-1994   SKL@JACH    changed error reporting ERR_, removed VALUE
*  26-Oct-1994   SKL@JACH    changed MAGNIF from INT to REAL
*  04-Nov-1994   SKL@JACH    added option of auto scaling if magnif=0
*  09-Dec-1994   CAA@JACH    added parameter setting of cursor cut st,end
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

        INCLUDE 'NDF_PAR'
        INCLUDE 'NDF_ERR'

	INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'

* Status :

	INTEGER STATUS

* Import :

* Import-Export :

* Export :

* External references :

* Global variables

	INCLUDE 'PLT2DCOM'

* Local Constants :

* Local variables :

	INTEGER
     :	  CUT_NUMXTIC,
     :	  CUT_NUMYTIC,
     :	  J,
     :	  MAX_POINTS,
     :	  NAXIS( 2),
     :	  NDIM,
     :	  NUMBER_POINTS,
     :	  PEN_ANNOT,
     :	  PEN_LINE,
     :	  POINTER_IMAGE,
     :	  SUBIM_EN( 2),
     :	  SUBIM_ST( 2),
     :	  TEMP_XCEN,
     :	  TEMP_YCEN,
     :	  TEXT_FONT,
     :	  TEXT_PREC
      INTEGER
     :	  X1,
     :	  X2,
     :	  YN,
     :	  Y1,
     :	  Y2,
     :    NELEMENTS,
     :    LOC_IMAGE,
     :	  LOC_SUBIM

	PARAMETER ( MAX_POINTS = 10000)
	PARAMETER ( TEXT_PREC = 2)

	REAL
     :    CUT_MAGNIF,
     :	  CUT_AXISRAT,
     :	  CUT_DEFSIZE,
     :	  CUT_VALUES( MAX_POINTS),
     :	  CUT_XCEN,
     :	  CUT_YCEN,
     :	  LINE_DATA( MAX_POINTS, 2),
     :	  PLOT_AXISRAT,
     :	  TEXT_FRACT,
     :	  TEXT_HT,
     :	  XEN,
     :	  X_HI,
     :	  X_LO,
     :	  XPOS,
     :	  XST,
     :	  XTICK_INTERVAL,
     :	  XTICK_START,
     :	  X_VALUES( MAX_POINTS)
      REAL
     :	  YEN,
     :	  Y_HI,
     :	  Y_HIR,
     :	  Y_LO,
     :	  Y_LOR,
     :	  YPOS,
     :	  YST,
     :    MAGNIF_X,
     :    MAGNIF_Y,
     :    MAX_MAGNIF

	PARAMETER ( CUT_DEFSIZE = 16.0)
	PARAMETER ( TEXT_FRACT = 0.02)
        PARAMETER ( MAX_MAGNIF = 500.0 )

	CHARACTER
     :	  COLOUR_CODE*1,
     :	  CURSOR_MARK*5,
     :	  CURSOR_WHERE*20,
     :	  CUT_ANNOTATION*20,
     :	  CUT_POINTER*1,
     :	  CUT_POSITIONING*20,
     :	  CUT_SCALING*1,
     :	  CUT_TITLE*80,
     :	  SUBIM_OPTION*1,
     :	  USE_CURSOR*10

* Internal References :

* =====================================================================

* check status on entry

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'Error : CUT_DISPLAY : On entry',
     :                 STATUS )
	  RETURN
	END IF

D       write (6,*) 'CUT_DISPLAY on entry, max x:   ', max_x
D       write (6,*) 'CUT_DISPLAY on entry, max y:   ', max_y

* define plotting area axis ratio

	PLOT_AXISRAT = REAL( MAX_X)/REAL( MAX_Y)

* get option to use cursor in cut coordinate selection

	CALL PAR_GET0C( 'CUT_USE_CURSOR', USE_CURSOR, STATUS)

* put cursor option string to upper case

	CALL CHR_UCASE( USE_CURSOR )

* test if user wants to use cursor and perform relevant operations

	IF( USE_CURSOR( 1:1) .EQ. 'Y') THEN

* puts up cursor for start point of line

	  CALL CURSOR_DISPLAY( STATUS)

	  IF( STATUS. NE. SAI__OK) THEN
	    RETURN
	  END IF

* gets start of line from cursor real values

	  CALL PAR_GET0I( 'X_CUR_PIXEL', X1, STATUS)
	  CALL PAR_GET0I( 'Y_CUR_PIXEL', Y1, STATUS)

* puts up cursor for end point of line

	  CALL CURSOR_DISPLAY( STATUS)

	  IF( STATUS. NE. SAI__OK) THEN
	    RETURN
	  END IF

* gets end of line from cursor real values

	  CALL PAR_GET0I( 'X_CUR_PIXEL', X2, STATUS)
	  CALL PAR_GET0I( 'Y_CUR_PIXEL', Y2, STATUS)

* set parameter for cut start and end position

	  CALL PAR_PUT0I( 'CUT_XST', X1, STATUS)
	  CALL PAR_PUT0I( 'CUT_YST', Y1, STATUS)
	  CALL PAR_PUT0I( 'CUT_XEN', X2, STATUS)
	  CALL PAR_PUT0I( 'CUT_YEN', Y2, STATUS)

	ELSE

* get start and end position in pixels

	  CALL PAR_GET0I( 'CUT_XST', X1, STATUS)
	  CALL PAR_GET0I( 'CUT_YST', Y1, STATUS)
	  CALL PAR_GET0I( 'CUT_XEN', X2, STATUS)
	  CALL PAR_GET0I( 'CUT_YEN', Y2, STATUS)

	END IF

* set working X array to zero

	DO J = 1, MAX_POINTS
	  CUT_VALUES( J) = 0.0
	  LINE_DATA( J, 1) = 0.0
	  LINE_DATA( J, 2) = 0.0
	END DO

* get sub-image option

	CALL PAR_GET0C( 'SUBIM_OPTION', SUBIM_OPTION, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'Error : CUT_DISPLAY : after PAR_GETS',
     :                 STATUS )
 	  RETURN
	END IF

* associate data image with HDS file

	CALL NDF_ASSOC( 'CUT_IMAGE', 'READ', LOC_IMAGE, STATUS)

	IF( STATUS .NE. SAI__OK) THEN
          CALL ERR_REP('ERR',
     :         'Error : Cannot locate specified image, sorry ...',
     :                 STATUS )
 	  RETURN
	END IF

* test sub-image option for whole or slice of image

	IF( SUBIM_OPTION .NE. 'S') THEN

* map data in 'DATA_ARRAY' to obtain memory pointer POINTER_IMAGE
* and size of 2D image in NAXIS

	  CALL NDF_MAP( LOC_IMAGE, 'Data', '_REAL', 'READ',
     :	                POINTER_IMAGE, NELEMENTS, STATUS)

	  IF( STATUS. NE. SAI__OK)THEN
	    CALL NDF_ANNUL( LOC_IMAGE,  STATUS)
            CALL ERR_REP('ERR', 'CUT_DISPLAY : During NDF_MAP image',
     :                   STATUS )
	    RETURN
	  END IF

          CALL NDF_DIM( LOC_IMAGE, 2, NAXIS, NDIM, STATUS)

	ELSE

* here if want to plot slice of current image : GET slice area

	  CALL PAR_GET0I( 'SUBIM_XST', SUBIM_ST( 1), STATUS)
	  CALL PAR_GET0I( 'SUBIM_XEN', SUBIM_EN( 1), STATUS)
	  CALL PAR_GET0I( 'SUBIM_YST', SUBIM_ST( 2), STATUS)
	  CALL PAR_GET0I( 'SUBIM_YEN', SUBIM_EN( 2), STATUS)

	  IF( STATUS .NE. SAI__OK) THEN
            CALL ERR_REP('ERR',
     :                   'CUT_DISPLAY : after PAR_GETS subim_st,en',
     :                   STATUS )
   	    RETURN
	  END IF

* GET slice locator

	  CALL NDF_SECT( LOC_IMAGE, 2, SUBIM_ST, SUBIM_EN, LOC_SUBIM,
     :	                  STATUS)

* map data in 'DATA_ARRAY' to obtain memory pointer POINTER_IMAGE
* and size of 2D image in NAXIS

	  CALL NDF_MAP( LOC_SUBIM, 'Data', '_REAL', 'READ',
     :	                POINTER_IMAGE, NELEMENTS, STATUS)

	  IF( STATUS. NE. SAI__OK)THEN
 	    CALL NDF_ANNUL( LOC_IMAGE,  STATUS)
	    CALL NDF_ANNUL( LOC_SUBIM,  STATUS)
            CALL ERR_REP('ERR',
     :                   'CUT_DISPLAY : During NDF_MAP sub-image',
     :                   STATUS )
	    RETURN
	  END IF

	  NAXIS( 1) = ( SUBIM_EN( 1) - SUBIM_ST( 1) + 1)
	  NAXIS( 2) = ( SUBIM_EN( 2) - SUBIM_ST( 2) + 1)

	END IF

* test if pixel start and end is actually in image

	IF( X1 .LT. 1) X1 = 1
	IF( X1 .GT. NAXIS( 1)) X1 = NAXIS( 1)
	IF( Y1 .LT. 1) Y1 = 1
	IF( Y1 .GT. NAXIS( 2)) Y1 = NAXIS( 2)
	IF( X2 .LT. 1) X2 = 1
	IF( X2 .GT. NAXIS( 1)) X2 = NAXIS( 1)
	IF( Y2 .LT. 1) Y2 = 1
	IF( Y2 .GT. NAXIS( 2)) Y2 = NAXIS( 2)

* call subroutine to get pixel coordinates of cut line

	IF( X1 .EQ. X2) THEN       ! cut down a column of the image

	  CALL CUT_LINSETC( X1, X2, Y1, Y2, MAX_POINTS, LINE_DATA,
     :	                    NUMBER_POINTS, STATUS)

	  CALL CUT_INTERPOLATEC( NUMBER_POINTS, MAX_POINTS, LINE_DATA,
     :	                         CUT_VALUES, NAXIS( 1), NAXIS( 2),
     :	                         %VAL( POINTER_IMAGE), STATUS)

	ELSE IF( Y1 .EQ. Y2) THEN  ! cut across a row of the image

	  CALL CUT_LINSETR( X1, X2, Y1, Y2, MAX_POINTS, LINE_DATA,
     :	                    NUMBER_POINTS, STATUS)

	  CALL CUT_INTERPOLATER( NUMBER_POINTS, MAX_POINTS, LINE_DATA,
     :	                         CUT_VALUES, NAXIS( 1), NAXIS( 2),
     :	                         %VAL( POINTER_IMAGE), STATUS)

	ELSE                       ! cut arbitrary orientation

	  CALL CUT_LINSET( X1, X2, Y1, Y2, MAX_POINTS, LINE_DATA,
     :	                   NUMBER_POINTS, STATUS)

	  CALL CUT_INTERPOLATE( NUMBER_POINTS, MAX_POINTS, LINE_DATA,
     :	                        CUT_VALUES, NAXIS( 1), NAXIS( 2),
     :	                        %VAL( POINTER_IMAGE), STATUS)

	END IF

* release data depending on whether full and sub-image plotted

	IF( SUBIM_OPTION .EQ. 'S') THEN

* unmap sub-image data and release locators

 	  CALL NDF_ANNUL( LOC_SUBIM, STATUS)
	  CALL NDF_ANNUL( LOC_IMAGE, STATUS)

	  IF( STATUS .NE. SAI__OK) THEN
            CALL ERR_REP('ERR',
     :                   'CUT_DISPLAY : after NDF_ANNUL sub-image',
     :                   STATUS )
	    RETURN
	  END IF

	ELSE

* unmap image data and release locator

	  CALL NDF_ANNUL( LOC_IMAGE,   STATUS)

	  IF( STATUS .NE. SAI__OK) THEN
            CALL ERR_REP('ERR',
     :                   'CUT_DISPLAY : after NDF_ANNULs',
     :                   STATUS )
	    RETURN
	  END IF

        END IF

* get the cut annotation option from the parameter system

	CALL PAR_GET0C( 'CUT_ANNOTATION', CUT_ANNOTATION, STATUS)

* get the cut positioning variable from the parameter system

	CALL PAR_GET0C( 'CUT_POSITIONING', CUT_POSITIONING, STATUS)

* test if want manual scaling or auto scaling of X anf Y plot scale

	CALL PAR_GET0C( 'CUT_SCALING', CUT_SCALING, STATUS)

* get max,min in data and in largest range position values

	CALL CUT_MAXMIN( CUT_SCALING, NUMBER_POINTS, MAX_POINTS, LINE_DATA,
     :	                 CUT_VALUES, X_LO, X_HI, Y_LO, Y_HI, CUT_POINTER)

* set cut scaling parameters

	IF( CUT_SCALING .EQ. 'M') THEN

	  Y_HIR = Y_HI
	  Y_LOR = Y_LO

	  CALL PAR_GET0R( 'CUT_YMAX', Y_HI, STATUS)
	  CALL PAR_GET0R( 'CUT_YMIN', Y_LO, STATUS)

	ELSE

	  Y_HIR = Y_HI
	  Y_LOR = Y_LO

	END IF

* get the magnification factor

	CALL PAR_GET0R( 'CUT_MAGNIF', CUT_MAGNIF, STATUS)

*      put size of data image into simple variables

	NX = X2 - X1 + 1
	NY = Y2 - Y1 + 1

*      if magnification is 0 then need to auto scale to fill screen

	IF( CUT_MAGNIF .EQ. 0.0) THEN

*        calculate magnification for maximum image dimension

	  IF( DEVICE_NAME .EQ. 'T5688') THEN

*          set magnification for Sigma 5688 device working in quadrant mode

	    MAGNIF_X = REAL(MAX_X)/2.0/REAL(NX)
	    MAGNIF_Y = REAL(MAX_Y)/2.0/REAL(NY)

	    CUT_MAGNIF = MIN( MAGNIF_X, MAGNIF_Y)

	  ELSE

*          set magnification for other devices

	    MAGNIF_X = REAL(MAX_X)/REAL(NX)
	    MAGNIF_Y = REAL(MAX_Y)/REAL(NY)

	    CUT_MAGNIF = MIN( MAGNIF_X, MAGNIF_Y)

	  END IF

*        make actual magnification 60% of this value to allow for annotation

          CUT_MAGNIF = 0.6 * CUT_MAGNIF

*        test for above maximum value for magnification

	  IF( CUT_MAGNIF .GT. MAX_MAGNIF) THEN

	    CUT_MAGNIF = MAX_MAGNIF

	  END IF

*        put calculated magnification into parameter

          CALL PAR_PUT0R( 'CUT_CALMAG', CUT_MAGNIF, STATUS)

	END IF

* get the cut axis ratio

	CALL PAR_GET0R( 'CUT_AXISRATIO', CUT_AXISRAT, STATUS)

* test whether user want to select position or use cursor

	IF( CUT_POSITIONING .EQ. 'CURSOR') THEN

* get the option to mark cursor position

	  CALL PAR_GET0C( 'CURSOR_CROSS', CURSOR_MARK, STATUS)

* set the option to mark cursor position to NO

	  CALL PAR_PUT0C( 'CURSOR_CROSS', 'NO', STATUS)

* put up cursor to get the X,Y cut position on screen

	  CALL CURSOR_POSITION( STATUS)

* get the X,Y cut position on screen from parameter system

	  CALL PAR_GET0R( 'X_CUR_REAL', CUT_XCEN, STATUS)
	  CALL PAR_GET0R( 'Y_CUR_REAL', CUT_YCEN, STATUS)

* reset the cursor marking option

	  CALL PAR_PUT0C( 'CURSOR_CROSS', CURSOR_MARK, STATUS)

	ELSE

* get the X,Y centre of the cut

	  CALL PAR_GET0I( 'CUT_XCEN', TEMP_XCEN, STATUS)
	  CALL PAR_GET0I( 'CUT_YCEN', TEMP_YCEN, STATUS)

	  CUT_XCEN = REAL( TEMP_XCEN)
	  CUT_YCEN = REAL( TEMP_YCEN)

	END IF

* get the position the cut cursor refers to

	CALL PAR_GET0C( 'CURSOR_WHERE', CURSOR_WHERE, STATUS)

* calculate the x and y start and end points of plot

	IF( CURSOR_WHERE .EQ. 'BOTTOM_LEFT') THEN

	  XST = CUT_XCEN
	  XEN = CUT_XCEN + CUT_MAGNIF*( MAX_X/CUT_DEFSIZE)/PLOT_AXISRAT

	  YST = CUT_YCEN
	  YEN = CUT_YCEN + CUT_MAGNIF*( MAX_Y/CUT_DEFSIZE)*CUT_AXISRAT

	ELSE IF( CURSOR_WHERE .EQ. 'TOP_RIGHT') THEN

	  XST = CUT_XCEN - CUT_MAGNIF*( MAX_X/CUT_DEFSIZE)/PLOT_AXISRAT
	  XEN = CUT_XCEN

	  YST = CUT_YCEN - CUT_MAGNIF*( MAX_Y/CUT_DEFSIZE)*CUT_AXISRAT
	  YEN = CUT_YCEN

	ELSE

	  XST = CUT_XCEN - 0.5*CUT_MAGNIF*( MAX_X/CUT_DEFSIZE)/PLOT_AXISRAT
	  XEN = CUT_XCEN + 0.5*CUT_MAGNIF*( MAX_X/CUT_DEFSIZE)/PLOT_AXISRAT

	  YST = CUT_YCEN - 0.5*CUT_MAGNIF*( MAX_Y/CUT_DEFSIZE)*CUT_AXISRAT
	  YEN = CUT_YCEN + 0.5*CUT_MAGNIF*( MAX_Y/CUT_DEFSIZE)*CUT_AXISRAT

	END IF

* put the cut x,y start,end into the image start,end parameters

	CALL PAR_PUT0R( 'IM_XST', XST, STATUS)
	CALL PAR_PUT0R( 'IM_YST', YST, STATUS)
	CALL PAR_PUT0R( 'IM_XEN', XEN, STATUS)
	CALL PAR_PUT0R( 'IM_YEN', YEN, STATUS)

* setup centre position on screen of current device

	TEXT_HT = TEXT_FRACT*CUT_MAGNIF*MAX_Y/CUT_DEFSIZE

* set the text font and precision

	CALL PAR_GET0I( 'COMMENT_FONT', TEXT_FONT, STATUS)

	CALL GSTXFP( TEXT_FONT, TEXT_PREC)

* get and set colour of line

	CALL PAR_GET0I( 'CUT_PEN', PEN_LINE, STATUS)
	CALL PAR_GET0C( 'CUT_COLOUR', COLOUR_CODE, STATUS)

!	type *, 'device_name = ', device_name
	IF(  DEVICE_NAME .EQ. 'T5688' .OR.
     :	     DEVICE_NAME .EQ. 'ARGS' .OR.
     :	     DEVICE_NAME .EQ. 'ARGS_OVERLAY' .OR.
     :	     DEVICE_NAME .EQ. 'IKON' .OR.
     :	     DEVICE_NAME .EQ. 'IKON_OVERLAY' .OR.
     :	     DEVICE_NAME .EQ. 'T6134' .OR.
     :	     DEVICE_NAME .EQ. 'CPSP' .OR.
     :	     DEVICE_NAME .EQ. 'CPSL' .OR.
     :	     DEVICE_NAME .EQ. 'X-WINDOWS' .OR.
     :	     DEVICE_NAME .EQ. 'VAXSTATION8') THEN
	  CALL SET_COLOUR2( PEN_LINE, COLOUR_CODE)
	ELSE
	  COLOUR_CODE = 'N'
	  IF(  DEVICE_NAME .NE. 'PS_LANDSCAPE' .AND.
     :	       DEVICE_NAME .NE. 'PS_PORTRAIT') THEN
	    CALL SET_COLOUR2( PEN_LINE, COLOUR_CODE)
	  END IF
	END IF

* put X numbers into correct variables

	IF( CUT_POINTER .EQ. 'X') THEN

	  DO J = 1, NUMBER_POINTS
	    X_VALUES( J) = LINE_DATA( J, 1)
	  END DO

	ELSE

	  DO J = 1, NUMBER_POINTS
	    X_VALUES( J) = LINE_DATA( J, 2)
	  END DO

	END IF

* draw cut after all this messing about ...

	CALL CUT_LINEPLOT( NUMBER_POINTS, X_VALUES, CUT_VALUES, X_LO,
     :                     X_HI, Y_LO, Y_HI, XST, XEN, YST, YEN,
     :                     CUT_MAGNIF, STATUS)

	CALL SGS_FLUSH

* set colour of all annotation

	CALL PAR_GET0I( 'CUT_PENANNOT', PEN_ANNOT, STATUS)
	CALL PAR_GET0C( 'CUT_COLANNOT', COLOUR_CODE, STATUS)
	IF(  DEVICE_NAME .NE. 'PS_LANDSCAPE' .AND.
     :	     DEVICE_NAME .NE. 'PS_PORTRAIT') THEN
	  CALL SET_COLOUR( PEN_ANNOT, COLOUR_CODE)
	END IF

* set to centre text at position

	CALL SGS_STXJ( 'CC')

* set text height

	TEXT_HT = TEXT_HT*1.25
	CALL SGS_SHTX( TEXT_HT)

* plot bottom line

	XPOS = ( XST + ( XEN - XST)/2.0)
	YPOS = YST - TEXT_HT*3.0

	CALL SGS_BTEXT( XPOS, YPOS)
	CALL SGS_ATXL( 'Data Max,Min : ')

	YN = INT( LOG10( MAX( ABS( Y_HIR), 1.0E-36)) + 1.0) + 3
	IF( Y_HIR .LT. 0.0) YN = YN + 1
	YN = YN + 1
	YN = -1*YN

	CALL SGS_ATXR( Y_HIR, YN, 2)
	CALL SGS_ATXL( '/')

	IF( ABS( Y_LOR) .GT. 1.0E-10) THEN
	  YN = INT( LOG10( MAX( ABS( Y_LOR), 1.0E-36)) + 1.0) + 3
	ELSE
	  YN = 4
	END IF

	IF( Y_LOR .LT. 0.0) YN = YN + 1
	YN = -1*YN

	CALL SGS_ATXR( Y_LOR, YN, 2)

	XPOS = ( XST + ( XEN - XST)/2.0)
	YPOS = YST - TEXT_HT*5.0

	CALL SGS_BTEXT( XPOS, YPOS)
	CALL SGS_ATXL( 'Cut coordinates : ')

	YN = INT( LOG10( MAX( ABS( REAL( X1)), 1.0E-36)) + 1.0)
	YN = YN + 1
	YN = -1*YN

	CALL SGS_ATXI( X1, YN)
	CALL SGS_ATXL( ',')

	YN = INT( LOG10( MAX( ABS( REAL( Y1)), 1.0E-36)) + 1.0)
	YN = -1*YN

	CALL SGS_ATXI( Y1, YN)
	CALL SGS_ATXL( '-to-')

	YN = INT( LOG10( MAX( ABS( REAL( X2)), 1.0E-36)) + 1.0)
	YN = -1*YN

	CALL SGS_ATXI( X2, YN)
	CALL SGS_ATXL( ',')

	YN = INT( LOG10( MAX( ABS( REAL( Y2)), 1.0E-36)) + 1.0)
	YN = -1*YN

	CALL SGS_ATXI( Y2, YN)

* plot line at zero

	IF( Y_LO .LE. 0.0 .AND. Y_HI .GT. 0.0) THEN

	  CALL SGS_LINE( XST,
     :	                 ( YST +
     :                     ( 0.0 - Y_LO)/( Y_HI - Y_LO)*( YEN - YST)
     :                          ),
     :	                 XEN,
     :	                 ( YST +
     :                     ( 0.0 - Y_LO)/( Y_HI - Y_LO)*( YEN - YST)
     :                          )         )

	END IF

* test if want full annotation of just the line and the bottom info

	IF( CUT_ANNOTATION .EQ. 'FULL') THEN

* plot box around plot

	  CALL SGS_BOX( XST, XEN, YST, YEN)

* get and plot title

	  CALL PAR_GET0C( 'CUT_TITLE', CUT_TITLE, STATUS)

	  XPOS = ( XST + ( XEN - XST)/2.0)
	  YPOS = YEN + TEXT_HT*2

	  CALL SGS_BTEXT( XPOS, YPOS)
	  CALL SGS_ATXL( CUT_TITLE)

* set text height

	  TEXT_HT = TEXT_HT/1.35
	  CALL SGS_SHTX( TEXT_HT)

* get X tick start and X tick interval and Y tick number

	  CALL PAR_GET0R( 'CUT_XTICST', XTICK_START, STATUS)
	  CALL PAR_GET0R( 'CUT_XTICINT', XTICK_INTERVAL, STATUS)
	  CALL PAR_GET0I( 'CUT_NUMYTIC', CUT_NUMYTIC, STATUS)

* draw tick marks on plot

	  CALL CUT_TICKS( X_LO, X_HI, Y_LO, Y_HI, XST, XEN, YST, YEN,
     :	                  CUT_MAGNIF, XTICK_START, XTICK_INTERVAL,
     :	                  CUT_NUMYTIC, CUT_NUMXTIC, STATUS)

	  IF( STATUS .NE. SAI__OK) THEN
            CALL ERR_REP('ERR',
     :                   'CUT_DISPLAY : after CUT_TICKS',
     :                   STATUS )
	    RETURN
	  END IF

* draw NUMBERS on plot

	  CALL CUT_NUMBERS( X_LO, X_HI, Y_LO, Y_HI, XST, XEN, YST, YEN,
     :	                    CUT_MAGNIF, CUT_AXISRAT, XTICK_START,
     :	                    XTICK_INTERVAL, CUT_NUMYTIC, CUT_NUMXTIC,
     :	                    STATUS)

	  IF( STATUS .NE. SAI__OK) THEN
            CALL ERR_REP('ERR',
     :                   'CUT_DISPLAY : after CUT_NUMBERS',
     :                   STATUS )
	    RETURN
	  END IF

	END IF

* flush all output from graphics buffer

	CALL SGS_FLUSH

	END
