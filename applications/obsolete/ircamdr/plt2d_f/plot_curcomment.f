	SUBROUTINE PLOT_CURCOMMENT( STATUS)

* Description : Routine to write text strings onto current workstation
* with cursor input of position

* =======================================================================

* Invocation : Invoked by d-task environment

* Parameters : Defined in interface module for PLT2D

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  27-Jul-1994 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'
        INCLUDE 'SAE_PAR'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

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

	INTEGER FONT
	INTEGER PREC
	INTEGER PEN_NUMBER

	REAL COMMENT_SIZE
	REAL COMMENT_XPOS
	REAL COMMENT_YPOS
	REAL XUP
	REAL YUP

	CHARACTER COMMENT_COLOUR*1
	CHARACTER COMMENT_ORIENT*2
	CHARACTER COMMENT_WORD*75

* Internal References :

* ================================================================

* check status on entry

	IF( STATUS. NE. SAI__OK) THEN

          CALL ERR_REP('ERR', 'Error : PLOT_CURCOMMENT : On entry',
     :                  STATUS )
	  RETURN

	END IF

* get the font code and the precision for plotting

	CALL PAR_GET0I( 'COMMENT_FONT', FONT, STATUS)

	CALL PAR_GET0I( 'COMMENT_PREC', PREC, STATUS)

* set the font and precision

	CALL GSTXFP( FONT, PREC)

* get parameters defining text plot from parameter system

	CALL PAR_GET0R( 'COMMENT_SIZE', COMMENT_SIZE, STATUS)

	CALL PAR_GET0I( 'COMMENT_PEN', PEN_NUMBER, STATUS)

	CALL PAR_GET0C( 'COMMENT_COLOUR', COMMENT_COLOUR, STATUS)

	CALL PAR_GET0C( 'COMMENT_ORIENT', COMMENT_ORIENT, STATUS)

	CALL PAR_GET0C( 'COMMENT_WORD', COMMENT_WORD, STATUS)

	IF( STATUS. NE. SAI__OK) THEN

          CALL ERR_REP('ERR',
     :                 'Error : PLOT_CURCOMMENT : after PAR_GETS',
     :                  STATUS )
	  RETURN

	END IF

* set text size

	CALL SGS_SHTX( COMMENT_SIZE)

* test which text orientation is required

	XUP = 0.0
	YUP = 1.0

	IF( COMMENT_ORIENT. EQ. 'HR') THEN

	  XUP = 0.0
	  YUP = 1.0

	ELSE IF( COMMENT_ORIENT. EQ. 'HL') THEN

	  XUP = 0.0
	  YUP = -1.0

	ELSE IF( COMMENT_ORIENT. EQ. 'VR') THEN

	  XUP = -1.0
	  YUP = 0.0

	ELSE IF( COMMENT_ORIENT. EQ. 'VL') THEN

	  XUP = 1.0
	  YUP = 0.0

	END IF

* set text positioning code

	CALL SGS_STXJ( 'CC')

* set text orientation

	CALL SGS_SUPTX( XUP, YUP)

* set colour

	CALL SET_COLOUR( PEN_NUMBER, COMMENT_COLOUR)

* put up cursor to get position of text

	CALL CURSOR_POSITION( STATUS)

	IF( STATUS .NE. SAI__OK) THEN

	  RETURN

	END IF

* get values of position from cursor real values

	CALL PAR_GET0R( 'X_CUR_REAL', COMMENT_XPOS, STATUS)
	CALL PAR_GET0R( 'Y_CUR_REAL', COMMENT_YPOS, STATUS)

* write out text string

	CALL SGS_BTEXT( COMMENT_XPOS, COMMENT_YPOS)

	CALL SGS_ATXL( COMMENT_WORD)

	CALL SGS_FLUSH

	END
