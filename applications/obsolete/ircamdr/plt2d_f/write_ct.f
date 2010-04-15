	SUBROUTINE WRITE_CT( NXC, NYC, COLOUR_TABLE, STATUS)

* Description : Subroutine to write colour table to workstation

* ========================================================================

* Invocation : invoked by COLOUR_TABLE in PLT2D

* Parameters : Defined in interface module

* Authors : C.Aspin (UOE)

* Date :  changes (institution::username)
*         CREATED : REVS::CAA, REVA::CAA

* History :
*  21-Jul-1994 Changed STR$ to CHR_, IFIX to INT (SKL@JACH)
*  26-JUL-1994 Changed error reporting to use ERR_, removed VALUE (SKL@JACH)
*  13-SEP-1994 Modified colour table values to be compatible with either
*                old IRCAM format (0-255) or new portable_figaro format
*                (0-1) (CAA@JACH)
* Endhistory

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'
        INCLUDE 'SAE_PAR'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'
        INCLUDE 'CHR_ERR'
	INCLUDE 'PLT2DCOM'

* Status

	INTEGER STATUS

* Import :

	INTEGER NXC
	INTEGER NYC

	REAL COLOUR_TABLE( NXC, NYC)

* Import-Export :

* Export :

* External references :

* Local Constants :

* Local variables :

	INTEGER CI
	INTEGER I
	INTEGER J
	INTEGER PEN_NUMBER, PCOL

	REAL TEMPO, RCI
	REAL PEN_COLOUR( 3)
	REAL COLOUR_TABLE2( 3, 256)
	REAL MAXVAL

	CHARACTER*15 CT_DIRECTION, ANNCOL

* Internal References :

* Local data :

* ===================================================================

* check status on entry
	IF( STATUS. NE. SAI__OK) THEN
	  RETURN
	END IF

	PCOL = 0
	RCI = 0.0

* get direction of colour table from parameter system
	CALL PAR_GET0C( 'CT_DIRECTION', CT_DIRECTION, STATUS)
	IF( STATUS. NE. SAI__OK) THEN
          CALL ERR_REP('ERR', 'WRITE_CT : after PAR_GET CT_DIRECTION',
     :                  STATUS )
	  RETURN
	END IF

* check whether colour table is old IRCAM format with number 0-255
* or new IRCAM/PORTABLE_FIGARO format with numbers 0-1.  If the
* latter scale to 0-255 for IRCAM software use.
	MAXVAL = -1.0E20
	DO I = 1, NYC
	  DO J = 1, NXC
	    IF( MAXVAL .LT. COLOUR_TABLE( J, I)) THEN
	      MAXVAL = COLOUR_TABLE( J, I)
	    END IF
	  END DO
	END DO
!	type *, maxval, ifix( maxval+0.5)
	IF( IFIX( MAXVAL+0.5) .EQ. 255) THEN
	  DO I = 1, NYC
	    DO J = 1, NXC
	      COLOUR_TABLE2( J, I) = COLOUR_TABLE( J, I)
	    END DO
	  END DO
	ELSE
	  DO I = 1, NYC
	    DO J = 1, NXC
	      COLOUR_TABLE2( J, I) = COLOUR_TABLE( J, I)*255
	    END DO
	  END DO
	END IF

* put each section of colour table into colour array for writing to
* workstation
	TEMPO = 256.0/REAL( MAXIMCOL)
!	write( 47, *) maximcol, tempo
	DO J = MAX( 1, MINIMCOL), MAXIMCOL

* calculate the color index in array to be used
	  IF( J .EQ. 1) THEN
	    RCI = 1.0
	  ELSE
	    RCI = RCI + TEMPO
	  END IF
	  CI = INT( RCI + 0.5)

* test which way around user wants colour_table
	  IF( CT_DIRECTION .EQ. 'INVERSE' .OR.
     :	      DEVICE_NAME .EQ. 'EPSP' .OR.
     :	      DEVICE_NAME .EQ. 'EPSL' .OR.
     :	      DEVICE_NAME .EQ. 'CPSP' .OR.
     :	      DEVICE_NAME .EQ. 'CPSL') THEN

* define pens with HIGHEST values first
	    CI = 256 - CI + 1
	  END IF

* test if color index is legal
	  IF( CI .LT. 1) CI = 1
	  IF( CI .GT. 256) CI = 256

* loop for 3 guns
	  DO I = 1, NXC

* define pen color
	    PEN_COLOUR( I) = COLOUR_TABLE2( I, CI)/255.0
	  END DO

!	write( 5, *) j, ci, pen_colour( 1), pen_colour( 2), pen_colour( 3)

* put section of colour table to screen
	  PEN_NUMBER = J

* set pen PEN_NUMBER to value PEN_COLOUR( 1, 2 and 3)
	  CALL GSPLCI( PEN_NUMBER)
	  CALL GSPMCI( PEN_NUMBER)
	  CALL GSTXCI( PEN_NUMBER)
	  CALL GSCR( 1, PEN_NUMBER, PEN_COLOUR( 1), PEN_COLOUR( 2),
     :	             PEN_COLOUR( 3))
	END DO

* if device is colour postscript then do something else
	IF( DEVICE_NAME .EQ. 'CPSP' .OR.
     :	    DEVICE_NAME .EQ. 'CPSL') THEN
	  CALL GSPLCI( MAXIMCOL+1)
	  CALL GSPMCI( MAXIMCOL+1)
	  CALL GSTXCI( MAXIMCOL+1)
	  CALL PAR_GET0C( 'ANNOTATE_COLOUR', ANNCOL, STATUS)
	  CALL CHR_UCASE( ANNCOL )
	  ANNCOL = ANNCOL( 1:1)
	  IF( ANNCOL .EQ. 'W') PCOL = 1
	  IF( ANNCOL .EQ. 'R') PCOL = 2
	  IF( ANNCOL .EQ. 'G') PCOL = 3
	  IF( ANNCOL .EQ. 'B') PCOL = 4
	  IF( ANNCOL .EQ. 'Y') PCOL = 5
	  IF( ANNCOL .EQ. 'P') PCOL = 6
	  IF( ANNCOL .EQ. 'C') PCOL = 7
	  IF( ANNCOL .EQ. 'S') PCOL = 8
	  IF( ANNCOL .EQ. 'N') PCOL = 9
	  IF( PCOL .EQ. 1 .OR. PCOL .EQ. 2 .OR.
     :	      PCOL .EQ. 5 .OR. PCOL .EQ. 6 .OR.
     :	      PCOL .EQ. 8) THEN
	    PEN_COLOUR( 1) = 1.0
	  END IF
	  IF( PCOL .EQ. 1 .OR. PCOL .EQ. 3 .OR.
     :	      PCOL .EQ. 5 .OR. PCOL .EQ. 7) THEN
	    PEN_COLOUR( 2) = 1.0
	  END IF
	  IF( PCOL .EQ. 8) THEN
	    PEN_COLOUR( 2) = 0.5
	  END IF
	  IF( PCOL .EQ. 1 .OR. PCOL .EQ. 4 .OR.
     :	      PCOL .EQ. 6 .OR. PCOL .EQ. 7) THEN
	    PEN_COLOUR( 3) = 1.0
	  END IF
	  IF( PCOL .EQ. 8) THEN
	    PEN_COLOUR( 3) = 0.5
	  END IF
	  IF( PCOL .EQ. 9) THEN
	    PEN_COLOUR( 1) = 0.0
	    PEN_COLOUR( 2) = 0.0
	    PEN_COLOUR( 3) = 0.0
	  END IF
	  CALL GSCR( 1, MAXIMCOL+1, PEN_COLOUR( 1), PEN_COLOUR( 2),
     :	             PEN_COLOUR( 3))
	END IF

* flush buffer of residual output
	CALL SGS_FLUSH

	END
