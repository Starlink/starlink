	SUBROUTINE SET_COLOUR2( NUMBER_OF_PEN, COLOUR_CODE)

* Description : Routine to change colour of one gun of colour table

* =====================================================================

* Authors : C.Aspin (UOE)

* History :
* 21-May-86 : CAA : added COLOUR/INTENT option to allow 3 pen intensities to be
*                   chosen
* endhistory :

* Type Definitions

	IMPLICIT NONE

* Global constants :

	INCLUDE 'ADAM_DEFNS'

	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'

* Status :

	INTEGER STATUS

* Import :

	INTEGER NUMBER_OF_PEN

	CHARACTER*1 COLOUR_CODE

* Import-Export :

* Export :

* External references :

* Global variables

	INCLUDE 'PLT2DCOM'

* Local Constants :

* Local variables :

	INTEGER PEN_COLOUR

	REAL COLOUR( 3)

	CHARACTER*20 GUN_SPECIFY

* Internal References :

* =====================================================================

* initialize gun colour to zero
	  COLOUR( 1) = 0.0
	  COLOUR( 2) = 0.0
	  COLOUR( 3) = 0.0

* get the parameter specifying the choice of pen colour variables
	  CALL PAR_GET0C( 'GUN_SPEC', GUN_SPECIFY, STATUS)

* test the pen specify variable for the action to take
	  IF( GUN_SPECIFY .EQ. 'INTENSITY') THEN

* get the intensities of the three guns for the pen chosen and check values are
* between 0.0 and 1.0
	    CALL PAR_GET0R( 'GUN_1', COLOUR( 1), STATUS)
	    IF( COLOUR( 1) .LT. 0.0 .OR. COLOUR( 1) .GT. 1.0) THEN
	      RETURN
	    END IF
	    CALL PAR_GET0R( 'GUN_2', COLOUR( 2), STATUS)
	    IF( COLOUR( 2) .LT. 0.0 .OR. COLOUR( 2) .GT. 1.0) THEN
	      RETURN
	    END IF
	    CALL PAR_GET0R( 'GUN_3', COLOUR( 3), STATUS)
	    IF( COLOUR( 3) .LT. 0.0 .OR. COLOUR( 3) .GT. 1.0) THEN
	      RETURN
	    END IF
	  ELSE

* set pen colour (INTEGER) from colour code (CHAR)
	    IF( COLOUR_CODE .EQ. 'W') PEN_COLOUR = 1
	    IF( COLOUR_CODE .EQ. 'R') PEN_COLOUR = 2
	    IF( COLOUR_CODE .EQ. 'G') PEN_COLOUR = 3
	    IF( COLOUR_CODE .EQ. 'B') PEN_COLOUR = 4
	    IF( COLOUR_CODE .EQ. 'Y') PEN_COLOUR = 5
	    IF( COLOUR_CODE .EQ. 'P') PEN_COLOUR = 6
	    IF( COLOUR_CODE .EQ. 'C') PEN_COLOUR = 7
	    IF( COLOUR_CODE .EQ. 'S') PEN_COLOUR = 8
	    IF( COLOUR_CODE .EQ. 'N') PEN_COLOUR = 9

* set colour of RED gun
	    IF( PEN_COLOUR .EQ. 1 .OR. PEN_COLOUR .EQ. 2 .OR.
     :	        PEN_COLOUR .EQ. 5 .OR. PEN_COLOUR .EQ. 6 .OR.
     :	        PEN_COLOUR .EQ. 8) THEN
	      COLOUR( 1) = 1.0
	    END IF

* set colour of GREEN gun
	    IF( PEN_COLOUR .EQ. 1 .OR. PEN_COLOUR .EQ. 3 .OR.
     :	        PEN_COLOUR .EQ. 5 .OR. PEN_COLOUR .EQ. 7) THEN
	      COLOUR( 2) = 1.0
	    END IF
	    IF( PEN_COLOUR .EQ. 8) THEN
	      COLOUR( 2) = 0.5
	    END IF

* set colour of BLUE gun
	    IF( PEN_COLOUR .EQ. 1 .OR. PEN_COLOUR .EQ. 4 .OR.
     :	        PEN_COLOUR .EQ. 6 .OR. PEN_COLOUR .EQ. 7) THEN
	      COLOUR( 3) = 1.0
	    END IF
	    IF( PEN_COLOUR .EQ. 8) THEN
	      COLOUR( 3) = 0.5
	    END IF

* set colour to BLACK
	    IF( PEN_COLOUR .EQ. 9) THEN
	      COLOUR( 1) = 0.0
	      COLOUR( 2) = 0.0
	      COLOUR( 3) = 0.0
	    END IF
	  END IF

* set the color of the polyline, polymarker and text
	  IF( DEVICE_NAME .NE. 'CPSP' .AND.
     :	      DEVICE_NAME .NE. 'CPSL') THEN
	    CALL GSPLCI( NUMBER_OF_PEN)
	    CALL GSPMCI( NUMBER_OF_PEN)
	    CALL GSTXCI( NUMBER_OF_PEN)
	    CALL GSCR( 1, NUMBER_OF_PEN, COLOUR( 1), COLOUR( 2), COLOUR( 3))
!	    type *, 'SET_COLOUR:', NUMBER_OF_PEN, ' ', COLOUR_CODE
	  ELSE
!	    type *, 'SET_COLOUR:', NUMBER_OF_PEN, ' ', COLOUR_CODE
	  END IF

	END
