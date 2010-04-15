	SUBROUTINE GET_INTENSITIES( COL_CHAR, INTENSITIES)

* Description : Subroutine to interpret the colour chosen into the intensities
*	        of the three guns for that pen

* Changed range from 0-255 to 0-1 : 14-Sep-1994 : CAA@JACH

	IMPLICIT NONE

	INTEGER PEN_COLOUR

	REAL INTENSITIES( 3)

	CHARACTER*1 COL_CHAR

* initialize the intensities to 0

	INTENSITIES( 1) = 0.0
	INTENSITIES( 2) = 0.0
	INTENSITIES( 3) = 0.0

* test which colour chosen

	PEN_COLOUR = 0
	IF( COL_CHAR. EQ. 'W' .OR. COL_CHAR. EQ. 'w') PEN_COLOUR = 1
	IF( COL_CHAR. EQ. 'R' .OR. COL_CHAR. EQ. 'r') PEN_COLOUR = 2
	IF( COL_CHAR. EQ. 'G' .OR. COL_CHAR. EQ. 'g') PEN_COLOUR = 3
	IF( COL_CHAR. EQ. 'B' .OR. COL_CHAR. EQ. 'b') PEN_COLOUR = 4
	IF( COL_CHAR. EQ. 'Y' .OR. COL_CHAR. EQ. 'y') PEN_COLOUR = 5
	IF( COL_CHAR. EQ. 'P' .OR. COL_CHAR. EQ. 'p') PEN_COLOUR = 6
	IF( COL_CHAR. EQ. 'C' .OR. COL_CHAR. EQ. 'c') PEN_COLOUR = 7
	IF( COL_CHAR. EQ. 'S' .OR. COL_CHAR. EQ. 's') PEN_COLOUR = 8
	IF( COL_CHAR. EQ. 'N' .OR. COL_CHAR. EQ. 'n') PEN_COLOUR = 9

* set the red gun intensities

	IF( PEN_COLOUR. EQ. 1. OR. PEN_COLOUR. EQ. 2 .OR.
     *	    PEN_COLOUR. EQ. 5. OR. PEN_COLOUR. EQ. 6 .OR.
     *	    PEN_COLOUR. EQ. 8) THEN
	  INTENSITIES( 1) = 1.0
	END IF

* set the blue gun intensities

	IF( PEN_COLOUR. EQ. 1. OR. PEN_COLOUR. EQ. 3 .OR.
     *	    PEN_COLOUR. EQ. 5. OR. PEN_COLOUR. EQ. 7) THEN
	  INTENSITIES( 2) = 1.0
	END IF
	IF( PEN_COLOUR. EQ. 8) THEN
	  INTENSITIES( 2) = 0.5
	END IF

* set the green gun intensities

	IF( PEN_COLOUR. EQ. 1. OR. PEN_COLOUR. EQ. 4 .OR.
     *	    PEN_COLOUR. EQ. 6. OR. PEN_COLOUR. EQ. 7) THEN
	  INTENSITIES( 3) = 1.0
	END IF
	IF( PEN_COLOUR .EQ. 8) THEN
	  INTENSITIES( 3) = 0.5
	END IF

* setup a black pen intensity

	IF( PEN_COLOUR .EQ. 9) THEN
	  INTENSITIES( 1) = 0.0
	  INTENSITIES( 2) = 0.0
	  INTENSITIES( 3) = 0.0
	END IF

	END
