	SUBROUTINE DECODE_LABEL( LABEL_STRING, LABEL_X, LABEL_Y, LABEL,
     :	                         LABEL_LENGTH, STATUS)

* Description : Subroutine to decode the X,Y position and the text string
*               from the input string LABEL_STRING for the labelling of
*               features under the PLT2D LABEL action

* History :
*  20-JUL-1994 Changed STR$ and LIB$ calls to CHR_, IFIX to INT (SKL@JACH)
*

	IMPLICIT NONE

	INCLUDE 'ADAM_DEFNS'
	INCLUDE 'DTDEFNS'
	INCLUDE 'DTERRS'
        INCLUDE 'SAE_PAR'
        INCLUDE 'CHR_ERR'

	INTEGER END_LABEL
	INTEGER LABEL_LENGTH
	INTEGER LABEL_X
	INTEGER LABEL_Y
	INTEGER NUMBER_CHARS
	INTEGER START_LABEL
	INTEGER STATUS

	REAL X_REAL
	REAL Y_REAL

	CHARACTER*(*) LABEL_STRING
	CHARACTER*(*) LABEL
	CHARACTER*80  REST
	CHARACTER*80 X_STRING
	CHARACTER*80 Y_STRING

* initialize the label variables

	LABEL_X = 1
	LABEL_Y = 1
	LABEL = ' '
	LABEL_LENGTH = 1

* get first word from string as this is the X position of the label

	CALL GET_WORD( LABEL_STRING, X_STRING, NUMBER_CHARS, REST,
     :                 STATUS)

	IF( STATUS  .NE. SAI__OK) THEN
	  RETURN
	END IF

* convert X string to real number

        CALL CTOR( X_STRING( 1:NUMBER_CHARS),  X_REAL, STATUS )

	IF( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP('ERR', 'Error converting real to string',
     :                  STATUS )
	  RETURN
	END IF

* take nearest integer as the X position of the label

	LABEL_X = INT( X_REAL + 0.5)

* swap the strings over to get the Y position

	LABEL_STRING = REST

* get second word from string as this is the Y position of the label

	CALL GET_WORD( LABEL_STRING, Y_STRING, NUMBER_CHARS, REST,
     :                 STATUS)

	IF( STATUS .NE. SAI__OK) THEN
	  RETURN
	END IF

* convert Y string to real number

        CALL CTOR( Y_STRING( 1:NUMBER_CHARS),  Y_REAL, STATUS )

	IF( STATUS .NE. SAI__OK ) THEN
          CALL ERR_REP('ERR', 'Error converting real to string',
     :                  STATUS )
	  RETURN
	END IF

* take nearest integer as the Y position of the label

	LABEL_Y = INT( Y_REAL + 0.5)

* get the first non-blank character from the rest string as this is the
* start of the label to be plotted

        CALL CHR_FANDL( REST, START_LABEL, END_LABEL )

* test if there was something after the Y position

	IF( START_LABEL .GT. 0) THEN

* swap over the rest and label string strings

	  LABEL = REST( START_LABEL:END_LABEL)

	  LABEL_LENGTH = END_LABEL - START_LABEL + 1

 	END IF

	END
