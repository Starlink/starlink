	SUBROUTINE COLUMN_LABEL( ICST,
     *	                         ICNB,
     *	                         ICIC)

	IMPLICIT NONE

*     HISTORY
*     14-JUL-1994  Changed LIB$ to CHR_ (SKL@JACH)

      INCLUDE 'CHR_ERR'


	INTEGER ICIC
	INTEGER ICNB
	INTEGER ICST
	INTEGER IVAR
	INTEGER J
	INTEGER STATUS
	INTEGER TEMP_NUMH
	INTEGER TEMP_NUMS
	INTEGER TEMP_NUMT
        INTEGER NCHAR

	CHARACTER*1  CHAR_NUMH
	CHARACTER*1  CHAR_NUMT
	CHARACTER*1  CHAR_NUMS
	CHARACTER*80 NUM(3)

* calculate start column value number for loop

	IVAR = ICST - ICIC

* initialize position counting variable

	J=0

* loop to calculate the hundreds, tens and single digits of the column numbers

	DO WHILE ( J .LT. ICNB)

* increment the column number counting variable

	  J = J + 1

* increment the current column number variable

	  IVAR = IVAR + ICIC

* calculate the numerical hundreds column number

	  TEMP_NUMH = IFIX( IVAR/100.0)

* test if this is a single digit

	  IF( TEMP_NUMH .GE. 0 .AND. TEMP_NUMH .LE. 9) THEN

* convert numeric hundreds column number to a character

            CALL CHR_ITOC( TEMP_NUMH, CHAR_NUMH, NCHAR )
	    NUM( 1)( J:J) = CHAR_NUMH
	  ELSE

* if too many digits in the column hundreds then put a * there instead

	    NUM( 1)( J:J) = '*'
	  END IF

* calculate the numerical tens column number

	  TEMP_NUMT = IFIX(( IVAR - ( TEMP_NUMH*100.0))/10.0)

* test if this is a single digit

	  IF( TEMP_NUMT .GE. 0 .AND. TEMP_NUMT .LE. 9) THEN

* convert numeric tens column number to a character

            CALL CHR_ITOC( TEMP_NUMT, CHAR_NUMT, NCHAR )
	    NUM( 2)( J:J) = CHAR_NUMT
	  ELSE

* if too many digits in the column tens then put a * there instead

	    NUM( 2)( J:J) = '*'
	  END IF

* calculate the numerical single column number

	  TEMP_NUMS = IFIX( ( IVAR - ( TEMP_NUMH*100.0) - ( TEMP_NUMT*10.0)))

* test if this is a single digit

	  IF( TEMP_NUMS .GE. 0 .AND. TEMP_NUMS .LE. 9) THEN

* convert numeric single column number to a character

            CALL CHR_ITOC( TEMP_NUMS, CHAR_NUMS, NCHAR )
	    NUM( 3)( J:J) = CHAR_NUMS
	  ELSE

* if too many digits in the column singles then put a * there instead

	    NUM( 3)( J:J) = '*'
	  END IF
	END DO

* write out the column number strings

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	CALL MSG_SETC( 'HUN', NUM( 1)( 1:ICNB))
	CALL MSG_OUT( 'MESSAGE', '^HUN', STATUS)

	CALL MSG_SETC( 'TEN', NUM( 2)( 1:ICNB))
	CALL MSG_OUT( 'MESSAGE', '^TEN', STATUS)

	CALL MSG_SETC( 'SIN', NUM( 3)( 1:ICNB))
	CALL MSG_OUT( 'MESSAGE', '^SIN', STATUS)

	CALL MSG_OUT( 'BLANK', ' ', STATUS)

	END
