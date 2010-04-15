	SUBROUTINE INTEGER_PICTURE(  NX,
     :	                             NY,
     :	                             ARR,
     :	                             LST,
     :	                             LNB,
     :	                             LIC,
     :	                             CST,
     :	                             CNB,
     :	                             CIC,
     :	                             SUBT,
     :	                             NORM,
     :	                             DATA)

	IMPLICIT NONE

*     14-JUL-1994  Changed LIB$ to CHR_ (SKL@JACH)

        INCLUDE 'CHR_ERR'


	INTEGER CEN
	INTEGER CIC
	INTEGER CNB
	INTEGER COLUMN_COUNT
	INTEGER CST
	INTEGER I
	INTEGER LEN
	INTEGER LIC
	INTEGER LINE_COUNT
	INTEGER LNB
	INTEGER LST
	INTEGER NX
	INTEGER NY
	INTEGER STATUS
	INTEGER SUBT
        INTEGER NCHAR

	REAL ARR( NX, NY)
	REAL NORM
	REAL VAL

	CHARACTER*80 DATA
	CHARACTER*3  LINE_CHAR
	CHARACTER*80 LINE_OUT

*
* calculate start and end lines of write
*
	LINE_COUNT = LST - LIC

	LEN = LST + LIC*( LNB - 1)
*
* loop to write out all lines
*
	DO WHILE ( LINE_COUNT .LT. LEN)
*
* calculate start column
*
	  COLUMN_COUNT = CST - CIC

* Initialize the character position variable to zero

	  I = 0

* Setup the end of the column scan

	  CEN = CST + ( CIC*( CNB - 1))

* Increment the line count variable

	  LINE_COUNT = LINE_COUNT + LIC

* Loop until the end of the column scan

	  DO WHILE ( COLUMN_COUNT .LT. CEN)

* Increment the column number variable

	    COLUMN_COUNT = COLUMN_COUNT + CIC

* Increment the character position variable

	    I = I + 1

* Calculate the value of the pixel from the data,offset and scale

	    VAL = 16.0*( ARR( COLUMN_COUNT, LINE_COUNT) - SUBT)/NORM

!	    type *,arr(column_count, line_count), val

* Test which character the data represents and set it in the output buffer

	    IF( VAL .GE. -0.5 .AND. VAL .LT. 15.5) THEN

	      IF( VAL .GE. -0.5 .AND. VAL .LT. 0.5) THEN
	        DATA( I:I) = '0'
	      ELSE IF( VAL .GE. 0.5 .AND. VAL .LT. 1.5) THEN
	        DATA( I:I) = '1'
	      ELSE IF( VAL .GE. 1.5 .AND. VAL .LT. 2.5) THEN
	        DATA( I:I) = '2'
	      ELSE IF( VAL .GE. 2.5 .AND. VAL .LT. 3.5) THEN
	        DATA( I:I) = '3'
	      ELSE IF( VAL .GE. 3.5 .AND. VAL .LT. 4.5) THEN
	        DATA( I:I) = '4'
	      ELSE IF( VAL .GE. 4.5 .AND. VAL .LT. 5.5) THEN
	        DATA( I:I) = '5'
	      ELSE IF( VAL .GE. 5.5 .AND. VAL .LT. 6.5) THEN
	        DATA( I:I) = '6'
	      ELSE IF( VAL .GE. 6.5 .AND. VAL .LT. 7.5) THEN
	        DATA( I:I) = '7'
	      ELSE IF( VAL .GE. 7.5 .AND. VAL .LT. 8.5) THEN
	        DATA( I:I) = '8'
	      ELSE IF( VAL .GE. 8.5 .AND. VAL .LT. 9.5) THEN
	        DATA( I:I) = '9'
	      ELSE IF( VAL .GE. 9.5 .AND. VAL .LT. 10.5) THEN
	        DATA( I:I) = 'A'
	      ELSE IF( VAL .GE. 10.5 .AND. VAL .LT. 11.5) THEN
	        DATA( I:I) = 'B'
	      ELSE IF( VAL .GE. 11.5 .AND. VAL .LT. 12.5) THEN
	        DATA( I:I) = 'C'
	      ELSE IF( VAL .GE. 12.5 .AND. VAL .LT. 13.5) THEN
	        DATA( I:I) = 'D'
	      ELSE IF( VAL .GE. 13.5 .AND. VAL .LT. 14.5) THEN
	        DATA( I:I) = 'E'
	      ELSE IF( VAL .GE. 14.5 .AND. VAL .LT. 15.5) THEN
	        DATA( I:I) = 'F'
	      END IF

	    ELSE IF( VAL .LT. -0.5) THEN
	      DATA( I:I) = '-'
	    ELSE IF( VAL .GT. 15.5) THEN
	      DATA(I:I)='+'
	    END IF
	  END DO
*
* set the variable for outputting the line of integer pixels
*
	  LINE_OUT = DATA( 1:CNB)
*
* convert line count to character string of three elements
*
          CALL CHR_ITOC( LINE_COUNT, LINE_CHAR, NCHAR )
*
* type out curent line of data
*
	  CALL MSG_SETC( 'COUNT', LINE_CHAR( 1:3))
	  CALL MSG_SETC( 'VALS', LINE_OUT)
	  CALL MSG_OUT( 'MESSAGE', '^VALS  ^COUNT', STATUS)

	END DO

	END
