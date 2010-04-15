	SUBROUTINE MAKEGLITCHSUB( DIMSX, DIMSY, ARRIN, OUTFILE, STATUS)

	IMPLICIT NONE

*     HISTORY
*     14-JUL-1994  Changed LIB$ to FIO_, removed call to LIB$DATE_TIME
*                  (VAX specific) (SKL@JACH)
*      9-AUG-2004  Use FIO for open and close (TIMJ@JACH)

        INCLUDE 'FIO_PAR'
	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  STATUS,
     :	  LUN,
     :	  J,
     :	  K,
*     :	  L1,
     :	  NUMPIX,
     :	  BAD_NUMBER

	REAL
     :	  ARRIN( DIMSX, DIMSY)

	CHARACTER*( *)
     :	  OUTFILE

*	CHARACTER*80
*     :	  DATETIME

*      initialize the number of bad pixels found
	NUMPIX = 0

*      Open output file
	CALL FIO_OPEN( OUTFILE, 'WRITE','LIST',0, LUN, STATUS)

*      write header line to bad pixel file
*	CALL LIB$DATE_TIME( DATETIME)
*	CALL STR$TRIM( DATETIME, DATETIME, L1)
*	WRITE( LUN, '(A)') 'Bad pixel list : '//
*     :                     DATETIME( 1:L1)

*      calculate mean value in FULL image
	DO J = 1, DIMSY
	  DO K = 1, DIMSX
	    IF( ARRIN( K, J) .EQ. 1.0000) THEN
	      WRITE( LUN, *) K, J
	      BAD_NUMBER = BAD_NUMBER + 1
	    END IF
	  END DO
	END DO

*      tell user mean and sigma and maximum and minimum
	CALL MSG_SETI( 'NUMBAD', BAD_NUMBER)
	CALL MSG_OUT( 'MESS',
     :	  'Number of bad pixels found = ^NUMBAD', STATUS)

*      close output file and release lun
	CALL FIO_CLOSE( LUN, STATUS )

	END
