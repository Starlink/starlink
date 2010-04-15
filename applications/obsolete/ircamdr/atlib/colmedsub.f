	SUBROUTINE COLMEDSUB( DIMSX, DIMSY, ARRIN, ODIMSX, ODIMSY, ARROUT,
     :	                      EXCLREG, XST, YST, XEN, YEN, STATUS)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMSX,
     :	  DIMSY,
     :	  ODIMSX,
     :	  ODIMSY,
     :	  STATUS,
     :	  J,
     :	  K,
     :	  XST,
     :	  YST,
     :	  XEN,
     :	  YEN,
     :	  NUMPIX

	REAL
     :	  ARROUT( ODIMSX, ODIMSY),
     :	  ARRIN( DIMSX, DIMSY),
     :	  MEAN,
     :	  MEDIAN,
     :	  MODE,
     :	  SUM,
     :	  VALMIN,
     :	  VALMAX

	REAL*8
     :	  DATA( 3000)

	LOGICAL
     :	  EXCLREG

*      scan through all the input pixels IN X ...

	DO J = 1, DIMSX

*        tell user which column is being processed ...

	  IF( IFIX( J/20.0+0.5)*20 .EQ. J) THEN

	    CALL MSG_SETI( 'J', J)
	    CALL MSG_OUT( 'MESSAGE', 'Processing COLUMN ^J ...', STATUS)

	  END IF

	  NUMPIX = 0

	  DO K = 1, DIMSY

*          test if the exclusion region has been specified and whether pixel
*          is in it or out of it ...

	    IF( EXCLREG) THEN

	      IF( K .LT. YST .OR. K .GT. YEN) THEN

	        NUMPIX = NUMPIX + 1

	        DATA( NUMPIX) = DBLE( ARRIN( J, K))

	      ELSE IF( J .LT. XST .OR. J .GT. XEN) THEN

	        NUMPIX = NUMPIX + 1

	        DATA( NUMPIX) = DBLE( ARRIN( J, K))

	      END IF

	    ELSE

	      NUMPIX = NUMPIX + 1

	      DATA( NUMPIX) = DBLE( ARRIN( J, K))

	    END IF

	  END DO

*        Call subroutine to order the column values
	  CALL PDA_QSAD( NUMPIX, DATA )

*        call subroutine to find median for the input data

	  CALL MED3D_CALMEDSUB( NUMPIX, DATA, VALMAX, VALMIN, SUM, MEAN,
     :	                        MEDIAN, MODE)

*        set the output pixel with the median value

	  ARROUT( J, ODIMSY) = MEDIAN

	END DO

	END
