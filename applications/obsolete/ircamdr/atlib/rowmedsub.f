	SUBROUTINE ROWMEDSUB( DIMSX, DIMSY, ARRIN, ODIMSX, ODIMSY, ARROUT,
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

*      scan through all the input pixels IN Y ...

	DO J = 1, DIMSY

*        tell user which row is being processed ...

	  IF( IFIX( J/20.0+0.5)*20 .EQ. J) THEN

	    CALL MSG_SETI( 'J', J)
	    CALL MSG_OUT( 'MESSAGE', 'Processing ROW ^J ...', STATUS)

	  END IF

	  NUMPIX = 0

	  DO K = 1, DIMSX

*          test if the exclusion region has been specified and whether pixel
*          is in it or out of it ...

	    IF( EXCLREG) THEN

	      IF( K .LT. XST .OR. K .GT. XEN) THEN

	        NUMPIX = NUMPIX + 1

	        DATA( NUMPIX) = DBLE( ARRIN( J, K))

	      ELSE IF( J .LT. YST .OR. J .GT. YEN) THEN

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

	  ARROUT( ODIMSX, J) = MEDIAN

	END DO

	END
