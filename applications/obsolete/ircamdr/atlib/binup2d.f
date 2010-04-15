	SUBROUTINE BINUP2D( NXI, NYI, IMAGE_IN, BINX, BINY, NXO, NYO,
     :	                    IMAGE_OUT, IMAGE_OUT2, STATUS)

	IMPLICIT NONE

	INTEGER BINX
	INTEGER BINY
	INTEGER IEN1
	INTEGER IEN2
	INTEGER IST1
	INTEGER IST2
	INTEGER I
	INTEGER J
	INTEGER K
	INTEGER L
	INTEGER NXI
	INTEGER NYI
	INTEGER NXO
	INTEGER NYO
	INTEGER STATUS

	REAL IMAGE_IN( NXI, NYI)
	REAL IMAGE_OUT( NXO, NYO)
	REAL IMAGE_OUT2( NXO, NYO)
	REAL*8 DUMBO
	REAL*8 DUMBO2
	REAL*8 STD

*      set the output image to zero
	DO J = 1, NYO
	  DO K = 1, NXO
	    IMAGE_OUT( K, J) = 0.0
	    IMAGE_OUT2( K, J) = 0.0
	  END DO
	END DO

*      define start of blocking
	IST2 = 1
	IEN2 = BINY

*      loop for y scan of output image
	DO I = 1, NYO
	  IST1 = 1
	  IEN1 = BINX

*        loop for x scan of output image
	  DO J = 1, NXO

*          loops to scan through the input image pixels/output pixel
	    DUMBO = 0.0D0
	    DUMBO2 = 0.0D0
	    DO K = IST2, IEN2
	      DO L = IST1, IEN1

*              test if the current positions are within the input and output
*              images
	        IF( I .GE. 1 .AND. I .LE. NYO .AND.
     :	            J .GE. 1 .AND. J .LE. NXO .AND.
     :	            K .GE. 1 .AND. K .LE. NYI .AND.
     :	            L .GE. 1 .AND. L .LE. NXI) THEN

*                sum up the output pixel in a double precision variable to check
*                for overflows
	          DUMBO = DUMBO + DBLE( IMAGE_IN( L, K))
	          DUMBO2 = DUMBO2 + DBLE( IMAGE_IN( L, K)**2)
	        END IF
	      END DO
	    END DO

*          test for overflows of single precision variable
	    IF( ABS( DUMBO) .LT. 1.0E20) THEN

*            set the output pixel
	      IMAGE_OUT( J, I) = SNGL( DUMBO)/( BINX*BINY)

*            calculate std over bin
	      STD = DUMBO2/( BINX*BINY) - ( DUMBO/( BINX*BINY))**2
	      IF( STD .GE. 0.0D0) THEN
	        IMAGE_OUT2( J, I) = SQRT( SNGL( STD))
	      ELSE
	        IMAGE_OUT2( J, I) = -999
	      END IF
	    ELSE

*            if there is going to be an overflow
	      IMAGE_OUT( J, I) = -999
	      IMAGE_OUT2( J, I) = -999
	    END IF

*          increment the X input pixel scanning variables

	    IST1 = IST1 + BINX
	    IEN1 = IEN1 + BINX
	  END DO

*        increment the Y input pixel scanning variables

	  IST2 = IST2 + BINY
	  IEN2 = IEN2 + BINY
	END DO

	END
