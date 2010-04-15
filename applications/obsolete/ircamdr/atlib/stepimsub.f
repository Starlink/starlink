

	SUBROUTINE STEPIMSUB( DIMS1, DIMS2, ARRIN, ARROUT, SNUM, SBAS,
     :	                      SINT, XMAX, XMIN, STATUS)

*  HISTORY
*    11-Aug-1994 Changed DIM arguments so that routine will compile (SKL@JACH)
*
	INCLUDE 'SAE_PAR'

	INTEGER
     :	  DIMS1,
     :	  DIMS2,
     :	  SNUM,
     :	  STATUS,
     :	  J,
     :	  K,
     :	  L

	REAL
     :	  ARRIN( DIMS1, DIMS2),
     :    ARROUT( DIMS1, DIMS2),
     :	  SBAS,
     :	  SINT,
     :	  XMIN,
     :	  XMAX,
     :	  TOP,
     :	  BOTTOM,
     :	  MIDDLE

	IF( STATUS .NE. SAI__OK) RETURN

	DO J = 1, DIMS2

	  DO K = 1, DIMS1

	    IF( ARRIN( K, J) .LT. SBAS) THEN

	      ARROUT( K, J) = XMIN

	    ELSE IF( ARRIN( K, J) .GT. ( SBAS+SNUM*SINT)) THEN

	      ARROUT( K, J) = XMAX

	    ELSE

	      L = 0

	      DO WHILE ( L .LE. SNUM)

	        L = L + 1

	        BOTTOM = SBAS + ( L-1)*SINT
	        TOP = SBAS + L*SINT
	        MIDDLE = ( TOP + BOTTOM)/2.0

	        IF( ARRIN( K, J) .GE. BOTTOM .AND.
     :	            ARRIN( K, J) .LE. TOP) THEN

	          ARROUT( K, J) = MIDDLE

	          GOTO 100

	        END IF

	      END DO

  100	      CONTINUE

	    END IF

	  END DO

	END DO

	END
