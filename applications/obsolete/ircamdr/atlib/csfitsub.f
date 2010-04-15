
	SUBROUTINE CSFITSUB( NX, NY, ARRINP, ARRINT, XST, YST, XEN, YEN,
     :	                     AXST, AYST, AXEN, AYEN, POLMIN, OUTFILE,
     :	                     ARROUTT, RES, XRES, YRES, STATUS)

	IMPLICIT NONE

*     HISTORY
*     14-JUL-1994  Changed LIB$ to FIO_ (SKL@JACH)

        INCLUDE 'FIO_PAR'


	INCLUDE 'SAE_PAR'

	INTEGER NX, NY, XST, YST, XEN, YEN, STATUS
	INTEGER AXST, AYST, AXEN, AYEN
	INTEGER I, J, K, L, NPIX, XRES, YRES
	INTEGER LUN, COUNTER

	REAL*8 SUMSQD, SUMD, STD
	REAL ARRINP( NX, NY), ARRINT( NX, NY), RES, XOFF, YOFF, POLMIN
	REAL ARROUTT( NX, NY)
	REAL ACTTHETA, CALTHETA, DEVTHETA
	REAL PI

	CHARACTER*( *) OUTFILE

*      Define PI
	PI = 4.0*ATAN( 1.0)

*      Initialize the residual variable to something LARGE!
	RES = 1.0E30

*      Initialize number of centre position considered
	COUNTER = 0

*      Get lun for output list file and open file
	CALL FIO_GUNIT( LUN, STATUS )
	OPEN( UNIT=LUN, FILE=OUTFILE, STATUS='NEW')

*      Write header line to output file
	WRITE( LUN, '(A)')
     :	'## CSFIT - Pixel, Standard deviation, '//
     :  'number pixels, x-pixel, y-pixel'

*      Loops to scan through array specified setting each pixel as centre
*      of calculated centro-symmetric pattern
	DO I = AYST, ( AYST+AYEN-1)
	  DO J = AXST, ( AXST+AXEN-1)

*          Initialize number of pixels in CS pattern for particular centre
	    NPIX = 0

*          Tell user how far got...
	    IF( J .EQ. AXST .AND. INT( I/2.0)*2 .EQ. I) THEN
	      CALL MSG_SETI( 'NB', I)
	      CALL MSG_OUT( 'MESS', 'Scanning ROW ^NB', STATUS)
	    END IF

*          Initialize sum-of-squares variable for each centre pixel
	    SUMSQD = 0.0D0
	    SUMD = 0.0D0

*          Increment number of centre positions variable
	    COUNTER = COUNTER + 1

*          Loops to scan through defined CS pattern area
	    DO K = YST, ( YST+YEN-1)
	      DO L = XST, ( XST+XEN-1)

*              Calculate x and y offset of pattern point from current centre
	        XOFF = REAL( L-J)
	        YOFF = REAL( K-I)

*              Only consider pixel if polarization is above specified
*              input level
	        IF( ARRINP( L, K) .GT. POLMIN) THEN

*                Get the actual position angle (in degs.) from data array
	          ACTTHETA = ARRINT( L, K)

*                Test if current pixel different from centre pixel
	          IF( ( L-J) .NE. 0 .OR. ( K-I) .NE. 0) THEN

*                  Increment number of pixels in CS pattern for this
*                  centre variable
	            NPIX = NPIX + 1

*                  Test if current pixel different from centre pixel
	            IF( ( L-J) .NE. 0) THEN

*                    Calculate position angle from offsets
	              CALTHETA = ATAN( ABS( YOFF)/ABS( XOFF))*360.0/( 2*PI)

*                    Put calculated position angle in right quadrant
	              IF( XOFF .LT. 0.0 .AND. YOFF .GT. 0.0) THEN
	                CALTHETA = 180.0 - ABS( CALTHETA)
	              ELSE IF( XOFF .GT. 0.0 .AND. YOFF .LT. 0.0) THEN
	                CALTHETA = 180.0 - ABS( CALTHETA)
	              END IF

*                  If in x-centre column then set position angle to 90 deg.
	            ELSE
	              CALTHETA = 90.0
	            END IF

*                  Calculate deviation from centro-symmetry
	            IF( CALTHETA .GE. 0.0) THEN
	              DEVTHETA = CALTHETA - ACTTHETA

*                    If deviation outside -90.0 to +90.0 correct for wrap
	              IF( DEVTHETA .GT. 90.0) THEN
	                DEVTHETA = DEVTHETA - 180.0
	              ELSE IF( DEVTHETA .LT. -90.0) THEN
	                DEVTHETA = DEVTHETA + 180.0
	              END IF
	            ELSE
	              DEVTHETA = 0.0
	            END IF

*                  Calculate sum-of-squares of deviation over whole pattern
*                  area for curent centre pixel
	            SUMSQD = SUMSQD + DEVTHETA**2
	            SUMD = SUMD + DEVTHETA
	          END IF
	        END IF

	      END DO
	    END DO

*          Use sum-of-squares to calculated standard deviation
	    IF( SUMSQD .GE. 0.0D0) THEN
	      IF( NPIX .GT. 0) THEN
	        STD = ( SUMSQD/NPIX)-( SUMD/NPIX)**2
	        IF( STD .GE. 0.0D0) THEN
	          STD = SQRT( STD)
	        ELSE
	          STD = -999
	        END IF
	      ELSE
	        STD = -999
	      END IF
	    ELSE
	      STD = -999
	    END IF

!	type *, j, i, sngl( std)

*          Test if current sum-of-squares is minimum and set output
*          variables if it is
	    IF( STD .LT. RES) THEN
	      RES = SNGL( STD)
	      XRES = J
	      YRES = I
	    END IF

	  END DO
	END DO

*      Initialize number of centre position considered
	COUNTER = AXST-1

*      Write line to output file to tell this is x scan
	WRITE( LUN, '(A)')
     :	  '## Scan through calculated centre in X'

*      Scan through optimum pixel in x and re-calculate statistic
	I = YRES
	DO J = AXST, AXEN

*        Initialize number of pixels in CS pattern for particular centre
	  NPIX = 0

*        Initialize sum-of-squares variable for each centre pixel
	  SUMSQD = 0.0D0
	  SUMD = 0.0D0

*        Increment number of centre positions variable
	  COUNTER = COUNTER + 1

*        Loops to scan through defined CS pattern area
	  DO K = YST, YEN
	    DO L = XST, XEN

*            Calculate x and y offset of pattern point from current centre
	      XOFF = REAL( L-J)
	      YOFF = REAL( K-I)

*            Only consider pixel if polarization is above specified
*            input level
	      IF( ARRINP( L, K) .GT. POLMIN) THEN

*              Get the actual position angle (in degs.) from data array
	        ACTTHETA = ARRINT( L, K)

*              Test if current pixel different from centre pixel
	        IF( ( L-J) .NE. 0 .OR. ( K-I) .NE. 0) THEN

*                Increment number of pixels in CS pattern for this
*                centre variable
	          NPIX = NPIX + 1

*                Test if current pixel different from centre pixel
	          IF( ( L-J) .NE. 0) THEN

*                  Calculate position angle from offsets
	            CALTHETA = ATAN( ABS( YOFF)/ABS( XOFF))*360.0/( 2*PI)

*                  Put calculated position angle in right quadrant
	            IF( XOFF .LT. 0.0 .AND. YOFF .GT. 0.0) THEN
	              CALTHETA = 180.0 - ABS( CALTHETA)
	            ELSE IF( XOFF .GT. 0.0 .AND. YOFF .LT. 0.0) THEN
	              CALTHETA = 180.0 - ABS( CALTHETA)
	            END IF

*                If in x-centre column then set position angle to 90 deg.
	          ELSE
	            CALTHETA = 90.0
	          END IF

*                Calculate deviation from centro-symmetry
	          IF( CALTHETA .GE. 0.0) THEN
	            DEVTHETA = CALTHETA - ACTTHETA

*                  If deviation outside -90.0 to +90.0 correct for wrap
	            IF( DEVTHETA .GT. 90.0) THEN
	              DEVTHETA = DEVTHETA - 180.0
	            ELSE IF( DEVTHETA .LT. -90.0) THEN
	              DEVTHETA = DEVTHETA + 180.0
	            END IF
	          ELSE
	            DEVTHETA = 0.0
	          END IF

*                Calculate sum-of-squares of deviation over whole pattern
*                area for curent centre pixel
	          SUMSQD = SUMSQD + DEVTHETA**2
	          SUMD = SUMD + DEVTHETA
	        END IF
	      END IF

	    END DO
	  END DO

*        Scale sum-of-squares to calculated standard deviation
	  IF( SUMSQD .GE. 0.0D0) THEN
	    IF( NPIX .GT. 0) THEN
	      STD = ( SUMSQD/NPIX)-( SUMD/NPIX)**2
	      IF( STD .GE. 0.0D0) THEN
	        STD = SQRT( STD)
	       ELSE
	        STD = -999
	      END IF
	    ELSE
	      STD = -999
	    END IF
	  ELSE
	    STD = -999
	  END IF

*        Write out sum-of-squares variable to list file
	  WRITE( LUN, *) COUNTER, SNGL( STD), NPIX, J, I

	END DO

*      Initialize number of centre position considered
	COUNTER = AYST-1

*      Write line to output file to tell this is y scan
	WRITE( LUN, '(A)')
     :	  '## Scan through calculated centre in Y'

*      Scan through optimum pixel in y and re-calculate statistic
	J = XRES
	DO I = AYST, AYEN

*        Initialize number of pixels in CS pattern for particular centre
	  NPIX = 0

*        Initialize sum-of-squares variable for each centre pixel
	  SUMSQD = 0.0D0
	  SUMD = 0.0D0

*        Increment number of centre positions variable
	  COUNTER = COUNTER + 1

*        Loops to scan through defined CS pattern area
	  DO K = YST, YEN
	    DO L = XST, XEN

*            Calculate x and y offset of pattern point from current centre
	      XOFF = REAL( L-J)
	      YOFF = REAL( K-I)

*            Only consider pixel if polarization is above specified
*            input level
	      IF( ARRINP( L, K) .GT. POLMIN) THEN

*              Get the actual position angle (in degs.) from data array
	        ACTTHETA = ARRINT( L, K)

*              Test if current pixel different from centre pixel
	        IF( ( L-J) .NE. 0 .OR. ( K-I) .NE. 0) THEN

*                Increment number of pixels in CS pattern for this
*                centre variable
	          NPIX = NPIX + 1

*                Test if current pixel different from centre pixel
	          IF( ( L-J) .NE. 0) THEN

*                  Calculate position angle from offsets
	            CALTHETA = ATAN( ABS( YOFF)/ABS( XOFF))*360.0/( 2*PI)

*                  Put calculated position angle in right quadrant
	            IF( XOFF .LT. 0.0 .AND. YOFF .GT. 0.0) THEN
	              CALTHETA = 180.0 - ABS( CALTHETA)
	            ELSE IF( XOFF .GT. 0.0 .AND. YOFF .LT. 0.0) THEN
	              CALTHETA = 180.0 - ABS( CALTHETA)
	            END IF

*                If in x-centre column then set position angle to 90 deg.
	          ELSE
	            CALTHETA = 90.0
	          END IF

*                Calculate deviation from centro-symmetry
	          IF( CALTHETA .GE. 0.0) THEN
	            DEVTHETA = CALTHETA - ACTTHETA

*                  If deviation outside -90.0 to +90.0 correct for wrap
	            IF( DEVTHETA .GT. 90.0) THEN
	              DEVTHETA = DEVTHETA - 180.0
	            ELSE IF( DEVTHETA .LT. -90.0) THEN
	              DEVTHETA = DEVTHETA + 180.0
	            END IF
	          ELSE
	            DEVTHETA = 0.0
	          END IF

*                Calculate sum-of-squares of deviation over whole pattern
*                area for curent centre pixel
	          SUMSQD = SUMSQD + DEVTHETA**2
	          SUMD = SUMD + DEVTHETA
	        END IF
	      END IF

	    END DO
	  END DO

*        Scale sum-of-squares to calculated standard deviation
	  IF( SUMSQD .GE. 0.0D0) THEN
	    IF( NPIX .GT. 0) THEN
	      STD = ( SUMSQD/NPIX)-( SUMD/NPIX)**2
	      IF( STD .GE. 0.0D0) THEN
	        STD = SQRT( STD)
	      ELSE
	        STD = -999
	      END IF
	    ELSE
	      STD = -999
	    END IF
	  ELSE
	    STD = -999
	  END IF

*        Write out sum-of-squares variable to list file
	  WRITE( LUN, *) COUNTER, SNGL( STD), NPIX, J, I

	END DO

*      Close output list file and free lun
	CLOSE( LUN)
	CALL FIO_PUNIT( LUN, STATUS )

*      Loops to scan through array creating best fit centro-symmetric
*      pattern using p image as map to where to put vectors
	DO I = 1, NY
	  DO J = 1, NX

*          Calculate x and y offset of pattern point from current centre
	    XOFF = REAL( XRES-J)
	    YOFF = REAL( YRES-I)

*          Only consider pixel if polarization is above specified
*          input level
	    IF( ARRINP( J, I) .GT. 0.01) THEN

*            Test if current pixel different from centre pixel
	      IF( ( XRES-J) .NE. 0 .OR. ( YRES-I) .NE. 0) THEN

*              Test if current pixel different from centre pixel
	        IF( ( XRES-J) .NE. 0) THEN

*                Calculate position angle from offsets
	          CALTHETA = ATAN( ABS( YOFF)/ABS( XOFF))*360.0/( 2*PI)

*                Put calculated position angle in right quadrant
	          IF( XOFF .LT. 0.0 .AND. YOFF .GT. 0.0) THEN
	            CALTHETA = 180.0 - ABS( CALTHETA)
	          ELSE IF( XOFF .GT. 0.0 .AND. YOFF .LT. 0.0) THEN
	            CALTHETA = 180.0 - ABS( CALTHETA)
	          END IF

*              If in x-centre column then set position angle to 90 deg.
	        ELSE
	          CALTHETA = 90.0
	        END IF

	        ARROUTT( J, I) = CALTHETA

	      ELSE

	        ARROUTT( J, I) = 0.0

	      END IF

	    ELSE

	      ARROUTT( J, I) = 0.0

	    END IF

	  END DO

	END DO

	END
