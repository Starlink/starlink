	SUBROUTINE FITSWARRAY( TAPEORDISK, MTCHAN, LUNO, BZERO, BSCALE, ARRAY,
     :	                       DIMSX, DIMSY, SWAPBYTES)

	IMPLICIT NONE

	BYTE PIXBYTES( 4), OUTBYTES( 4)

	INTEGER MTCHAN, LUNO, DIMSX, DIMSY
	INTEGER JPIC, J1, J2, JTAPE, TEMPVAR, OUTVAR, NUMPIXELS

	PARAMETER ( NUMPIXELS = 720)

	INTEGER DATA_32BITS( NUMPIXELS)
	INTEGER MTWR, MTREAD

	REAL ARRAY( DIMSX, DIMSY), BSCALE, BZERO

	CHARACTER*( *) TAPEORDISK

	LOGICAL SWAPBYTES

	EQUIVALENCE ( TEMPVAR, PIXBYTES)
	EQUIVALENCE ( OUTVAR,  OUTBYTES)

*      Initialize counting variables
	J1 = 0
	J2 = 0

*      Loop to scan through data array in Y
	DO WHILE( J2 .LE. DIMSY)

*        Loop to scan through data array in X
	  DO JPIC = 1, NUMPIXELS

	    J1 = J1 + 1
	    IF( MOD( J1, DIMSX) .EQ. 1) THEN
	      J1 = 1
	      J2 = J2 + 1
	    END IF

	    IF( J2 .LE. DIMSY) THEN

*            Scale the data array pixel by bscale and bzero
	      IF( BSCALE .NE. 0.0) THEN
	        TEMPVAR = IFIX(( ARRAY( J1, J2) - BZERO)/BSCALE + 0.5)
	      ELSE
	        TEMPVAR = 1E6
	      END IF

*            Test if users wants bytes swapped in tape/disk image
	      IF( SWAPBYTES) THEN
	        OUTBYTES( 1) = PIXBYTES( 4)
	        OUTBYTES( 2) = PIXBYTES( 3)
	        OUTBYTES( 3) = PIXBYTES( 2)
	        OUTBYTES( 4) = PIXBYTES( 1)
	      ELSE
	        OUTBYTES( 1) = PIXBYTES( 1)
	        OUTBYTES( 2) = PIXBYTES( 2)
	        OUTBYTES( 3) = PIXBYTES( 3)
	        OUTBYTES( 4) = PIXBYTES( 4)
	      END IF

	      DATA_32BITS( JPIC) = OUTVAR
	    END IF
	  END DO

*        At end of block write to tape/disk
	  IF( TAPEORDISK( 1:1) .EQ. 'T') THEN
	    JTAPE = MTWR( MTCHAN, DATA_32BITS, 5, 2880)
	  ELSE
	    WRITE( LUNO) DATA_32BITS
	  END IF
	END DO

*   Write terminating tapemarks and backstep one tapemark
	IF( TAPEORDISK( 1:1) .EQ. 'T') THEN
	  JTAPE = MTWR( MTCHAN, DATA_32BITS, 0, 1)
	  JTAPE = MTWR( MTCHAN, DATA_32BITS, 0, 1)
	  JTAPE = MTREAD( MTCHAN, DATA_32BITS, 8, -1)
	END IF

	END
