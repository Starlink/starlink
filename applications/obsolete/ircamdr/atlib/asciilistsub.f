	SUBROUTINE ASCIILISTSUB( NX, NY, ARRIN, OUTFORM, LUN, STATUS)

	INCLUDE 'SAE_PAR'

	INTEGER
     :	  NX,
     :	  NY,
     :	  STATUS,
     :	  OUTFORM,
     :	  LUN,
     :	  J,
     :	  K

	REAL
     :	  ARRIN( NX, NY)

	IF( STATUS .NE. SAI__OK) RETURN

	IF( OUTFORM .EQ. 1) THEN

	  WRITE( LUN, *) NX, NY

	  DO J = 1, NY

	    DO K = 1, NX

	      WRITE( LUN, *) ARRIN( K, J)

	    END DO

	  END DO

	ELSE

	  DO J = 1, NY

	    DO K = 1, NX

	      WRITE( LUN, *) K, J, ARRIN( K, J)

	    END DO

	  END DO

	END IF

	END
