	SUBROUTINE DEFGRADSUB( DIMSX, DIMSY, ARRIN, ODIMSX, ODIMSY,
     :	                       ARROUT, COLROW, NUMCOL, MEDIAN1,
     :	                       MEDIAN2, STATUS)

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
     :	  NUMCOL,
     :	  NUMPIX1,
     :	  NUMPIX2

	REAL
     :	  ARROUT( ODIMSX, ODIMSY),
     :	  ARRIN( DIMSX, DIMSY),
     :	  MEAN1,
     :	  MEDIAN1,
     :	  MODE1,
     :	  SUM1,
     :	  VALMIN1,
     :	  VALMAX1,
     :	  MEAN2,
     :	  MEDIAN2,
     :	  MODE2,
     :	  SUM2,
     :	  VALMIN2,
     :	  VALMAX2

	REAL*8
     :	  DATA( 3000)

	CHARACTER*(*)
     :	  COLROW

*      scan through all the input pixels at start of image ...

	NUMPIX1 = 0

	IF( COLROW( 1:1) .EQ. 'R') THEN

	  DO J = 1, NUMCOL

	    DO K = 1, DIMSX

	      NUMPIX1 = NUMPIX1 + 1

	      DATA( NUMPIX1) = DBLE( ARRIN( K, J))

	    END DO

	  END DO

	ELSE

	  DO J = 1, DIMSY

	    DO K = 1, NUMCOL

	      NUMPIX1 = NUMPIX1 + 1

	      DATA( NUMPIX1) = DBLE( ARRIN( K, J))

	    END DO

	  END DO

	END IF

*      call subroutine to order the column values

	CALL PDA_QSAD( NUMPIX1, DATA )

*      call subroutine to find median for the input data

	CALL MED3D_CALMEDSUB( NUMPIX1, DATA, VALMAX1, VALMIN1, SUM1, MEAN1,
     :	                      MEDIAN1, MODE1)

*      scan through all the input pixels at END of image ...

	NUMPIX2 = 0

	IF( COLROW( 1:1) .EQ. 'R') THEN

	  DO J = DIMSY-NUMCOL+1, DIMSY

	    DO K = 1, DIMSX

	      NUMPIX2 = NUMPIX2 + 1

	      DATA( NUMPIX2) = DBLE( ARRIN( K, J))

	    END DO

	  END DO

	ELSE

	  DO J = 1, DIMSY

	    DO K = DIMSX-NUMCOL+1, DIMSX

	      NUMPIX2 = NUMPIX2 + 1

	      DATA( NUMPIX2) = DBLE( ARRIN( K, J))

	    END DO

	  END DO

	END IF

*      call subroutine to order the column values

	CALL PDA_QSAD( NUMPIX2, DATA )

*      call subroutine to find median for the input data

	CALL MED3D_CALMEDSUB( NUMPIX2, DATA, VALMAX2, VALMIN2, SUM2, MEAN2,
     :	                      MEDIAN2, MODE2)

*      interpolate from start to end using median values found

	DO J = 1, ODIMSY

	  DO K = 1, ODIMSX

	    IF( COLROW( 1:1) .EQ. 'R') THEN

	      ARROUT( K, J) = MEDIAN1 + ( MEDIAN2-MEDIAN1)*( J-1)/( ODIMSY-1)

	    ELSE

	      ARROUT( K, J) = MEDIAN1 + ( MEDIAN2-MEDIAN1)*( K-1)/( ODIMSX-1)

	    END IF

	  END DO

	END DO

	END
