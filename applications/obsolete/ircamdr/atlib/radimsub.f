	SUBROUTINE RADIMSUB( NXI, NYI, INARR, NXO, NYO, OUTARR, X, Y, R)

	IMPLICIT NONE

	INCLUDE 'SAE_PAR'

	INTEGER
     :    NXI,
     :	  NYI,
     :    NXO,
     :	  NYO,
     :    R,
     :	  X,
     :	  Y,
     :	  NPTS,
     :	  J,
     :	  I,
     :	  STATUS

	REAL
     :	  INARR( NXI, NYI),
     :	  OUTARR( NXO, NYO),
     :	  DATA( 10000),
     :	  THETA,
     :	  XC,
     :	  YC,
     :	  XP,
     :	  YP,
     :	  PI

	PARAMETER ( PI = 3.14159265)

	DO J = 1, NYO

	  DO I = 1, NXO

	    OUTARR( I, J) = 0.0

	  END DO

	END DO

	XC = REAL( X)
	YC = REAL( Y)

	DO J = 1, NXO

	  THETA = ( J-1)*PI/180.0*2

	  XP = XC + R*SIN( THETA)

	  YP = YC + R*COS( THETA)

	  WRITE( 11, *) ( J-1)*2,XC,YC,XP,YP

	  CALL RADIM_DATA( NXI, NYI, INARR, XC, YC, XP, YP, DATA, NPTS)

	  DO I = 1, MIN( NPTS, NYO)

	    OUTARR( J, I) = DATA( I)

	  END DO

	END DO

	END
