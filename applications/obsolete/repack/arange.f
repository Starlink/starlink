*+ARANGE	Finds absolute range and max fractional part
	SUBROUTINE ARANGE(NPTS, ARRAY, RANGE, FRMAX)
	INTEGER NPTS		!input
	REAL ARRAY(NPTS)	!input
	REAL RANGE		!output	Largest absolute value
	REAL FRMAX		!output Maximum fractional part
*-Author	Clive Page	1990-NOV-9
	INTEGER I
	REAL RMIN, RMAX, FRAC
*
	RMIN = ARRAY(1)
	RMAX = ARRAY(1)
	FRAC = MOD(ARRAY(1),1.0)
	DO I = 1,NPTS
	    RMIN = MIN(ARRAY(I), RMIN)
	    RMAX = MAX(ARRAY(I), RMAX)
	    FRAC = MAX(FRAC, MOD(ARRAY(I),1.0))
	END DO
	RANGE = MAX(ABS(RMIN), ABS(RMAX))
	FRMAX = FRAC
	END

