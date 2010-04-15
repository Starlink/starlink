*+DTRANSQ Transforms one frame to another, returns angles and 3 vector
	SUBROUTINE DTRANSQ (RA, DEC, CTOS, AZ, EL, V)
	DOUBLE PRECISION RA		!input	Radians
	DOUBLE PRECISION DEC		!input	Radians
	DOUBLE PRECISION CTOS(3,3)	!input	Celestial - other matrix
	DOUBLE PRECISION AZ		!output	Radians
	DOUBLE PRECISION EL		!output	Radians
	DOUBLE PRECISION V(3)		!output 3 vector of AZ,EL
*Author	Clive Page	1988 Sept 27
*- Modified to return 3 vector M. Denby
*  89 May: Mods by JDE to trap cases where ASIN, ATAN fail
* P McGale May 95 - UNIX mods

	DOUBLE PRECISION V1, V2, V3, CRA, CDEC, SRA, TWOPI
	PARAMETER (TWOPI = 6.28318530717957D0)

	sra = sin(ra)
	cra = cos(ra)
	v3  = sin(dec)
	cdec = cos(dec)
*	CALL MTH$DSINCOS(RA, SRA, CRA)
*	CALL MTH$DSINCOS(DEC, V3, CDEC)

	V1 = CRA * CDEC
	V2 = SRA * CDEC
	DO J = 1,3
	  V(J) = V1 * CTOS(1,J) + V2 * CTOS(2,J) + V3 * CTOS(3,J)
	END DO

*	Case of both ATAN2 arguments zero
	IF(V(2).EQ.0.0D0 .AND. V(1).EQ.0.0D0)THEN
		AZ = 0.0D0
	ELSE
		AZ = ATAN2(V(2), V(1))
	END IF

	IF(AZ .LT. 0.0D0) AZ = AZ + TWOPI

*	Ensure  -1 < V(3) < +1
	V(3) = MAX(V(3),-1.0D0)
	V(3) = MIN(V(3),+1.0D0)
* (I would have done: if abs(..) gt.1. then v(3) = sign(1.0,v(3)) - MJR)

	EL = ASIN(V(3))

	END
