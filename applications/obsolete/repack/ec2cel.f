*+EC2CEL convert Ecliptic to Celstial
	SUBROUTINE EC2CEL (ELO, ELA, RA, DEC)
	IMPLICIT NONE
* Input
	REAL*8		ELO, ELA	! Ecliptic coord pair (rads)
* Output
	REAL*8		RA, DEC		! Cel coord pair (rads)
* Author M. Denby Feb-88
* P McGale Apr 95 - UNIX changes
*-

	INCLUDE		'CONSTANTS.INC'

	CALL AX_DONVRT (ELO, ELA, ETOC, RA, DEC)
	END
