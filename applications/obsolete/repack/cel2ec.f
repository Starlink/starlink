*+CEL2EC convert Celestial to ecliptic
	SUBROUTINE CEL2EC (RA, DEC, ELO, ELA)
	IMPLICIT NONE
* Input
	DOUBLE PRECISION 	RA, DEC		! Cel coords (Rads)
* Output
	DOUBLE PRECISION	ELO, ELA	! Ecl coords (Rads)
* Author M. Denby Feb-88
* P. McGale Sept 94 - UNIX changes
*-
	INCLUDE		'CONSTANTS.INC'

	CALL AX_DONVRT (RA, DEC, CTOE, ELO, ELA)

	END

