*+ELAMAP2 Convert small map indices into ecliptic latitude
	DOUBLEPRECISION FUNCTION ELAMAP2(ILA)
	IMPLICIT NONE
* Input
	INTEGER		ILA		! Small map lat index
* M. Denby Jan-89
* P McGale Apr 95
*-
	INCLUDE 'CONSTANTS.INC'

	ELAMAP2=((DBLE(ILA)*2.D0)-92.D0)*DTOR

	END
