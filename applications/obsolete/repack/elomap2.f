*+ELOMAP2 Convert small map indices to ecliptic longitude
	DOUBLEPRECISION FUNCTION ELOMAP2(ILO,ILA)
	IMPLICIT NONE
* Input
	INTEGER		ILO, ILA	! Small map indices
* M. Denby Jan-89
* P McGale Apr 95
*-
	INCLUDE 'SMAPDEF.INC'

	ELOMAP2=(DBLE(ILO-1)*360.D0/DBLE(NBINS(ILA)))*DTOR

	END
