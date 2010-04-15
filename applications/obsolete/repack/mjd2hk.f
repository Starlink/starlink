*+MJD2HK Convert MJD to HK UT 1/2 seconds
	INTEGER	FUNCTION MJD2HK (MJD)
	INCLUDE 'SMAPDEF.INC'
* Input
	double precision	MJD
* M. Denby Nov 89
* P McGale May 95 - UNIX mods
*-
* Local
	double precision	SECS2
	PARAMETER      (SECS2 = 86400.D0*2.D0)
*
	MJD2HK = INT((MJD - S2_REF_MJD)*SECS2)

	END
