	SUBROUTINE RADEC_NEW( RA, DEC, RAOFF, DECOFF, RA_NEW, DEC_NEW)

* Description : Subroutine to calculate a new RA and DEC from old RA
*               and DEC plus offsets in arcseconds.

	IMPLICIT NONE

	REAL RA( 3)
	REAL DEC( 3)
	REAL RAOFF
	REAL DECOFF
	REAL RA_NEW( 3)
	REAL DEC_NEW( 3)
	REAL RRA
	REAL RDEC

*      Convert the RA and DEC positions to real numbers

	RRA = RA(1) + RA(2)/60.0 + RA( 3)/3600.0

	RDEC = DEC(1) + DEC(2)/60.0 + DEC( 3)/3600.0

*      Add the offsets to the RA and DEC

	RRA = RRA - RAOFF/( 15.0*3600.0)

	RDEC = RDEC + DECOFF/3600.0

*      Test if the new RA is less than 0 i.e. the value has gone
*      thorough 0

	IF( RRA .LT. 0.0) THEN

	  RRA = RRA + 24

	END IF

*      Test if the new RA is greater than 24 i.e. the value has gone
*      thorough 0

	IF( RRA .GT. 24.0) THEN

	  RRA = RRA - 24

	END IF

*      Re-calculate the hrs,mins,secs,dsecs

	RA_NEW( 1) = IFIX( RRA)
	RA_NEW( 2) = IFIX( ( RRA - RA_NEW( 1))*60.0)
	RA_NEW( 3) = ( RRA - RA_NEW( 1) - RA_NEW( 2)/60.0)*3600.0

*      Re-calculate the degs.mins.secs,dsecs

	DEC_NEW( 1) = IFIX( RDEC)
	DEC_NEW( 2) = IFIX( ( RDEC - DEC_NEW( 1))*60.0)
	DEC_NEW( 3) = ( RDEC - DEC_NEW( 1) - DEC_NEW( 2)/60.0)*3600.0

	END
