	SUBROUTINE RADEC_CONVERT( RA, DEC, RA_ARRAY, DEC_ARRAY)

* Description : Subroutine to convert input RA and DEC real numbers to
*               RA and DEC positions and put result into RA and DEC
*               arrays

	IMPLICIT NONE

	REAL DEC
	REAL DEC_ARRAY( 3)
	REAL RA
	REAL RA_ARRAY( 3)

*      Calculate the hrs,mins,secs,dsecs

	RA_ARRAY( 1) = IFIX( RA)
	RA_ARRAY( 2) = IFIX( ( RA - RA_ARRAY( 1))*60.0)
	RA_ARRAY( 3) = ( RA - RA_ARRAY( 1) - RA_ARRAY( 2)/60.0)*3600.0

*      Calculate the degs.mins.secs,dsecs

	DEC_ARRAY( 1) = IFIX( DEC)
	DEC_ARRAY( 2) = IFIX( ( DEC - DEC_ARRAY( 1))*60.0)
	DEC_ARRAY( 3) = ( DEC - DEC_ARRAY( 1) - DEC_ARRAY( 2)/60.0)*3600.0

	END
