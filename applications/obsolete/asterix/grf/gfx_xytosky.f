*+ GFX_XYTOSKY	Converts axis coords to sky coords
	SUBROUTINE GFX_XYTOSKY(X,Y,FRAME,AZ,EL)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
      INCLUDE 'MATH_PAR'
*  Import :
	REAL X				!input	X value
	REAL Y				!input	Y value
        INTEGER FRAME
*  Export :
	DOUBLE PRECISION AZ	!output	azimuth (degrees)
	DOUBLE PRECISION EL	!output	elevation (degrees)
*  Status :
        INTEGER STATUS
*  Global variables :
      INCLUDE 'GFX_SKY_CMN'
*  Local variables :
      DOUBLE PRECISION		EQU(2)			! Equatorial coords
      DOUBLE PRECISION		LCEL(2)			! Ecliptic/galactic

      REAL			LWORLD(2)		! World coordinates
*-

*  Local status
      STATUS = SAI__OK

*  Store axis coordinates
      LWORLD(1) = X
      LWORLD(2) = Y

*  Convert to RA/DEC
      CALL WCI_CNA2S( LWORLD, G_PIXID, G_PRJID, EQU, STATUS )

*  Equatorial required?
      IF ( FRAME .EQ. 1 ) THEN
        AZ = EQU(1) * MATH__DRTOD
        EL = EQU(2) * MATH__DRTOD

*  Ecliptic required?
      ELSE IF ( FRAME .EQ. 2 ) THEN
        CALL WCI_CNS2S( EQU, G_SYSID, G_ECLSYS, LCEL, STATUS )
        AZ = LCEL(1) * MATH__DRTOD
        EL = LCEL(2) * MATH__DRTOD

*  Galactic required?
      ELSE IF ( FRAME .EQ. 3 ) THEN
        CALL WCI_CNS2S( EQU, G_SYSID, G_GALSYS, LCEL, STATUS )
        AZ = LCEL(1) * MATH__DRTOD
        EL = LCEL(2) * MATH__DRTOD

      END IF

      END
