*+ GFX_SKYTOXY	Converts sky coords to axis coords
	SUBROUTINE GFX_SKYTOXY(AZ,EL,FRAME,X,Y)

        IMPLICIT NONE

*  Global constants :
        INCLUDE 'SAE_PAR'
        INCLUDE 'MATH_PAR'
*  Import :
	DOUBLE PRECISION AZ		!input	azimuth (degrees)
	DOUBLE PRECISION EL		!input	elevation (degrees)
        INTEGER FRAME
*  Export :
	REAL X				!output	X value
	REAL Y				!output	Y value
*  Status :
        INTEGER STATUS
*  Global variables :
        INCLUDE 'GFX_SKY_CMN'
*  Local variables :
      DOUBLE PRECISION		LCEL(2), EQU(2)

      REAL			LWORLD(2)		! Axis coordinates
*-

*  Local status
      STATUS = SAI__OK

*  Equatorial supplied?
      IF ( FRAME .EQ. 1 ) THEN
        EQU(1) = AZ * MATH__DDTOR
        EQU(2) = EL * MATH__DDTOR

*  Ecliptic supplied?
      ELSE IF ( FRAME .EQ. 2 ) THEN
        LCEL(1) = AZ * MATH__DDTOR
        LCEL(2) = EL * MATH__DDTOR
        CALL WCI_CNS2S( LCEL, G_ECLSYS, G_SYSID, EQU, STATUS )

*  Galactic supplied?
      ELSE IF ( FRAME .EQ. 3 ) THEN
        LCEL(1) = AZ * MATH__DDTOR
        LCEL(2) = EL * MATH__DDTOR
        CALL WCI_CNS2S( LCEL, G_GALSYS, G_SYSID, EQU, STATUS )

      END IF

*  Convert equatorial to world
      CALL WCI_CNS2A( EQU, G_PIXID, G_PRJID, LWORLD, STATUS )
      X = LWORLD(1)
      Y = LWORLD(2)

      END
