*+ GFX_TMAT - construct transformation matrix
	SUBROUTINE GFX_TMAT( PIXID, PRJID, SYSID, STATUS )
*
*    Description:
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*    Global variables :
*    Import :
      INTEGER			PIXID, PRJID, SYSID
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_SKY_CMN'
*    Local Variables:
      DOUBLE PRECISION          EPOCH                   ! Epoch of obs'n

      REAL                      EQNX                    ! Equinox of input sys
*-

*  Store WCS data
      G_PIXID = PIXID
      G_PRJID = PRJID
      G_SYSID = SYSID

*  Extract equinox and epoch from input WCS
      IF ( SYSID .NE. ADI__NULLID ) THEN
        CALL ADI_CGET0R( SYSID, 'EQUINOX', EQNX, STATUS )
        CALL ADI_CGET0D( SYSID, 'EPOCH', EPOCH, STATUS )
      ELSE
        EQNX = 2000.0
        EPOCH = 2000.0
      END IF

*  Create descriptions of eclipic and galactic coordinates
      CALL WCI_NEWSYS( 'ECL', EQNX, EPOCH, G_ECLSYS, STATUS )
      CALL WCI_NEWSYS( 'GAL', EQNX, EPOCH, G_GALSYS, STATUS )

      END
