*+  PSF_PUT_MODEL - Write details of psf model to file
      SUBROUTINE PSF_PUT_MODEL( SLOT, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     27 May 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      INTEGER                      SLOT               ! Psf handle
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       PLOC               ! PSF data structure
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Locate PSF structure, creating if necessary
      CALL ADI1_LOCPSF( P_FID(SLOT), .TRUE., PLOC, STATUS )

*    Create sub-components
      CALL DAT_NEW0C( PLOC, 'ROUTINE_NAME', 20, STATUS )
      CALL DAT_NEW0C( PLOC, 'LIBRARY_NAME', 20, STATUS )

*    Write in values
      CALL CMP_PUT0C( PLOC, 'LIBRARY_NAME', L_NAME(P_LIBID(SLOT)),
     :                STATUS )
      CALL CMP_PUT0C( PLOC, 'ROUTINE_NAME', 'PSF_'//
     :                L_MODN(P_MODID(SLOT),P_LIBID(SLOT)), STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT( 'Error writing PSF model' )
        CALL ERR_ANNUL( STATUS )
      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_PUT_MODEL', STATUS )
      END IF

      END
