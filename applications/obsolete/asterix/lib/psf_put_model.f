*+  PSF_PUT_MODEL - Write details of psf model to file
      SUBROUTINE PSF_PUT_MODEL( PSID, STATUS )
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
*
*    Import :
*
      INTEGER                      PSID               ! Psf handle
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)       PLOC               ! PSF data structure
      CHARACTER*15		TAG			! Psf tag name

      INTEGER			FID			! File identifier
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get file identifier
      CALL ADI_CGET0I( PSID, 'FileID', FID, STATUS )

*  Locate PSF structure, creating if necessary
      CALL ADI1_LOCPSF( FID, .TRUE., PLOC, STATUS )

*  Create sub-components
      CALL DAT_NEW0C( PLOC, 'ROUTINE_NAME', 20, STATUS )
      CALL DAT_NEW0C( PLOC, 'LIBRARY_NAME', 20, STATUS )

*  Write in values
      CALL CMP_PUT0C( PLOC, 'LIBRARY_NAME', 'PSFLIB', STATUS )
      CALL ADI_CGET0C( PSID, 'Tag', TAG, STATUS )
      CALL CMP_PUT0C( PLOC, 'ROUTINE_NAME', TAG, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL MSG_PRNT( 'Error writing PSF model' )
        CALL ERR_ANNUL( STATUS )
      END IF

*  Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_PUT_MODEL', STATUS )
      END IF

      END
