*+  PSF_GETSLOT - Grab a slot in the PSF common block
      SUBROUTINE PSF_GETSLOT( FID, SLOT, STATUS )
*
*    Author :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      1 Nov 89 : Original (DJA)
*      7 Dec 92 : Removed SIZEOF function for port (DJA)
*
*    Type declarations :
*
      IMPLICIT NONE
*
      INTEGER	       STATUS
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    External references :
*
      EXTERNAL                 PSF_BLK
*
*    Import :
*
      INTEGER			FID			! Input dataset id
*
*    Export :
*
      INTEGER          		SLOT                   	! Slot
*
*    Local variables :
*
      INTEGER          I                      ! Loop over slots
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Psf system initialised yet?
      IF ( .NOT. PSFINIT ) THEN
        CALL PSF_INIT( STATUS )
      END IF

*    Scan for an empty slot
      SLOT = 0
      I = 1
      DO WHILE ( (I.LE.PSF_NMAX) .AND. (SLOT.EQ.0) )
        IF ( FID .EQ. P_FID(I) ) THEN
          SLOT = I
        ELSE IF ( P_USED(I) ) THEN
          I = I + 1
        ELSE
          SLOT = I
        END IF
      END DO

*    Make sure there are enough slots left
      IF ( SLOT .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'No more psf slots left', STATUS )

      ELSE IF ( .NOT. P_USED(SLOT) ) THEN

*      Zero the storage area
        P_MODEL(SLOT) = .FALSE.
        P_INST(SLOT) = 0
        P_GOTAX(SLOT) = .FALSE.
        P_FID(SLOT) = FID

*    Create psf object
        CALL ADI_NEW0( 'PsfDescription', P_PSID(SLOT), STATUS )
        CALL ADI_CPUT0I( P_PSID(SLOT), 'Slot', SLOT, STATUS )

*    Store file id
        CALL ADI_CPUT0I( P_PSID(SLOT), 'FileID', FID, STATUS )

*      Mark slot in use
        P_USED(SLOT) = .TRUE.

      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_GETSLOT', STATUS )
      END IF

      END
