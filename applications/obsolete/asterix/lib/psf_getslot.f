*+  PSF_GETSLOT - Grab a slot in the PSF common block
      SUBROUTINE PSF_GETSLOT( SLOT, STATUS )
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'ASTLIB(PSF_CMN)'
*
*    External references :
*
      EXTERNAL                 PSF_BLK
*
*    Export :
*
      INTEGER          SLOT                   ! Slot
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
        IF ( P_USED(I) ) THEN
          I = I + 1
        ELSE
          SLOT = I
        END IF
      END DO

*    Make sure there are enough slots left
      IF ( SLOT .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL MSG_PRNT( '! No more psf slots left' )
      ELSE

*      Zero the storage area
        P_LIBID(SLOT) = 0
        P_MODID(SLOT) = 0
        P_MODEL(SLOT) = .FALSE.
        P_INST(SLOT) = 0
        P_GOTAX(SLOT) = .FALSE.

*      Mark slot in use
        P_USED(SLOT) = .TRUE.

      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_GETSLOT', STATUS )
      END IF

      END
