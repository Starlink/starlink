*+  PSF_RELEASE - Return a slot to the PSF system
      SUBROUTINE PSF_RELEASE( SLOT, STATUS )
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      1 Nov 89 : Original (DJA)
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
*    Import :
*
      INTEGER          SLOT                   ! Psf handle
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Valid slot?
      IF ( (SLOT.LT.0) .OR. (SLOT.GT.PSF_NMAX) ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid slot identifier', STATUS )
      ELSE

*      Mark slot as free
        P_USED(SLOT) = .FALSE.

*      Free model data
        IF ( P_MODEL(SLOT) .AND. SM_GOTDATA(SLOT) ) THEN
          CALL DYN_UNMAP( SM_FLAG(SLOT), STATUS )
          CALL DYN_UNMAP( SM_DATA(SLOT), STATUS )
        END IF

      END IF

      END
