*+  PSF_RELEASE - Return a slot to the PSF system
      SUBROUTINE PSF_RELEASE( SLOT, STATUS )
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      1 Nov 1989 : Original (DJA)
*      9 Jan 1996 : Added call to closure routine if present (DJA)
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

*    Invoke psf close routine if present
        IF ( L_MOD_C(P_MODID(SLOT),P_LIBID(SLOT)) .NE. 0 ) THEN

          CALL PSF_REL_EXEC( %VAL(L_MOD_C(P_MODID(SLOT),P_LIBID(SLOT))),
     :                       SLOT, STATUS )
        END IF

*    Mark slot as free
        P_USED(SLOT) = .FALSE.

*    Free model data
        IF ( P_MODEL(SLOT) .AND. SM_GOTDATA(SLOT) ) THEN
          CALL DYN_UNMAP( SM_FLAG(SLOT), STATUS )
          CALL DYN_UNMAP( SM_DATA(SLOT), STATUS )
        END IF

      END IF

      END



*+  PSF_REL_EXEC - Invoke a psf closure routine
      SUBROUTINE PSF_REL_EXEC( ROUTINE, SLOT, STATUS )
*
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     30 Oct 89 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Import :
*
      INTEGER                  SLOT                    ! The PSF to use
      EXTERNAL                 ROUTINE                 ! The _CLOSE routine
*
*    Status :
*
      INTEGER                  STATUS
*-

      CALL ROUTINE( SLOT, STATUS )

      END
