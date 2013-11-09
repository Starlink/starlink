*+  PSF_RELEASE - Return a slot to the PSF system
      SUBROUTINE PSF_RELEASE( PSID, STATUS )
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
*    Import :
*
      INTEGER          PSID                   ! Psf handle
*
*    Local Variables:
      INTEGER			FID			! File id
      INTEGER			RTNPTR			! Psf routine ptr

      LOGICAL			THERE			! Routine exists?
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke psf close routine if present
      CALL PSF0_FNDRTN( PSID, 'Close', THERE, RTNPTR, STATUS )
      IF ( THERE ) THEN
        CALL PSF_REL_EXEC( %VAL(RTNPTR), PSID, STATUS )
      END IF

*  Free model data
c      IF ( P_MODEL(SLOT) .AND. SM_GOTDATA(SLOT) ) THEN
c        CALL DYN_UNMAP( SM_FLAG(SLOT), STATUS )
c        CALL DYN_UNMAP( SM_DATA(SLOT), STATUS )
c      END IF

*  Get file identifier
      CALL ADI_CGET0I( PSID, 'FileID', FID, STATUS )

*  Erase the psf property
c      CALL ADI_CERASE( PSID, PSF__PROP, STATUS )

      END



*+  PSF_REL_EXEC - Invoke a psf closure routine
      SUBROUTINE PSF_REL_EXEC( ROUTINE, PSID, STATUS )
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
      INTEGER                  PSID                    ! The PSF to use
      EXTERNAL                 ROUTINE                 ! The _CLOSE routine
*
*    Status :
*
      INTEGER                  STATUS
*-

      CALL ROUTINE( PSID, STATUS )

      END
