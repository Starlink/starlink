      SUBROUTINE PSF_TASSOCO( ID, SLOT, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      CHARACTER*(DAT__SZLOC) LOC
        INTEGER ID,SLOT,STATUS
      IF ( STATUS.NE.SAI__OK) RETURN
      CALL ADI1_GETLOC( ID,LOC,STATUS)
      CALL PSF_ASSOCO( LOC, SLOT, STATUS )

      END

*+  PSF_ASSOCO - Associate a dataset and create PSF if necessary
      SUBROUTINE PSF_ASSOCO( LOC, SLOT, STATUS )
*
*    Description :
*
*     Routine associates a dataset being written to a psf handle.
*     Enough info must be file to enable psf system to find axis
*     info etc.
*
*    Method :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     05 Feb 90 : Original (DJA)
*     29 Jan 94 : Initialisation via block data (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_ERR'
      INCLUDE 'PSF_PAR'
*
*    Global variables :
*
      INCLUDE 'PSF_CMN'
*
*    Import :
*
      CHARACTER*(DAT__SZLOC)   LOC                     ! Input dataset
*
*    Export :
*
      INTEGER                  SLOT                    ! PSF slot number
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    External references :
*
      EXTERNAL		       PSF_BLK
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Grab slot
      CALL PSF_GETSLOTL( LOC, SLOT, STATUS )

*    Get library and routine name from user
      CALL PSF_PROMPT( .FALSE., ' ', SLOT, STATUS )

*    Initialise the PSF routine
      CALL PSF_SLOTINIT( LOC, SLOT, STATUS )

*    Try to write model to file
      CALL PSF_PUT_MODEL( LOC, SLOT, STATUS )

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_ASSOCO', STATUS )
      END IF

      END
