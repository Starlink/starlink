      SUBROUTINE PSF_TASSOCI( ID, SLOT, STATUS )
      IMPLICIT NONE
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      CHARACTER*(DAT__SZLOC) LOC
	INTEGER ID,SLOT,STATUS
      LOGICAL THERE
      IF ( STATUS.NE.SAI__OK) RETURN
      CALL ADI_THERE( ID, '.PSF_SLOT', THERE, STATUS )
      IF ( THERE ) THEN
        CALL ADI_CGET0I( ID, '.PSF_SLOT', SLOT, STATUS )
      ELSE
        CALL ADI1_GETLOC( ID, LOC, STATUS )
        CALL PSF_ASSOCI( LOC, SLOT, STATUS )
        CALL ADI_CPUT0I( ID, '.PSF_SLOT', SLOT, STATUS )
      END IF
      END

*+  PSF_ASSOCI - Associate existing dataset with PSF routine
      SUBROUTINE PSF_ASSOCI( LOC, SLOT, STATUS )
*
*    Description :
*
*     Establishes the link between the dataset LOC and a psf handle. This
*     involves choosing a psf using the parameter system and calling any
*     initialisation routine defined for that psf.
*
*    Method :
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     31 Aug 89 : Original (DJA)
*     14 Dec 93 : Handle spatial reponse if present (DJA)
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
      INCLUDE 'ASTLIB(PSF_CMN)'
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
*
*    Local variables :
*
      CHARACTER*80             LNAME, RNAME            ! Library/routine names
      CHARACTER*(DAT__SZLOC)   PLOC                    ! Input dataset PSF
      CHARACTER*(DAT__SZLOC)   SLOC                    ! Spatial response

      INTEGER                  LIBID,MODID             ! Library/routine codes

      LOGICAL                  GOOD_PSF                ! Found a valid PSF
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Psf system initialised yet?
      IF ( .NOT. PSFINIT ) THEN
        CALL PSF_INIT( STATUS )
      END IF

*    Initialise
      GOOD_PSF = .FALSE.

*    Allocate slot
      CALL PSF_GETSLOT( SLOT, STATUS )

*    Does the dataset have an attached spatial response?
      CALL HDX_FIND( LOC, 'MORE.ASTERIX.SPATIAL_RESP', SLOC, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN

*      We have an internal routine to handle responses
        GOOD_PSF = .TRUE.
        LNAME = 'PSFLIB'
        RNAME = 'PSF_RESPFILE'

      ELSE

*      Annul status
        CALL ERR_ANNUL( STATUS )

*      Is the PSF structure OK
        CALL HDX_FIND( LOC, 'MORE.ASTERIX.PSF', PLOC, STATUS )

*      Got a psf
        IF ( STATUS .EQ. SAI__OK ) THEN

*        Get the components which identify the routine
          CALL CMP_GET0C( PLOC, 'LIBRARY_NAME', LNAME, STATUS )
          CALL CMP_GET0C( PLOC, 'ROUTINE_NAME', RNAME, STATUS )

*        Trap failure to find routine
          IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
          ELSE
            GOOD_PSF = .TRUE.
          END IF

*        Free psf box
          CALL DAT_ANNUL( PLOC, STATUS )

        ELSE
          CALL ERR_ANNUL( STATUS )

        END IF

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Good psf? Check that the routine exists
      IF ( GOOD_PSF ) THEN
        CALL PSF_CHKLIBRTN( LNAME, RNAME, LIBID, MODID, STATUS )
        IF ( STATUS .NE. SAI__OK ) THEN
          CALL ERR_ANNUL( STATUS )
          GOOD_PSF = .FALSE.
        END IF
      END IF

*    Set sensible default for PSF parameter, and query user
      IF ( GOOD_PSF ) THEN
        IF ( RNAME(1:4) .EQ. 'PSF_' ) THEN
          CALL PSF_PROMPT( .TRUE., RNAME(5:), SLOT, STATUS )
        ELSE
          CALL PSF_PROMPT( .TRUE., RNAME, SLOT, STATUS )
        END IF
      ELSE
        CALL PSF_PROMPT( .TRUE., 'ANAL', SLOT, STATUS )
      END IF

*    Initialise
      CALL PSF_SLOTINIT( LOC, SLOT, STATUS )

*    Tidy up
 99   IF ( STATUS .NE. SAI__OK ) THEN
        CALL AST_REXIT( 'PSF_ASSOCI', STATUS )
      END IF

      END
