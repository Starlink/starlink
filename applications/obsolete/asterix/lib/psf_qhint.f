*+  PSF_QHINT - Ask for hints about psf properties
      SUBROUTINE PSF_QHINT( SLOT, HINT, OK, DATA, STATUS )
*
*    Description :
*
*     Returns the radii in radians at which the specified fractions of the
*     integrated psf are enclosed, at image position (X0,Y0).
*
*    Method :
*
*     Requests the psf handler to produce the named hint if present.
*
*    Deficiencies :
*
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     23 Dec 1993 (DJA):
*        Original version
*      1 May 1996 (DJA):
*        Use method interface
*
*    Type definitions :
*
      IMPLICIT NONE
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
      INTEGER                  SLOT                    ! The PSF id
      CHARACTER*(*)            HINT		       ! Hint name
*
*    Export :
*
      LOGICAL                  OK                      ! Hint data ok?
      BYTE                     DATA(*)                 ! Hint data
*
*    Status :
*
      INTEGER                  STATUS
*
*  Local Variables:
*
      INTEGER			RTNPTR			! Psf method
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Routine exists?
      CALL PSF0_FNDRTN( P_PSID(SLOT), 'Hint', OK, RTNPTR, STATUS )
      IF ( OK ) THEN
        CALL PSF_QHINT_EXEC( %VAL(RTNPTR), P_PSID(SLOT), HINT,
     :                       DATA, STATUS )
        IF ( STATUS .EQ. SAI__OK ) THEN
          OK = .TRUE.
        ELSE
          CALL ERR_ANNUL( STATUS )
        END IF
      END IF

*    Tidy up
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL AST_REXIT( 'PSF_QHINT', STATUS )
      END IF

      END



*+  PSF_QHINT_EXEC - Invoke library routine for hints
      SUBROUTINE PSF_QHINT_EXEC( ROUTINE, PSID, HINT, DATA, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     23 Dec 1993 (DJA):
*        Original version
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
      EXTERNAL                 ROUTINE                 ! LIB routine to call
      INTEGER                  PSID			! Psf object
      CHARACTER*(*)            HINT		       ! Hint name
*
*    Export :
*
      BYTE                     DATA(*)                 ! Hint data
*
*    Status :
*
      INTEGER                  STATUS
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Invoke routine
      CALL ROUTINE( PSID, HINT, DATA, STATUS )

      END
