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
*     23 Dec 93 : Original (DJA)
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
      INCLUDE 'ASTLIB(PSF_CMN)'
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
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      OK = .FALSE.

*    See if the library has a hint routine available
      IF ( L_MOD_H(P_MODID(SLOT),P_LIBID(SLOT)) .NE. 0 ) THEN
        CALL PSF_QHINT_EXEC( %VAL(L_MOD_H(P_MODID(SLOT),
     :                                  P_LIBID(SLOT))),
     :                      SLOT, HINT, DATA, STATUS )
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
      SUBROUTINE PSF_QHINT_EXEC( ROUTINE, SLOT, HINT, DATA, STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (ROSAT, University of Birmingham)
*
*    History :
*
*     23 Dec 93 : Original (DJA)
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
      INTEGER                  SLOT                    ! The PSF id
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

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL ROUTINE( SLOT, HINT, DATA, STATUS )

      END
