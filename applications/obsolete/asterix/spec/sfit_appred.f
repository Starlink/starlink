*+  SFIT_APPRED - Apply red-shift to energy bounds
      SUBROUTINE SFIT_APPRED( REDSHIFT, NEN, ELBOUND, EUBOUND, STATUS )
*
*    Description :
*
*     Apply red-shift to energy bounds
*
*    Method :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      4 Dec 92 : Original (DJA)
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
      REAL                       REDSHIFT                ! Red shift to apply
      INTEGER                    NEN                     ! Number of bounds
*
*    Import-Export :
*
      REAL                       ELBOUND(*)              ! Lower bounds
      REAL                       EUBOUND(*)              ! Upper bounds
*
*    Status :
*
      INTEGER STATUS
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( REDSHIFT .GT. 0.0 ) THEN
        CALL ARR_MULTR( REDSHIFT+1.0, NEN, ELBOUND )
        CALL ARR_MULTR( REDSHIFT+1.0, NEN, EUBOUND )
      END IF

      END
