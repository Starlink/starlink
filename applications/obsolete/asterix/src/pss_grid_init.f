*+  PSS_GRID_INIT - Initialises a grid mapping the user chosen image area
      SUBROUTINE PSS_GRID_INIT( STATUS )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     29 Jul 89 : Original (DJA)
*     10 Jul 93 : No longer uses inline functions (DJA)
*     13 Sep 93 : Allow sub-sampling (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Function definitions :
*
      REAL                     DAT
*
*    Local variables :
*
      REAL                     RSUBPIX                 ! Sub-pixelling

      INTEGER                  IAX                     ! Loop over axes
      INTEGER                  SUBPIX                  ! Sub-pixelling
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Get oversampling
      IF ( CP_EXPERT ) THEN
        CALL PAR_GET0I( 'SAMPLE', SUBPIX, STATUS )
        IF ( STATUS .NE. SAI__OK ) GOTO 99
      ELSE
        SUBPIX = 1
      END IF
      RSUBPIX = REAL(SUBPIX)

*    Set psf resampling flag if SUBPIX not unity
      PSF_RESAM = ( SUBPIX .GT. 1 )

*    Sub-sampling if SUBPIX is negative
      IF ( SUBPIX .LT. 0 ) THEN
        RSUBPIX = 1.0 / ABS(RSUBPIX)
      ELSE IF ( SUBPIX .EQ. 0 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Illegal sampling value, should be non-zero',
     :                STATUS )
        GOTO 99
      END IF

*    Define initial grid parameters
      DO IAX = 1, 2

*      Grid origin in axis units
        GR_A0(IAX) = DAT(IAX,BDS_SPOF(1,IAX))

*      Number of grid points
        GR_DIMS(IAX) = (BDS_SPOF(2,IAX)-BDS_SPOF(1,IAX)+1)*RSUBPIX

*      Spacing in radians
        GR_DA(IAX) = AX_DR(IAX) / RSUBPIX
        GR_AC(IAX) = 1
        GR_GAC(IAX) = 1

*      Grid centre
        GR_CC(IAX) = GR_A0(IAX) + REAL(GR_DIMS(IAX))*GR_DA(IAX)/2.0

      END DO

*    Total number of grid points
      GR_NELM = GR_DIMS(1)*GR_DIMS(2)

*    Set first pass
      GR_PASS = 1

*    Abort point
 99   CONTINUE

      END
