*+  PSS_CACHE - Set up data cache and perform required summations
      SUBROUTINE PSS_CACHE( NX, NY, IMD, IMBV, QUAL, BGND,
     :                      BGDV, PNX, PNY, PSFV, LBGND,
     :                      CACHE_CTRL, STATUS )
*
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 May 92 : Original (DJA)
*      7 Apr 93 : Moved setting of DC_CP_C outside quality test. If the
*                 centre pixel is bad, this picks up the last good cached
*                 pixel. (DJA)
*     28 Feb 94 : Quality now pre-masked, so test changed (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  NX, NY
      REAL                     IMD(NX,NY)              ! ... raw data
      REAL                     IMBV(NX,NY)             ! ... variance
      REAL                     BGND(NX,NY)             ! ... background values
      REAL                     BGDV(NX,NY)             ! ... background variance
      BYTE                     QUAL(NX,NY)             ! ... data quality
      INTEGER                  PNX, PNY                ! PSF work space
      REAL                     PSFV(-PNX/2:PNX/2,
     :                             -PNY/2:PNY/2 )
      REAL                     LBGND(NX,NY)            ! Log(bgnd)
      INTEGER                  CACHE_CTRL              ! Cache control
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      REAL                     BGND_S                  ! Bgnd sum accumulator
      REAL                     IMD_S                   ! Data sum accumulator
      REAL                     PSF_S                   ! Psf sum accumulator

      INTEGER                  CP                      ! Cache pointer
      INTEGER                  II,JJ                   ! Loop across DATA
      INTEGER                  PII, PJJ                ! PSF equiv of (II,JJ)

      LOGICAL                  DO_BGND                 ! Cache background
      LOGICAL                  DO_BGND_SUM             ! Find background sum?
      LOGICAL                  DO_BGDV                 ! Cache background error
      LOGICAL                  DO_IMBV                 ! Cache data error
      LOGICAL                  DO_IMD                  ! Cache raw data
      LOGICAL                  DO_IMD_SUM              ! Find raw data sum?
      LOGICAL                  DO_LBGND                ! Cache log(bgnd)
      LOGICAL                  DO_PSF                  ! Cache psf
      LOGICAL                  DO_PSF_SUM              ! Find psf sum?

      INTEGER                  CX(PSS__CACHELEN)
      INTEGER                  CY(PSS__CACHELEN)
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Cacheing switches
      DO_IMD = .TRUE.
      DO_PSF = .TRUE.
      DO_BGND = IAND(CACHE_CTRL,DC_F_BGND).NE.0
      DO_LBGND = IAND(CACHE_CTRL,DC_F_LBGND).NE.0
      DO_IMBV = IAND(CACHE_CTRL,DC_F_IMBV).NE.0
      DO_BGDV = IAND(CACHE_CTRL,DC_F_BGDV).NE.0
      DO_IMD_SUM = IAND(CACHE_CTRL,DC_F_IMD_SUM).NE.0
      DO_PSF_SUM = IAND(CACHE_CTRL,DC_F_PSF_SUM).NE.0
      DO_BGND_SUM = IAND(CACHE_CTRL,DC_F_BGND_SUM).NE.0

*    Cache psf?
      CP = 0
      IF ( DO_PSF ) THEN
        IF ( BDS_QUAL_OK ) THEN
          DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
            PJJ = JJ - GR_RNG_CEN(2)
            DO II = GR_RNG_LO(1), GR_RNG_HI(1)
              IF ( QUAL(II,JJ) .EQ. QUAL__GOOD ) THEN
                PII = II - GR_RNG_CEN(1)
                CP = CP + 1
                CX(CP) = II
                CY(CP) = JJ
                DC_PSF(CP) = PSFV(PII,PJJ)
              END IF
              IF ( PII.EQ.0.AND.PJJ.EQ.0) DC_CP_C = CP
            END DO
          END DO
        ELSE
          DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
            PJJ = JJ - GR_RNG_CEN(2)
            DO II = GR_RNG_LO(1), GR_RNG_HI(1)
              PII = II - GR_RNG_CEN(1)
              CP = CP + 1
              CX(CP) = II
              CY(CP) = JJ
              IF ( PII.EQ.0.AND.PJJ.EQ.0) DC_CP_C = CP
              DC_PSF(CP) = PSFV(PII,PJJ)
            END DO
          END DO
        END IF
      END IF
      DC_LO = 1
      DC_HI = CP

*    Cache dataset arrays
      CP = 0
      IF ( DO_IMD ) THEN
        DO CP = DC_LO, DC_HI
          DC_IMD(CP) = IMD(CX(CP),CY(CP))
        END DO
      END IF
      IF ( DO_BGND ) THEN
        DO CP = DC_LO, DC_HI
          DC_BGND(CP) = BGND(CX(CP),CY(CP))
        END DO
      END IF
      IF ( DO_LBGND ) THEN
        DO CP = DC_LO, DC_HI
          DC_LBGND(CP) = LBGND(CX(CP),CY(CP))
        END DO
      END IF
      IF ( DO_IMBV ) THEN
        DO CP = DC_LO, DC_HI
          DC_IMBV(CP) = IMBV(CX(CP),CY(CP))
        END DO
      END IF
      IF ( DO_BGDV ) THEN
        DO CP = DC_LO, DC_HI
          DC_BGDV(CP) = BGDV(CX(CP),CY(CP))
        END DO
      END IF

*    Check DC_CP_C
      IF ( DC_CP_C .EQ. 0 ) DC_CP_C = 1

*    Initialise sums
      PSF_S = 0.0
      IMD_S = 0.0
      BGND_S = 0.0

*    Do sums
      IF ( DO_IMD_SUM ) THEN
        DO CP = DC_LO, DC_HI
          IMD_S = IMD_S + DC_IMD(CP)
        END DO
        DC_IMD_S = IMD_S
      END IF
      IF ( DO_BGND_SUM ) THEN
        DO CP = DC_LO, DC_HI
          BGND_S = BGND_S + DC_BGND(CP)
        END DO
        DC_BGND_S = BGND_S
      END IF

      END
*+  PSS_CACHE_CASH - Set up data cache and perform required summations
      SUBROUTINE PSS_CACHE_CASH( IX, IY, PSF_W, NX, NY, IMD,
     :           QUAL, BGND, PNX, PNY, PSFV, LBGND, STATUS )
*
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 May 92 : Original (DJA)
*     28 Feb 94 : Quality now pre-masked (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  IX, IY                  ! Reference data pixel
      INTEGER                  NX, NY                  ! Data dimensions
      LOGICAL                  PSF_W                   ! Psf on whole box?
      REAL                     IMD(NX,NY)              ! ... raw data
      REAL                     BGND(NX,NY)             ! ... background values
      BYTE                     QUAL(NX,NY)             ! ... data quality
      INTEGER                  PNX, PNY                ! PSF work space
      REAL                     PSFV(-PNX/2:PNX/2,
     :                             -PNY/2:PNY/2 )
      REAL                     LBGND(NX,NY)            ! Log(bgnd)
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER                  CLO                     ! Cache bounds
      INTEGER                  CP                      ! Cache pointer
      INTEGER                  II,JJ                   ! Loop across DATA
      INTEGER                  LN                      ! Product of LX, LY
      INTEGER                  LX, LY                  ! Last values of IX,IY
      INTEGER                  PII, PJJ                ! PSF equiv of (II,JJ)
*
*    Save to next call :
*
      SAVE                     LX, LY, LN
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise pointers
      CP = 0
      DC_LO = 1

*    Has reference pixel changed
      IF ( DC_VOLATILE .OR. (IX.NE.LX) .OR. (IY.NE.LY) ) THEN

*      Save position
        LX = IX
        LY = IY
        LN = GR_NELM

*      Cache psf if
        IF ( .NOT. PSF_W ) THEN

*        Cache all arrays
          IF ( BDS_QUAL_OK ) THEN
            DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
              PJJ = JJ - GR_RNG_CEN(2)
              IF ( PJJ .EQ. 0 ) CLO = CP
              DO II = GR_RNG_LO(1), GR_RNG_HI(1)
                IF ( QUAL(II,JJ) .EQ. QUAL__GOOD ) THEN
                  PII = II - GR_RNG_CEN(1)
                  CP = CP + 1
                  DC_PSF(CP) = PSFV(PII,PJJ)
                  DC_IMD(CP) = IMD(II,JJ)
                  DC_BGND(CP) = BGND(II,JJ)
                  DC_LBGND(CP) = LBGND(II,JJ)
                END IF
              END DO
              IF ( PJJ .EQ. 0 ) THEN
                DC_CP_C = (CP+CLO)/2
              END IF
            END DO
          ELSE
            DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
              PJJ = JJ - GR_RNG_CEN(2)
              IF ( PJJ .EQ. 0 ) CLO = CP
              DO II = GR_RNG_LO(1), GR_RNG_HI(1)
                PII = II - GR_RNG_CEN(1)
                CP = CP + 1
                DC_PSF(CP) = PSFV(PII,PJJ)
                DC_IMD(CP) = IMD(II,JJ)
                DC_BGND(CP) = BGND(II,JJ)
                DC_LBGND(CP) = LBGND(II,JJ)
              END DO
              IF ( PJJ .EQ. 0 ) THEN
                DC_CP_C = (CP+CLO)/2
              END IF
            END DO
          END IF

        ELSE

*        Cache all image arrays, but not psf
          IF ( BDS_QUAL_OK ) THEN
            DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
              IF ( JJ .EQ. GR_RNG_CEN(2) ) CLO = CP
              DO II = GR_RNG_LO(1), GR_RNG_HI(1)
                IF ( QUAL(II,JJ) .EQ. QUAL__GOOD ) THEN
                  CP = CP + 1
                  DC_IMD(CP) = IMD(II,JJ)
                  DC_BGND(CP) = BGND(II,JJ)
                  DC_LBGND(CP) = LBGND(II,JJ)
                END IF
              END DO
              IF ( JJ .EQ. GR_RNG_CEN(2) ) DC_CP_C = (CP+CLO)/2
            END DO
          ELSE
            DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
              IF ( JJ .EQ. GR_RNG_CEN(2) ) CLO = CP
              DO II = GR_RNG_LO(1), GR_RNG_HI(1)
                CP = CP + 1
                DC_IMD(CP) = IMD(II,JJ)
                DC_BGND(CP) = BGND(II,JJ)
                DC_LBGND(CP) = LBGND(II,JJ)
              END DO
              IF ( JJ .EQ. GR_RNG_CEN(2) ) DC_CP_C = (CP+CLO)/2
            END DO
          END IF
        END IF

      ELSE

*      Just cache psf
        IF ( BDS_QUAL_OK ) THEN
          DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
            PJJ = JJ - GR_RNG_CEN(2)
            IF ( PJJ .EQ. 0 ) CLO = CP
            DO II = GR_RNG_LO(1), GR_RNG_HI(1)
              IF ( QUAL(II,JJ) .EQ. QUAL__GOOD ) THEN
                PII = II - GR_RNG_CEN(1)
                CP = CP + 1
                DC_PSF(CP) = PSFV(PII,PJJ)
              END IF
            END DO
            IF ( PJJ .EQ. 0 ) DC_CP_C = (CP+CLO)/2
          END DO
        ELSE
          DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
            PJJ = JJ - GR_RNG_CEN(2)
            IF ( PJJ .EQ. 0 ) CLO = CP
            DO II = GR_RNG_LO(1), GR_RNG_HI(1)
              PII = II - GR_RNG_CEN(1)
              CP = CP + 1
              DC_PSF(CP) = PSFV(PII,PJJ)
            END DO
            IF ( PJJ .EQ. 0 ) DC_CP_C = (CP+CLO)/2
          END DO
        END IF

      END IF

*    Lose volatility
      DC_VOLATILE = .FALSE.

*    Set top of cache
      DC_HI = CP

*    Check cache centre valid
      IF ( DC_CP_C .LT. 1 ) DC_CP_C = 1

      END
*+  PSS_CACHE_PSF - Cache the psf with whole pixel offsets
      SUBROUTINE PSS_CACHE_PSF( NX, NY, QUAL, PNX, PNY, PSFV, STATUS )
     :
*
*    Description :
*
*    Method :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     26 May 92 : Original (DJA)
*     28 Feb 94 : Quality now pre-masked (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER                  NX, NY
      BYTE                     QUAL(NX,NY)             ! ... data quality
      INTEGER                  PNX, PNY                ! PSF work space
      REAL                     PSFV(-PNX/2:PNX/2,
     :                             -PNY/2:PNY/2 )
*
*    Status :
*
      INTEGER                  STATUS                  ! Run-time error
*
*    Local variables :
*
      INTEGER                  CP                      ! Cache pointer
      INTEGER                  II,JJ                   ! Loop across DATA
      INTEGER                  PII, PJJ                ! PSF equiv of (II,JJ)
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Cache psf data, switch on presence of quality
      CP = 0
      IF ( BDS_QUAL_OK ) THEN
        DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
          PJJ = JJ - GR_RNG_CEN(2)
          DO II = GR_RNG_LO(1), GR_RNG_HI(1)
            IF ( QUAL(II,JJ) .EQ. QUAL__GOOD ) THEN
              PII = II - GR_RNG_CEN(1)
              CP = CP + 1
              DC_PSF(CP) = PSFV(PII,PJJ)
            END IF
          END DO
        END DO
      ELSE
        DO JJ = GR_RNG_LO(2), GR_RNG_HI(2)
          PJJ = JJ - GR_RNG_CEN(2)
          DO II = GR_RNG_LO(1), GR_RNG_HI(1)
            PII = II - GR_RNG_CEN(1)
            CP = CP + 1
            DC_PSF(CP) = PSFV(PII,PJJ)
          END DO
        END DO
      END IF

      END
