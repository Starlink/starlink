*+  COVERAGE - Produce percentage coverage as function of data value
      SUBROUTINE COVERAGE( STATUS )
*
*    Description :
*
*     Constructs the coverage function for 2d images, or in the case where
*     an input with dimensionality greater than 2 is supplied,the coverage
*     function for each slice. The coverage function is defined as that
*     percentage of the good pixels in a 2D slice with value above a specified
*     percentage of the range of the data in the slice. The user selects
*     the range of data to be covered - this defaults in the full range in
*     the whole dataset.
*
*     The output dataset is then a 1D dataset, with x-axis in units of the
*     input dataset's data, and y-axis in percentage. In the higher dimen-
*     sional case a stack of such functions is returned.
*
*    Environment parameters :
*     parameter(dimensions) =type(access,i.e. R,W or U)
*           <description of parameter>
*    Method :
*     <description of how the application works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     15 Jun 92 : V1.7-0 Original (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*40              LABEL, UNITS            ! Input data attributes

      REAL                      DMAX, DMIN              ! Range in data

      INTEGER                   DDIMS(ADI__MXDIM)       ! Dummy dimensions
      INTEGER                   DNDIM                   ! Dummy dimensionality
      INTEGER                   I                       ! General loop variable
      INTEGER                   IDIMS(ADI__MXDIM)       ! Input dimensions
      INTEGER                   IDPTR                   ! Input data ptr
      INTEGER			IFID			! Input dataset id
      INTEGER                   INDIM                   ! Input dimensionality
      INTEGER                   INELM                   ! Input # of points
      INTEGER                   IQPTR                   ! Input quality ptr
      INTEGER                   NCBIN                   ! # coverage bins
      INTEGER                   ODIMS(ADI__MXDIM-1)     ! Output dimensions
      INTEGER                   ODPTR                   ! Output data ptr
      INTEGER			OFID			! Output dataset id
      INTEGER                   ONDIM                   ! Output dimensionality

      LOGICAL                   ANYBAD                  ! Any bad quality points
      LOGICAL                   OK                      ! Validity test
      LOGICAL                   QOK                     ! Quality present?
*
*    Version :
*
      CHARACTER*30             VERSION
        PARAMETER              (VERSION = 'COVERAGE Version 1.8-0')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Version id
      CALL MSG_PRNT( VERSION )

*    Start ASTERIX
      CALL AST_INIT()

*    Get files
      CALL USI_TASSOC2( 'INP', 'OUT', 'READ', IFID, OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check data
      CALL BDI_CHKDATA( IFID, OK, INDIM, IDIMS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( INDIM .LT. 2 ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Input data should be 2 or 3 dimensional',
     :                                                       STATUS )
      END IF

*    Map data
      CALL BDI_MAPDATA( IFID, 'READ', IDPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Quality present?
      CALL BDI_CHKQUAL( IFID, QOK, DNDIM, DDIMS, STATUS )
      IF ( QOK ) THEN
        CALL BDI_MAPLQUAL( IFID, 'READ', ANYBAD, IQPTR, STATUS )
        IF ( .NOT. ANYBAD ) THEN
          CALL BDI_UNMAPLQUAL( IFID, STATUS )
          QOK = .FALSE.
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Number of data points
      CALL ARR_SUMDIM( INDIM, IDIMS, INELM )

*    Get range in data
      IF ( QOK ) THEN
        CALL ARR_RANG1RLQ( INELM, %VAL(IDPTR), %VAL(IQPTR), DMIN, DMAX,
     :                                                         STATUS )
      ELSE
        CALL ARR_RANG1R( INELM, %VAL(IDPTR), DMIN, DMAX, STATUS )
      END IF

*    Set defaults for coverage graph
      CALL USI_DEF0R( 'MIN', DMIN, STATUS )
      CALL USI_DEF0R( 'MAX', DMAX, STATUS )

*    Get range for coverage
      CALL USI_GET0R( 'MIN', DMIN, STATUS )
      CALL USI_GET0R( 'MAX', DMAX, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get number of bins for coverage array
      CALL USI_GET0I( 'NBIN', NCBIN, STATUS )

*    Create output dimensions
      ODIMS(1) = NCBIN
      IF ( INDIM .GT. 2 ) THEN
        DO I = 3, INDIM
          ODIMS(I-1) = IDIMS(I)
        END DO
      END IF
      ONDIM = INDIM - 1

*    Create output axes. First axis comes from input data, second and
*    subsequent axes are copied from 3rd onwards from input, if present.
      CALL BDI_CREAXES( OFID, ONDIM, STATUS )
      CALL BDI_CREAXVAL( OFID, 1, .TRUE., NCBIN, STATUS )
      CALL BDI_PUTAXVAL( OFID, 1, DMIN + (DMAX-DMIN)/REAL(NCBIN)/2.0,
     :                (DMAX-DMIN)/REAL(NCBIN), NCBIN, STATUS )
      CALL BDI_GETUNITS( IFID, UNITS, STATUS )
      CALL BDI_GETLABEL( IFID, LABEL, STATUS )
      IF ( UNITS .GT. ' ' ) THEN
        CALL BDI_PUTAXUNITS( OFID, 1, UNITS, STATUS )
      END IF
      IF ( LABEL .GT. ' ' ) THEN
        CALL BDI_PUTAXLABEL( OFID, 1, LABEL, STATUS )
      END IF
      IF ( INDIM .GT. 2 ) THEN
        DO I = 3, INDIM
          CALL BDI_COPAXIS( IFID, OFID, I, I-1, STATUS )
        END DO
      END IF
      CALL BDI_PUTLABEL( OFID, 'Coverage', STATUS )
      CALL BDI_PUTUNITS( OFID, 'percent', STATUS )

*    Create output data
      CALL BDI_CREDATA( OFID, ONDIM, ODIMS, STATUS )
      CALL BDI_MAPDATA( OFID, 'WRITE', ODPTR, STATUS )

*    Perform coverage analysis
      CALL COVERAGE_INT( IDIMS(1)*IDIMS(2), INELM/(IDIMS(1)*IDIMS(2)),
     :                   %VAL(IDPTR), QOK, %VAL(IQPTR), NCBIN,
     :                   DMIN, DMAX, %VAL(ODPTR), STATUS )

*    Copy ancillary stuff
      CALL BDI_COPMORE( IFID, OFID, STATUS )

*    History update
      CALL HSI_COPY( IFID, OFID, STATUS )
      CALL HSI_ADD( OFID, VERSION, STATUS )

*    Release datasets
      CALL BDI_RELEASE( IFID, STATUS )
      CALL BDI_RELEASE( OFID, STATUS )

*    Tidy up
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  COVERAGE_INT - Does work for COVERAGE
      SUBROUTINE COVERAGE_INT( NIMAGE, NFRAME, IN, QOK, INQ,
     :                         NCOVER, LO, HI, OUT, STATUS )
*    Description :
*     <description of what the subroutine does - for user info>
*    Method :
*     <description of how the subroutine works - for programmer info>
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     15 Jun 92 : Original (DJA)
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
      INTEGER                  NIMAGE                  ! # points per frame
      INTEGER                  NFRAME                  ! # frames
      REAL                     IN(NIMAGE,NFRAME)       ! Input data
      LOGICAL                  QOK                     ! Quality present
      LOGICAL                  INQ(NIMAGE,NFRAME)      ! Input quality
      INTEGER                  NCOVER                  ! # coverage bins
      REAL                     LO, HI                  ! Data range for scaling
*
*    Export :
*
      REAL                     OUT(NCOVER,NFRAME)      ! Output coverages
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      REAL                     CBIN                    ! Width of bin in %
      REAL                     FRAC                    ! Normalised data value
      REAL                     RANGE                   ! Range of data

      INTEGER                  I                       ! Loop over input bins
      INTEGER                  IC                      ! Loop over coverage bins
      INTEGER                  ICB                     ! Coverage bin number
      INTEGER                  IFRAME                  ! Loop over frames
      INTEGER                  NGOOD                   ! # good points
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Range to cover
      RANGE = HI - LO
      CBIN = 1.0 / NCOVER

*    Loop over frames
      DO IFRAME = 1, NFRAME

*      Zero coverage counter
        DO IC = 1, NCOVER
          OUT(IC,IFRAME) = 0.0
        END DO

*      Quality present?
        IF ( QOK ) THEN
          NGOOD = 0
          DO I = 1, NIMAGE
            IF ( INQ(I,IFRAME) ) THEN
              NGOOD = NGOOD + 1
              IF ( (IN(I,IFRAME).GE.LO) .AND.
     :             (IN(I,IFRAME).LE.HI) ) THEN
                FRAC = (IN(I,IFRAME)-LO)/RANGE
                ICB = INT(FRAC/CBIN) + 1
                OUT(ICB,IFRAME) = OUT(ICB,IFRAME) + 1.0
              END IF
            END IF
          END DO
        ELSE
          NGOOD = NIMAGE
          DO I = 1, NIMAGE
            IF ( (IN(I,IFRAME).GE.LO) .AND.
     :           (IN(I,IFRAME).LE.HI) ) THEN
              FRAC = (IN(I,IFRAME)-LO)/RANGE
              ICB = INT(FRAC/CBIN) + 1
              OUT(ICB,IFRAME) = OUT(ICB,IFRAME) + 1.0
            END IF
          END DO
        END IF

*      Correct coverage bins to cumulative percentage of all good pixels
        DO IC = 1, NCOVER
          OUT(IC,IFRAME) = OUT(IC,IFRAME) * 100.0 / REAL(NGOOD)
          IF ( IC .GT. 1 ) THEN
            OUT(IC,IFRAME) = OUT(IC,IFRAME) + OUT(IC-1,IFRAME)
          END IF
        END DO

      END DO

      END
