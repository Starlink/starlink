*+  SPLINEFIT - Fit a spline to 1 or 2D data
      SUBROUTINE SPLINEFIT( STATUS )
*
*    Description :
*
*     Computes and applies a cubic spline approximation to an arbitrary set
*     of data points.
*
*    Environment parameters :
*
*     INP = UNIV(R)
*        Input dataset
*     SQ_RES = REAL(R)
*        Accuracy of fit required
*     USEW = LOGICAL(R)
*        use variance and quality - 2D or greater
*     OUT = UNIV(W)
*        Output dataset
*
*    Method :
*    Deficiencies :
*    Bugs :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Jun 91 : Original (DJA)
*     24 Nov 94 : V1.8-0 Now use USI for user interface (DJA)
*     20 Apr 95 : V1.8-1 New data interface (DJA)
*     13 Dec 1995 : V2.0-0 ADI port (DJA)
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
*    Local constants :
*
      INTEGER                   MAXLINES
        PARAMETER               ( MAXLINES = 6 )
      CHARACTER*6     		MTYPE
        PARAMETER               ( MTYPE = 'DOUBLE' )
*
*    Local variables :
*
      CHARACTER*80              TEXT(MAXLINES)         ! History text

      REAL                      SQ_RES                 ! Spline fit control
      REAL                      ACC                    ! Fit accuracy achieved

      INTEGER                   COEFF                  ! Spline coefficients
      INTEGER                   I                      ! Loop over axes
      INTEGER                   IDPTR, IVPTR, IQPTR    ! Input data
      INTEGER			IFID			! Input dataset id
      INTEGER                   IAPTR(ADI__MXDIM)      ! pointers
      INTEGER                   LIWRK, IWRKPTR         ! NAG integer workspace
      INTEGER                   DIMS(ADI__MXDIM)       ! Input data size
      INTEGER                   KNOTPTR(ADI__MXDIM)    ! Knot position arrays
      INTEGER                   MAXKNOT                ! Maximum no of knots
      INTEGER                   NDIM                   ! Input dimensionality
      INTEGER                   NELM                   ! Total no. of elements
      INTEGER                   NEST(ADI__MXDIM)       ! Max # knots per dim
      INTEGER                   NHREC                  ! # history recs used
      INTEGER                   NKNOT(ADI__MXDIM)      ! Actual # of knots
      INTEGER                   ODPTR           	! Output data pointers
      INTEGER			OFID			! Output dataset id
      INTEGER                   TLEN                    ! String length
      INTEGER                   WGTPTR                  ! Weights array
      INTEGER                   LWRK,WRKPTR             ! NAG float workspace

      LOGICAL                   OK                     ! General validity
      LOGICAL                   QUAL_OK, VAR_OK        ! Quality,variance ok?
      LOGICAL                   USE_WEIGHTS            ! Use variance & quality
*
*    Version :
*
      CHARACTER*30		VERSION
        PARAMETER  		( VERSION = 'SPLINEFIT Version 2.1-0b' )
*-

*  Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      CALL MSG_PRNT(VERSION)
      CALL NAG_MISSING( 'E02DCF, E02DDF', STATUS )
      CALL AST_INIT()

*  Get input and output
      CALL USI_ASSOC( 'INP', 'BinDS|Array', 'READ', IFID, STATUS )
      CALL USI_CLONE( 'INP', 'OUT', 'BinDS', OFID, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Check input
      CALL BDI_CHK( IFID, 'Data', OK, STATUS )
      CALL BDI_GETSHP( IFID, ADI__MXDIM, DIMS, NDIM, STATUS )
      IF ( .NOT. OK ) THEN
        STATUS = SAI__ERROR
        CALL ERR_REP( ' ', 'Invalid dataset', STATUS )
        GOTO 99
      END IF
      CALL BDI_CHK( IFID, 'Variance', VAR_OK, STATUS )
      IF ( VAR_OK ) THEN
        CALL MSG_PRNT('Data errors present')
      ELSE
        CALL MSG_PRNT('No data errors')
      END IF

      CALL BDI_CHK( IFID, 'Quality', QUAL_OK, STATUS )
      IF ( .NOT. QUAL_OK ) THEN
        CALL MSG_PRNT('No data quality')
      ELSE
        CALL MSG_PRNT('Data quality present')
        CALL BDI_MAPL( IFID, 'LogicalQuality', 'READ', IQPTR, STATUS )
      END IF

*  Map input data
      CALL BDI_MAP( IFID, 'Data', MTYPE, 'READ', IDPTR, STATUS )

*  Create and map output data
      CALL BDI_MAP( OFID, 'Data', MTYPE, 'WRITE', ODPTR, STATUS )
      IF ( VAR_OK ) THEN
        CALL BDI_MAP( OFID, 'Variance', MTYPE, 'READ', IVPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Total number of data elements
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*    Check enough data items
      IF ( NDIM .EQ. 1 ) THEN
        IF ( NELM .LT. 4 ) THEN
          CALL MSG_PRNT( 'One dimensional fitting requires at'/
     :                                     /' least 4 points' )
          STATUS = SAI__ERROR
          GOTO 99
        END IF
      END IF

*  Map axes
      DO I = 1, NDIM

*    Data exists?
        CALL BDI_AXCHK( IFID, I, 'Data', OK, STATUS )
        IF ( .NOT. OK ) THEN
          CALL MSG_SETI( 'N', I )
          CALL MSG_PRNT( 'No axis ^N data, assuming regularly spaced' )
        END IF

*    Map regardless, this routine will invent valuers
        CALL BDI_AXMAP( IFID, I, 'Data', MTYPE, 'READ', IAPTR(I),
     :                  STATUS )

      END DO

*  Number of points in dataset
      CALL MSG_SETI( 'NP', NELM )
      CALL MSG_PRNT( 'There are ^NP valid points in the dataset' )

*  Spline control parameters
      CALL USI_GET0R( 'SQ_RES', SQ_RES, STATUS )

*  Use weights and quality?
      USE_WEIGHTS = .TRUE.
      IF ( VAR_OK .AND. ( NDIM .GT. 1 ) ) THEN
        CALL USI_GET0L( 'USEW', USE_WEIGHTS, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map weights array and calculate from variances
      IF ( USE_WEIGHTS ) THEN
        CALL DYN_MAPT( 1, NELM, MTYPE, WGTPTR, STATUS )
        CALL SPLINEFIT_WGTD( NELM, VAR_OK, %VAL(IVPTR), QUAL_OK,
     :                       %VAL(IQPTR), %VAL(WGTPTR), STATUS )
      END IF

*  Map space for knots
      MAXKNOT = 1
      DO I = 1, NDIM
        NEST(I) = DIMS(I) + 4
        MAXKNOT = MAXKNOT * NEST(I)
      END DO
      CALL DYN_MAPT( 1, MAXKNOT, MTYPE, KNOTPTR(1), STATUS )
      DO I = 2, NDIM
        KNOTPTR(I) = KNOTPTR(I-1) + (NEST(I-1)-1)*8
      END DO

*  Workspace for spline coefficients
      CALL DYN_MAPT( 1, MAXKNOT, MTYPE, COEFF, STATUS )

*  Floating point workspace
      IF ( NDIM .EQ. 1 ) THEN
        LWRK = 4*NELM + 16*NEST(1) + 41
      ELSE
        LWRK = 4*(DIMS(1)+DIMS(2)) + 11*(NEST(1)+NEST(2)) +
     :        NEST(1)*DIMS(1) + MAX( DIMS(2), NEST(1)) + 54
      END IF
      CALL DYN_MAPT( 1, LWRK, MTYPE, WRKPTR, STATUS )

*  Get integer workspace
      IF ( NDIM .EQ. 1 ) THEN
        LIWRK = NEST(1)
      ELSE
        LIWRK = 3 + DIMS(1) + DIMS(2) + NEST(1) + NEST(2)
      END IF
      CALL DYN_MAPI( 1, LIWRK, IWRKPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Fit spline
      IF ( NDIM .EQ. 1 ) THEN
        CALL SPLINEFIT_1D( DIMS, %VAL(IAPTR(1)), %VAL(IDPTR), SQ_RES,
     :                          %VAL(WGTPTR), NEST, %VAL(KNOTPTR(1)),
     :                        %VAL(COEFF), LWRK, %VAL(WRKPTR), LIWRK,
     :               %VAL(IWRKPTR), NKNOT, ACC, %VAL(ODPTR), STATUS )

      ELSE IF ( ( NDIM .EQ. 2 ) .AND. .NOT. USE_WEIGHTS ) THEN
        CALL SPLINEFIT_2D( DIMS, %VAL(IAPTR(1)), %VAL(IAPTR(2)),
     :              %VAL(IDPTR), SQ_RES, NEST, %VAL(KNOTPTR(1)),
     :                      %VAL(KNOTPTR(2)), %VAL(COEFF), LWRK,
     :                       %VAL(WRKPTR), LIWRK, %VAL(IWRKPTR),
     :                         NKNOT, ACC, %VAL(ODPTR), STATUS )

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*  Free workspace
      CALL DYN_UNMAP( WRKPTR, STATUS )
      CALL DYN_UNMAP( IWRKPTR, STATUS )

*  Write history
      CALL HSI_ADD( OFID, VERSION, STATUS )
      TEXT(1) = 'Input : {INP}'
      TEXT(2) = ' '
      CALL MSG_SETR( 'FP', ACC )
      CALL MSG_MAKE( '  Weighted sum of squared residuals = ^FP',
     :                                            TEXT(3), TLEN )
      CALL MSG_PRNT( TEXT(3)(:TLEN) )
      NHREC = MAXLINES
      CALL USI_TEXT( 3, TEXT, NHREC, STATUS )
      CALL HSI_PTXT( OFID, NHREC, TEXT, STATUS )

*  Tidy up
 99   CALL AST_CLOSE( )
      CALL AST_ERR( STATUS )

      END




*+  SPLINEFIT_WGTD - Find weights array from input DOUBLE variances
      SUBROUTINE SPLINEFIT_WGTD( N, V_OK, VAR, Q_OK, QUAL, WGT, STATUS )
*
*    Description :
*
*    History :
*
*     11 Jun 91 : Original (BHVAD::DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER                    N                  ! Number of data items
      LOGICAL                    V_OK               ! Variance ok?
      DOUBLE PRECISION           VAR(*)             ! Variance data
      LOGICAL                    Q_OK               ! Quality ok?
      LOGICAL                    QUAL(*)            ! Quality data
*
*    Export :
*
      DOUBLE PRECISION           WGT(*)             ! Weights array
*
*    Local constants :
*
      DOUBLE PRECISION           SMALL
         PARAMETER               ( SMALL = 1.0E-10 )
*
*    Local variables :
*
      INTEGER                    I                  ! Loop over data
*-

*    Check status
      IF ( STATUS .EQ. SAI__OK ) THEN
        IF ( V_OK ) THEN
          IF ( Q_OK ) THEN
            DO I = 1, N
              IF ( QUAL(I) .AND. ( VAR(I) .NE. 0.0 ) ) THEN
                WGT(I) = 1.0D0 / VAR(I)
              ELSE
                WGT(I) = SMALL
              END IF
            END DO
          ELSE
            DO I = 1, N
              IF ( VAR(I) .NE. 0.0D0 ) THEN
                WGT(I) = 1.0D0 / VAR(I)
              ELSE
                WGT(I) = SMALL
              END IF
            END DO
          END IF
        ELSE
          IF ( Q_OK ) THEN
            DO I = 1, N
              IF ( QUAL(I) ) THEN
                WGT(I) = 1.0D0
              ELSE
                WGT(I) = SMALL
              END IF
            END DO
          ELSE
            DO I = 1, N
              WGT(I) = 1.0D0
            END DO
          END IF
        END IF
      END IF

      END



*+  SPLINEFIT_1D - Perform a one dimensional spline fit
      SUBROUTINE SPLINEFIT_1D( N, AXIS, DATA, SFACTOR, WGT, NEST,
     :            LAMBDA, C, LWRK, WRK, LIWRK, IWRK, NKNOT, FP, OUT,
     :                                                     STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Jun 91 : Original (BHVAD::DJA)
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
      INTEGER                    N                      ! Total no. of elements
      DOUBLE PRECISION           DATA(*)                ! Data
      DOUBLE PRECISION           AXIS(*)                ! Axis data
      DOUBLE PRECISION           WGT(*)                 ! Weights
      REAL                       SFACTOR                ! Accuracy factor
      INTEGER                    NEST                   ! Max number of knots
*
*    Workspace :
*
      INTEGER                    LWRK                   ! Amount of FLOAT work
      DOUBLE PRECISION           WRK(*)                 !
      INTEGER                    LIWRK                  ! Amount of INT work
      INTEGER                    IWRK(*)                !
*
*    Export :
*
      DOUBLE PRECISION           LAMBDA(*), C(*)        ! Knot positions
      INTEGER                    NKNOT                  ! # of knots
      REAL                       FP                     ! Fit achieved
      DOUBLE PRECISION           OUT(*)                 ! Fit data
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*1                START                  ! NAG control variable

      DOUBLE PRECISION           DFP                    ! Fit residual

      INTEGER                    I                      ! Loop over data values
      INTEGER                    IFAIL                  ! NAG status
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      START = 'C'
      IFAIL = -1

*    Perform fit
c     CALL E02BEF(START, N, AXIS, DATA, WGT, DBLE(SFACTOR), NEST, NKNOT,
c    :                          LAMBDA, C, DFP, WRK, LWRK, IWRK, IFAIL )
      FP = REAL( DFP )

*    Trap fit failure
      IF ( IFAIL .GT. 0 ) THEN
        IF ( IFAIL .GT. 3 ) THEN
          CALL MSG_PRNT('The fit could not reach the requested'/
     :                                             /' accuracy')
        ELSE
          CALL MSG_PRNT('! error in the spline fit')
          STATUS = SAI__ERROR
          GOTO 99
        END IF
      END IF

*    Evaluate spline
      IFAIL = -1
      DO I = 1, N
c       CALL E02BBF( NKNOT, LAMBDA, C, AXIS(I), OUT(I), IFAIL )
      END DO
      IF ( IFAIL .GT. 0 ) THEN
        CALL MSG_PRNT( 'Error evaulating spline fit' )
        STATUS = SAI__ERROR
      END IF

 99   CONTINUE

      END



*+  SPLINEFIT_2D - Perform a two dimensional spline fit, no weights
      SUBROUTINE SPLINEFIT_2D( DIMS, AXIS1, AXIS2, DATA, SFACTOR,
     :                            NEST, LAMBDA, MU, C, LWRK, WRK,
     :                      LIWRK, IWRK, NKNOT, FP, OUT, STATUS )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Jun 91 : Original (BHVAD::DJA)
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
      INTEGER                    DIMS(2)                ! Array size
      DOUBLE PRECISION           DATA(*)                ! Data
      DOUBLE PRECISION           AXIS1(*)               ! X axis data
      DOUBLE PRECISION           AXIS2(*)               ! Y axis data
      DOUBLE PRECISION           SFACTOR                ! Accuracy factor
      INTEGER                    NEST(2)                ! Max number of knots
*
*    Workspace :
*
      INTEGER                    LWRK                   ! Amount of FLOAT work
      DOUBLE PRECISION           WRK(*)                 !
      INTEGER                    LIWRK                  ! Amount of INT work
      INTEGER                    IWRK(*)                !
*
*    Export :
*
      DOUBLE PRECISION           LAMBDA(*), MU(*), C(*) ! Knot positions
      INTEGER                    NKNOT(2)               ! # of knots
      DOUBLE PRECISION           FP                     ! Fit achieved
      DOUBLE PRECISION           OUT(*)                 ! Fit data
*
*    Status :
*
      INTEGER STATUS
*
*    Local variables :
*
      CHARACTER*1                START                  ! NAG control variable

      INTEGER                    IFAIL                  ! NAG status
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      START = 'C'
      IFAIL = -1

*    Perform fit
c     CALL E02DCF( START, DIMS(1), AXIS1, DIMS(2), AXIS2, DATA,
c    :             SFACTOR, NEST(1), NEST(2), NKNOT(1), LAMBDA,
c    :             NKNOT(2), MU, C, FP, WRK, LWRK, IWRK, LIWRK,
c    :                                                  IFAIL )

*    Trap fit failure
      IF ( IFAIL .GT. 0 ) THEN
        IF ( IFAIL .GT. 3 ) THEN
          CALL MSG_PRNT('The fit could not reach the requested'/
     :                                             /' accuracy')
        ELSE
          CALL MSG_PRNT('! error in the spline fit')
          STATUS = SAI__ERROR
          GOTO 99
        END IF
      END IF

*    Evaluate spline
      IFAIL = -1
c     CALL E02DFF( DIMS(1), DIMS(2), NKNOT(1), NKNOT(2), AXIS1, AXIS2,
c    :             LAMBDA, MU, C, OUT, WRK, LWRK, IWRK, LIWRK, IFAIL )
      IF ( IFAIL .GT. 0 ) THEN
        CALL MSG_PRNT( 'Error evaulating spline fit' )
        STATUS = SAI__ERROR
      END IF

 99   CONTINUE

      END
