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
*    Status :
*
      INTEGER STATUS
*
*    Local constants :
*
      INTEGER                    MAXLINES
         PARAMETER               ( MAXLINES = 6 )
      CHARACTER*(DAT__SZTYP)     MAPTYPE
         PARAMETER               ( MAPTYPE = '_DOUBLE' )
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC)     ALOC                   ! Output dataset ASTERIX
      CHARACTER*(DAT__SZLOC)     ILOC                   ! Input dataset
      CHARACTER*(DAT__SZLOC)     OLOC                   ! Output dataset
      CHARACTER*6                PREC                   ! Precision required
      CHARACTER*80               TEXT(MAXLINES)         ! History text

      REAL                       SQ_RES                 ! Spline fit control
      REAL                       ACC                    ! Fit accuracy achieved

      INTEGER                    COEFF                  ! Spline coefficients
      INTEGER                    I                      ! Loop over axes
      INTEGER                    IDPTR, IVPTR, IQPTR    ! Input data
      INTEGER                    IAPTR(DAT__MXDIM)      ! pointers
      INTEGER                    LIWRK, IWRKPTR         ! NAG integer workspace
      INTEGER                    DIMS(DAT__MXDIM)       ! Input data size
      INTEGER                    KNOTPTR(DAT__MXDIM)    ! Knot position arrays
      INTEGER                    MAXKNOT                ! Maximum no of knots
      INTEGER                    NDIM                   ! Input dimensionality
      INTEGER                    NELM                   ! Total no. of elements
      INTEGER                    NEST(DAT__MXDIM)       ! Max # knots per dim
      INTEGER                    NHREC                  ! # history recs used
      INTEGER                    NKNOT(DAT__MXDIM)      ! Actual # of knots
      INTEGER                    ODPTR, OVPTR           ! Output data pointers
      INTEGER                    TDIMS(DAT__MXDIM)      ! Dummy dims array
      INTEGER                    TLEN                   ! String length
      INTEGER                    TNDIM                  ! Temp dimensionality
      INTEGER                    WGTPTR                 ! Weights array
      INTEGER                    LWRK,WRKPTR            ! NAG float workspace

      LOGICAL                    ANYBAD                 ! Any bad quality data?
      LOGICAL                    AFLIP(DAT__MXDIM)      ! Flipped axis
      LOGICAL                    OK                     ! General validity
      LOGICAL                    PRIM                   ! Input primitive
      LOGICAL                    QUAL_OK, VAR_OK        ! Quality,variance ok?
      LOGICAL                    REG                    ! Axis regular?
      LOGICAL                    USE_WEIGHTS            ! Use variance & quality
*
*    Version :
*
      CHARACTER*30 VERSION
        PARAMETER  (VERSION = 'SPLINEFIT Version 1.8-0')
*-

*    Check status
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Initialise
      CALL MSG_PRNT(VERSION)
      CALL AST_INIT()

*    Get locators to input and output
      CALL USI_ASSOC2( 'INP', 'OUT', 'READ', ILOC, OLOC, PRIM, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Copy input to output
      CALL BDA_COPAXES( ILOC, OLOC, STATUS )
      CALL BDA_COPMORE( ILOC, OLOC, STATUS )
      CALL BDA_COPTEXT( ILOC, OLOC, STATUS )

*    Check input
      CALL BDA_CHKDATA( ILOC, OK, NDIM, DIMS, STATUS )
      IF ( .NOT. OK ) THEN
        CALL MSG_PRNT('! invalid dataset')
        STATUS = SAI__ERROR
        GOTO 99
      END IF
      CALL BDA_CHKVAR( ILOC, VAR_OK, TNDIM, TDIMS, STATUS )
      IF ( VAR_OK ) THEN
        CALL MSG_PRNT('Data errors present')
      ELSE
        CALL MSG_PRNT('No data errors')
      END IF

      CALL BDA_CHKQUAL( ILOC, QUAL_OK, TNDIM, TDIMS, STATUS )
      IF ( .NOT. QUAL_OK ) THEN
        CALL MSG_PRNT('No data quality')
      ELSE
        CALL MSG_PRNT('Data quality present')
        CALL BDA_MAPLQUAL( ILOC, 'READ', ANYBAD, IQPTR, STATUS )
        IF ( .NOT. ANYBAD ) THEN
          CALL BDA_UNMAPLQUAL( ILOC, STATUS )
          QUAL_OK = .FALSE.
        END IF
      END IF

*    Map input data
      CALL BDA_MAPTDATA( ILOC, MAPTYPE, 'READ', IDPTR, STATUS )

*    Create and map output data
      CALL BDA_CREDATA( OLOC, NDIM, DIMS, STATUS )
      CALL BDA_MAPTDATA( OLOC, MAPTYPE, 'WRITE', ODPTR, STATUS )
      IF ( VAR_OK ) THEN
        CALL BDA_MAPTVAR( ILOC, MAPTYPE, 'READ', IVPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Total number of data elements
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

*    Check and map axes if not primitive
      IF ( .NOT. PRIM ) THEN
        DO I = 1, NDIM
          CALL BDA_CHKAXVAL( ILOC, I, OK, REG, TDIMS, STATUS )
          IF ( OK ) THEN
            CALL BDA_MAPTAXVAL( ILOC, MAPTYPE, 'READ', I, IAPTR(I),
     :                                                   STATUS )
          ELSE
            CALL MSG_SETI( 'N', I )
            CALL MSG_PRNT( 'Axis ^N invalid, assuming regularly'/
     :                                               /' spaced' )
          END IF
        END DO

*    otherwise map arrays for pixel centres
      ELSE

        CALL MSG_PRNT( 'No axis data, assuming regularly spaced' )
        DO I = 1, NDIM
          CALL DYN_MAPT( 1, DIMS(I), MAPTYPE, IAPTR(I), STATUS )
          CALL ARR_REG1D( 0.5D0, 1.0D0, DIMS(I), %VAL(IAPTR(I)),
     :                                                  STATUS )
        END DO

      END IF

*    Number of points in dataset
      CALL MSG_SETI( 'NP', NELM )
      CALL MSG_PRNT( 'There are ^NP valid points in the dataset' )

*    Spline control parameters
      CALL USI_GET0R( 'SQ_RES', SQ_RES, STATUS )

*    Use weights and quality?
      USE_WEIGHTS = .TRUE.
      IF ( VAR_OK .AND. ( NDIM .GT. 1 ) ) THEN
        CALL USI_GET0L( 'USEW', USE_WEIGHTS, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Map weights array and calculate from variances
      IF ( USE_WEIGHTS ) THEN
        CALL DYN_MAPT( 1, NELM, MAPTYPE, WGTPTR, STATUS )
        CALL SPLINEFIT_WGTD( NELM, VAR_OK, %VAL(IVPTR), QUAL_OK,
     :                       %VAL(IQPTR), %VAL(WGTPTR), STATUS )
      END IF

*    Unmap stuff no longer needed
      IF ( VAR_OK ) CALL BDA_UNMAPVAR( ILOC, STATUS )
      IF ( QUAL_OK ) CALL BDA_UNMAPLQUAL( ILOC, STATUS )

*    Map space for knots
      MAXKNOT = 1
      DO I = 1, NDIM
        NEST(I) = DIMS(I) + 4
        MAXKNOT = MAXKNOT * NEST(I)
      END DO
      CALL DYN_MAPT( 1, MAXKNOT, MAPTYPE, KNOTPTR(1), STATUS )
      DO I = 2, NDIM
        KNOTPTR(I) = KNOTPTR(I-1) + (NEST(I-1)-1)*8
      END DO

*    Workspace for spline coefficients
      CALL DYN_MAPT( 1, MAXKNOT, MAPTYPE, COEFF, STATUS )

*    Floating point workspace
      IF ( NDIM .EQ. 1 ) THEN
        LWRK = 4*NELM + 16*NEST(1) + 41
      ELSE
        LWRK = 4*(DIMS(1)+DIMS(2)) + 11*(NEST(1)+NEST(2)) +
     :        NEST(1)*DIMS(1) + MAX( DIMS(2), NEST(1)) + 54
      END IF
      CALL DYN_MAPT( 1, LWRK, MAPTYPE, WRKPTR, STATUS )

*    Get integer workspace
      IF ( NDIM .EQ. 1 ) THEN
        LIWRK = NEST(1)
      ELSE
        LIWRK = 3 + DIMS(1) + DIMS(2) + NEST(1) + NEST(2)
      END IF
      CALL DYN_MAPI( 1, LIWRK, IWRKPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Fit spline
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

*    Free workspace
      CALL DYN_UNMAP( WRKPTR, STATUS )
      CALL DYN_UNMAP( IWRKPTR, STATUS )

*    Write spline coefficients to output
      CALL BDA_LOCAST( OLOC, ALOC, STATUS )

*    Write history
      CALL HIST_ADD( OLOC, VERSION, STATUS )
      TEXT(1) = 'Input : {INP}'
      TEXT(2) = ' '
      CALL MSG_SETR( 'FP', ACC )
      CALL MSG_MAKE( '  Weighted sum of squared residuals = ^FP',
     :                                            TEXT(3), TLEN )
      CALL MSG_PRNT( TEXT(3)(:TLEN) )
      NHREC = MAXLINES
      CALL USI_TEXT( 3, TEXT, NHREC, STATUS )
      CALL HIST_PTXT( OLOC, NHREC, TEXT, STATUS )

*    Close files
      CALL BDA_CLOSE( ILOC, STATUS )
      CALL BDA_CLOSE( OLOC, STATUS )

*    Tidy up
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
      INCLUDE 'DAT_PAR'
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
      INCLUDE 'DAT_PAR'
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
      CALL E02BEF(START, N, AXIS, DATA, WGT, DBLE(SFACTOR), NEST, NKNOT,
     :                          LAMBDA, C, DFP, WRK, LWRK, IWRK, IFAIL )
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
        CALL E02BBF( NKNOT, LAMBDA, C, AXIS(I), OUT(I), IFAIL )
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
      INCLUDE 'DAT_PAR'
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
      CALL E02DCF( START, DIMS(1), AXIS1, DIMS(2), AXIS2, DATA,
     :             SFACTOR, NEST(1), NEST(2), NKNOT(1), LAMBDA,
     :             NKNOT(2), MU, C, FP, WRK, LWRK, IWRK, LIWRK,
     :                                                  IFAIL )

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
      CALL E02DFF( DIMS(1), DIMS(2), NKNOT(1), NKNOT(2), AXIS1, AXIS2,
     :             LAMBDA, MU, C, OUT, WRK, LWRK, IWRK, LIWRK, IFAIL )
      IF ( IFAIL .GT. 0 ) THEN
        CALL MSG_PRNT( 'Error evaulating spline fit' )
        STATUS = SAI__ERROR
      END IF

 99   CONTINUE

      END
