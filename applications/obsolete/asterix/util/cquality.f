*+  CQUALITY - Manipulate quality values in circular areas
      SUBROUTINE CQUALITY( STATUS )
*
*    Description :
*
*     Allows manipulation of quality values within circular or cylindrical
*     regions specified by the user. The available options are:
*
*       IGNORE  - Set temporary bad quality bit
*       RESTORE - Unset temporary bad quality bit
*       SET     - Set quality to specified value
*       AND     - AND existing QUALITY with specified QUALITY value
*       OR      - OR existing QUALITY with specified QUALITY value
*       EOR     - EOR existing QUALITY with specified QUALITY value
*       NOT     - NOT existing QUALITY with specified QUALITY value
*
*    Environment Parameters :
*
*     IGNORE    - Set temp bad qual bit?                 (Logical(F), read)
*     RESTORE   - Unset temp bad qual bit?               (Logical(F), read)
*     SET       - Set quality to specified value?        (Logical(F), read)
*     AND       - AND qualiity with specified value?     (Logical(F), read)
*     OR        - OR qualiity with specified value?      (Logical(F), read)
*     EOR       - EOR qualiity with specified value?     (Logical(F), read)
*     NOT       - NOT existing qualiity value?           (Logical(F), read)
*     OVERWRITE - Overwrite input file?                  (Logical(T), read)
*     QSEL      - Specify a quality value to alter?      (Logical(F), read)
*     INP       - Input file name.                       (Univ, read)
*     OUT       - Output filename                        (Univ, write)
*     QVAL      - Specified quality value                (Byte, read)
*     MODQUAL   - Quality value to modify                (Character, read)
*     QVAL      - Specified quality value                (Character, read)
*
*    Method :
*    Deficiencies :
*    Bugs :
*     DAT_UNMAP will generate bad status if any INPUT quality
*     values = 255 !!!
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*      9 May 91 : V1.4-0  Original. Copied from QUALITY - quality value
*                         changing done by QUALITY_<op> routines. (DJA)
*     19 Nov 92 : V1.7-0  Updated call to AXIS_VAL2PIX (DJA)
*     25 Feb 94 : V1.7-1  Use BIT_ routines to do bit manipulations (DJA)
*     24 Nov 94 : V1.8-0  Now use USI for user interface (DJA)
*
*    Type Definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
*
*    Status :
*
      INTEGER STATUS
*
*    Function definitions :
*
      BYTE                   BIT_NOTUB
*
*    Local Constants :
*
      INTEGER                MX_PTS                 ! Max no. circular regions
        PARAMETER            ( MX_PTS = 200 )
*
*    Local variables :
*
      CHARACTER*(DAT__SZLOC) ILOC                   ! Locator to input file
      CHARACTER*(DAT__SZLOC) OLOC                   ! Locator to output file
      CHARACTER*8            MODQUAL                ! Quality to modify
      CHARACTER*8            QVAL                   ! Quality value
      CHARACTER*80           TEXT(5)                ! History text
      CHARACTER*40           UNITS                  ! Axis units

      REAL                   XC(MX_PTS), YC(MX_PTS) ! Circular centres
      REAL                   RC(MX_PTS)             ! Circular radii

      INTEGER                AORDER(DAT__MXDIM)     !
      INTEGER                CPTR                   ! Pointer to dynamic
                                                    ! array
      INTEGER                DIMS(DAT__MXDIM)       ! Length of each dimension
      INTEGER                FSTAT                  ! i/o status
      INTEGER                I                      ! Loop counters
      INTEGER                INORDER(DAT__MXDIM)    ! Inverse of AORDER
      INTEGER                NCH                    ! Points changed
      INTEGER                NDIM                   ! Number of dimensions
      INTEGER                NELM                   ! Number of data points
      INTEGER                NMOD                   ! # modified points
      INTEGER                NRPTS, NXPTS, NYPTS    ! # values entered
      INTEGER                QDIMS(DAT__MXDIM)      ! Length of each dimension
      INTEGER                QNDIM                  ! Number of dimensions
      INTEGER                QPTR                   ! Pointer to mapped
                                                    ! QUALITY
      INTEGER                TDIMS(DAT__MXDIM)      ! Working dimensions
      INTEGER                TQPTR                  ! Temp quality array
      INTEGER                XPTR, YPTR             ! Spatial axis data

      BYTE                   BQVAL		    ! Quality value

      LOGICAL                INPRIM                 ! Is input primitive?
      LOGICAL                MODESET                ! Mode selected
      LOGICAL                MOVE_AXES              ! Move axes?
      LOGICAL                NOTON                  ! Point on X,Y plane?
      LOGICAL                OK                     ! OK?
      LOGICAL                OVERWRITE              ! Overwrite input dataset
      LOGICAL                QSEL                   ! Select QUALITY
                                                    ! Value to modify?
      LOGICAL                Q_AND, Q_OR, Q_IGNORE  ! Quality modes
      LOGICAL                Q_RESTORE, Q_SET, Q_EOR, Q_NOT
*
*    Version id :
*
      CHARACTER*30           VERSION
        PARAMETER           (VERSION = 'CQUALITY Version 1.8-0')
*-

      CALL MSG_PRNT( VERSION )

*    Initialize
      CALL AST_INIT

      MODESET=.FALSE.
      NMOD = 0

*    Obtain mode from user
      CALL USI_GET0L( 'SET', Q_SET, STATUS )
      MODESET = Q_SET
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'IGNORE', Q_IGNORE, STATUS )
        MODESET = Q_IGNORE
      END IF
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'RESTORE', Q_RESTORE, STATUS )
        MODESET = Q_RESTORE
      END IF
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'AND', Q_AND, STATUS )
        MODESET = Q_AND
      END IF
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'OR', Q_OR, STATUS )
        MODESET = Q_OR
      END IF
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'EOR', Q_EOR, STATUS )
        MODESET = Q_EOR
      END IF
      IF (.NOT.MODESET) THEN
        CALL USI_GET0L( 'NOT', Q_NOT, STATUS )
        MODESET = Q_NOT
      END IF

      IF ( Q_SET ) THEN
        CALL USI_GET0L( 'QSEL', QSEL, STATUS )
      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check that a mode has been selected
      IF ( .NOT. MODESET ) THEN
        CALL MSG_PRNT( '! No CQUALITY operation selected selected' )
        STATUS = SAI__ERROR
        GOTO 99
      END IF

*    Overwrite?
      CALL USI_GET0L( 'OVERWRITE', OVERWRITE, STATUS )

*    Get input
      IF ( OVERWRITE ) THEN
        CALL USI_ASSOCI( 'INP', 'UPDATE', ILOC, INPRIM, STATUS )
        OLOC = ILOC
      ELSE
        CALL USI_ASSOC2( 'INP', 'OUT', 'R', ILOC, OLOC, INPRIM, STATUS )
      END IF

*    Create output
      IF ( .NOT. OVERWRITE ) THEN
        CALL HDX_COPY( ILOC, OLOC, STATUS )
      END IF

*    Check status
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Validate dataset
      IF ( INPRIM ) THEN
        CALL MSG_PRNT ('! Input not a dataset')
        STATUS = SAI__ERROR
      ELSE
        CALL BDA_CHKDATA( OLOC, OK, NDIM, DIMS, STATUS )
        IF ( ( STATUS .EQ. SAI__OK ) .AND. .NOT. OK ) THEN
          CALL MSG_PRNT( '! Invalid data' )
          STATUS = SAI__ERROR
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Check axes
      CALL AXIS_GETORD( OLOC, 'X,Y', MOVE_AXES, AORDER,
     :                            NDIM, TDIMS, STATUS )

*    Find length of array
      CALL ARR_SUMDIM( NDIM, DIMS, NELM )

*    Check QUALITY
      CALL BDA_CHKQUAL( OLOC, OK, QNDIM, QDIMS, STATUS )

      IF ( .NOT. OK ) THEN
        IF ( Q_SET .OR. Q_IGNORE ) THEN
          CALL BDA_CREQUAL( OLOC, NDIM, DIMS, STATUS )
          CALL BDA_MAPQUAL( OLOC, 'WRITE', QPTR, STATUS )
          CALL ARR_INIT1B( QUAL__GOOD, NELM, %VAL(QPTR), STATUS )
          CALL BDA_PUTMASK( OLOC, QUAL__MASK, STATUS )
        ELSE
          STATUS = SAI__ERROR
          CALL ERR_REP( ' ', 'No input quality!', STATUS )
        END IF
      ELSE
        CALL BDA_MAPQUAL( OLOC, 'UPDATE', QPTR, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Adjust size of array to 7 D
      DO I = NDIM + 1, DAT__MXDIM
        DIMS(I) = 1
      END DO

*    Quality needing moved due to axis order?
      IF ( MOVE_AXES ) THEN
        CALL MSG_PRNT( 'Changing axis order to X,Y...' )
        CALL DYN_MAPB( NDIM, TDIMS, TQPTR, STATUS )
        CALL AR7_AXSWAP_B( DIMS, %VAL(QPTR), AORDER, TDIMS,
     :                                %VAL(TQPTR), STATUS )
      ELSE
        TQPTR = QPTR
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get axis units for message
      CALL BDA_GETAXUNITS( OLOC, AORDER(1), UNITS, STATUS )
      IF ( UNITS .GT. ' ' ) THEN
        CALL CHR_UCASE( UNITS )
        CALL MSG_SETC( 'UNIT', UNITS )
        CALL MSG_PRNT( 'Specify circular regions in ^UNIT' )
      END IF

*    Get centres and radii for positions
      CALL USI_GET1R( 'CX', MX_PTS, XC, NXPTS, STATUS )
      CALL USI_GET1R( 'CY', MX_PTS, YC, NYPTS, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99
      IF ( NXPTS .LT. 1 ) THEN
        CALL MSG_PRNT( '! No X values supplied' )
        STATUS = SAI__ERROR
      ELSE IF ( NXPTS .NE. NYPTS ) THEN
        CALL MSG_PRNT( '! Numbers of X and Y values differ' )
        STATUS = SAI__ERROR
      END IF
      CALL USI_GET1R( 'CR', MX_PTS, RC, NRPTS, STATUS )
      IF ( STATUS .EQ. SAI__OK ) THEN
        IF ( ( NRPTS .NE. 1 ) .AND. ( NRPTS .NE. NXPTS ) ) THEN
          CALL MSG_PRNT( '! Numbers of radii must be 1 or same as'/
     :                                   /' number of X,Y values' )
          STATUS = SAI__ERROR
        ELSE IF ( ( NRPTS .EQ. 1 ) .AND. ( NXPTS .GT. 1 ) ) THEN
          CALL ARR_INIT1R( RC(1), NXPTS, RC, STATUS )
        END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Set up temporary array for flagging selections - initialise to wanted
      CALL DYN_MAPL( 1, NELM, CPTR, STATUS )
      CALL ARR_INIT1L( .FALSE., NELM, %VAL(CPTR), STATUS )
      NMOD = 0

*    Map two spatial axes
      CALL BDA_MAPAXVAL( OLOC, 'READ', AORDER(1), XPTR, STATUS )
      CALL BDA_MAPAXVAL( OLOC, 'READ', AORDER(2), YPTR, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Set elements of selection array
      DO I = 1, NXPTS
        CALL CQUALITY_CIRSEL( TDIMS, XC(I), YC(I), RC(I), %VAL(XPTR),
     :                  %VAL(YPTR), %VAL(CPTR), NMOD, NOTON, STATUS )
        IF ( NOTON ) THEN
          CALL MSG_SETI( 'N', I )
          CALL MSG_PRNT( 'Point ^N is not within the bounds'/
     :                                   /' of the dataset' )
        END IF
      END DO
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Get quality value to modify
      IF ( QSEL ) THEN
        CALL QUALITY_GETQV( 'MODQUAL', ' ', MODQUAL, BQVAL, STATUS )
        CALL QUALITY_SETQSEL( NELM, BQVAL, %VAL(TQPTR), %VAL(CPTR),
     :                                              NMOD, STATUS )
      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Write the output quality
      IF ( Q_SET ) THEN
        CALL QUALITY_GETQV( 'QVAL', 'New quality value', QVAL,
     :                                         BQVAL, STATUS )
        TEXT(1) = '     QUALITY set to '//QVAL
        CALL QUALITY_SET( NELM, BQVAL, %VAL(CPTR), %VAL(TQPTR), NCH,
     :                                                      STATUS )

      ELSE IF ( Q_IGNORE ) THEN
        BQVAL = QUAL__IGNORE
        CALL QUALITY_OR( NELM, BQVAL, %VAL(CPTR), %VAL(TQPTR), NCH,
     :                                                     STATUS )
        TEXT(1) = '     IGNORE mode'

      ELSE IF ( Q_RESTORE ) THEN
        BQVAL = BIT_NOTUB( QUAL__IGNORE )
        CALL QUALITY_AND( NELM, BQVAL, %VAL(CPTR), %VAL(TQPTR), NCH,
     :                                                      STATUS )
        TEXT(1) = '     RESTORE mode'

      ELSE IF ( Q_AND ) THEN
        CALL QUALITY_GETQV( 'QVAL', 'Value for AND operation', QVAL,
     :                                               BQVAL, STATUS )
        CALL QUALITY_AND( NELM, BQVAL, %VAL(CPTR), %VAL(TQPTR), NCH,
     :                                                       STATUS )
        TEXT(1) = '     QUALITY ANDed with '//QVAL

      ELSE IF ( Q_OR ) THEN
        CALL QUALITY_GETQV( 'QVAL', 'Value for OR operation', QVAL,
     :                                              BQVAL, STATUS )
        CALL QUALITY_OR( NELM, BQVAL, %VAL(CPTR), %VAL(TQPTR), NCH,
     :                                                     STATUS )
        TEXT(1) = '     QUALITY ORed with '//QVAL

      ELSE IF ( Q_EOR ) THEN
        CALL QUALITY_GETQV( 'QVAL', 'Value for EOR operation', QVAL,
     :                                               BQVAL, STATUS )
        CALL QUALITY_EOR( NELM, BQVAL, %VAL(CPTR), %VAL(TQPTR), NCH,
     :                                                      STATUS )
        TEXT(1) = '     QUALITY EORed with '//QVAL

      ELSE IF ( Q_NOT ) THEN
        CALL QUALITY_NOT( NELM, %VAL(CPTR), %VAL(TQPTR), NCH, STATUS )
        TEXT(1) = '     QUALITY complemented'

      END IF
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Unmap quality, restoring axes if necessary
      IF ( MOVE_AXES ) THEN
        CALL MSG_PRNT( 'Restoring axis order...' )
        CALL AXIS_ORDINV( NDIM, AORDER, INORDER )
        CALL AR7_AXSWAP_B( TDIMS, %VAL(TQPTR), INORDER, DIMS,
     :                                   %VAL(QPTR), STATUS )
        CALL DYN_UNMAP( TQPTR, STATUS )
      END IF
      CALL BDA_UNMAPQUAL( OLOC, STATUS )
      IF ( STATUS .NE. SAI__OK ) GOTO 99

*    Inform user of altered points
      CALL MSG_SETI( 'NMOD', NMOD )
      CALL MSG_PRNT( '^NMOD points had quality modified' )

*    Write history
      CALL HIST_ADD( OLOC, VERSION, STATUS )
      IF ( QSEL ) THEN
        TEXT(2) = ' Only points having QUALITY = '//MODQUAL//' altered'
      END IF

 80   FORMAT( A,<NXPTS>E10.4 )
      WRITE( TEXT(3), 80, IOSTAT=FSTAT ) 'X-pos : ',(XC(I),I=1,NXPTS)
      WRITE( TEXT(4), 80, IOSTAT=FSTAT ) 'X-pos : ',(YC(I),I=1,NXPTS)
      WRITE( TEXT(5), 80, IOSTAT=FSTAT ) 'Radii : ',(RC(I),I=1,NRPTS)

      CALL HIST_PTXT( OLOC, 5, TEXT, STATUS )

*    Exit
 99   CALL AST_CLOSE()
      CALL AST_ERR( STATUS )

      END



*+  CQUALITY_CIRSEL - Use circular ranges to select valid output pixels
      SUBROUTINE CQUALITY_CIRSEL( DIMS, XC, YC, RC, XAX, YAX, SEL,
     :                                       NMOD, NOTON, STATUS )
*    Description :
*    History :
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
      INTEGER                DIMS(*)            ! DATA_ARRAY dimensions
      REAL                   XC,YC,RC           ! Circle centre and radius
      REAL                   XAX(*), YAX(*)     ! Spatial axes
*
*    Export :
*
      LOGICAL                SEL(*)             ! Selection array
      INTEGER                NMOD               ! # points modified
      LOGICAL                NOTON              ! Point in plane?
*
*    Status :
*
      INTEGER STATUS
*-
      IF (STATUS.NE.SAI__OK) RETURN

      CALL CQUALITY_CIRSEL_INT(DIMS(1),DIMS(2),DIMS(3),DIMS(4),DIMS(5),
     :                         DIMS(6),DIMS(7), XC, YC, RC, XAX, YAX,
     :                                       SEL, NMOD, NOTON, STATUS )

      END



*+  CQUALITY_CIRSEL_INT - Use circular ranges to select valid output pixels
      SUBROUTINE CQUALITY_CIRSEL_INT( D1,D2,D3,D4,D5,D6,D7, XC, YC,
     :                     RC, XAX, YAX, SEL, NMOD, NOTON, STATUS )
*    Description :
*     Loops over input data array setting SELECT = .TRUE. if values lie
*     within circular region
*    History :
*    Type definitions :
*
      IMPLICIT NONE
*
*    Status :
*
      INTEGER STATUS
*
*    Import :
*
      INTEGER                D1,D2,D3,D4,D5,D6,D7 ! DATA_ARRAY dimensions
      REAL                   XC,YC,RC             ! Circle centre and radius
      REAL                   XAX(D1), YAX(D2)     ! Spatial axes
*
*    Export :
*
      LOGICAL                SEL(D1,D2,D3,D4,D5,D6,D7)
      INTEGER                NMOD                 ! # points modified
      LOGICAL                NOTON                ! Point in plane?
*
*    Local variables :
*
      REAL                   R2, R2Y, RC2, XDIR, YDIR
      REAL                   XD, YD                 !

      INTEGER                A,B,C,D,E,X,Y          ! Loop counters
      INTEGER                XLO, XHI               ! X axis bounds
      INTEGER                YLO, YHI               ! Y axis bounds

*-

*    Find greatest extent of circle in plane
      XDIR = SIGN( 1.0, XAX(2)-XAX(1) )
      YDIR = SIGN( 1.0, YAX(2)-YAX(1) )
      NOTON = .TRUE.
      XD = ( XC - (XAX(1)-XDIR*RC) ) /
     :                    ((XAX(D1)+XDIR*RC)-(XAX(1)-XDIR*RC))
      YD = ( YC - (YAX(1)-YDIR*RC) ) /
     :                    ((YAX(D2)+YDIR*RC)-(YAX(1)-YDIR*RC))
      IF ( ( XD .LT. 0.0 ) .OR. ( XD .GT. 1.0 ) .OR.
     :     ( YD .LT. 0.0 ) .OR. ( YD .GT. 1.0 ) ) GOTO 99
      NOTON = .FALSE.
      CALL AXIS_VAL2PIX( D1, XAX, .FALSE., XC-XDIR*RC, XC+XDIR*RC,
     :                                          XLO, XHI, STATUS )
      CALL AXIS_VAL2PIX( D2, YAX, .FALSE., YC-YDIR*RC, YC+YDIR*RC,
     :                                          YLO, YHI, STATUS )

      RC2 = RC*RC

*    Loop over all slices
      DO A = 1, D7
        DO B = 1, D6
          DO C = 1, D5
            DO D = 1, D4
              DO E = 1, D3

*              Test each pixel in restricted region
                DO Y = YLO, YHI
                  R2Y = (YAX(Y)-YC)**2
                  DO X = XLO, XHI
                    R2 = (XAX(X)-XC)**2 + R2Y
                    IF ( R2 .LE. RC2 ) THEN
                      SEL(X,Y,E,D,C,B,A) = .TRUE.
                      NMOD = NMOD + 1
                    END IF
                  END DO
                END DO

              END DO
            END DO
          END DO
        END DO
      END DO

 99   CONTINUE

      END
