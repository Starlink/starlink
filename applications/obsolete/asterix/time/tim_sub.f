*+  TIM_SUB - common subroutines for t/s system
      SUBROUTINE  TIM_SUB(STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from ',STATUS)
        ENDIF

      ENDIF


      END



      SUBROUTINE  TIM_CHECK(FID,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
      INTEGER FID
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER NDIM,DIMS(ADI__MXDIM)
      LOGICAL DOK,AOK,WOK
*-
      DOK=.FALSE.

      IF (STATUS.EQ.SAI__OK) THEN

*  check main data array
        CALL BDI_CHK( FID, 'Data', DOK, STATUS )
        CALL BDI_GETSHP( FID, ADI__MXDIM, DIMS, NDIM, STATUS )
        IF (.NOT.DOK) THEN
          CALL MSG_PRNT('AST_ERR: invalid data')
          STATUS=SAI__ERROR
        ELSEIF (NDIM.NE.1) THEN
          CALL MSG_PRNT('AST_ERR: dataset not a time-series')
          STATUS=SAI__ERROR
        ELSE
          T_NVAL=DIMS(1)
        ENDIF

*  check axis values and width
        IF (STATUS.EQ.SAI__OK) THEN
          CALL BDI_AXCHK( FID, 1, 'Data', AOK, STATUS )
          T_REG = .FALSE.
          IF (.NOT.AOK) THEN
            CALL MSG_PRNT('AST_ERR: no axis values present')
            STATUS=SAI__ERROR
          ENDIF
          CALL BDI_AXCHK( FID, 1, 'Width', WOK, STATUS )
          T_UNIF = .FALSE.
        ENDIF

*  check variance
        CALL BDI_CHK( FID, 'Variance', T_VOK, STATUS )

*  check QUALITY
        CALL BDI_CHK( FID, 'Quality', T_QOK, STATUS )

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'TIM_CHECK',STATUS)
        ENDIF

      ENDIF


      END


      SUBROUTINE  TIM_MAP(FID,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'ADI_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
      INTEGER			FID
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER NVAL,TIMID
      LOGICAL OK
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  make dynamic copies

*  data
        CALL DYN_MAPR(1,T_NVAL,T_DPTR,STATUS)
        CALL BDI_GET1R( FID, 'Data', T_NVAL, %VAL(T_DPTR), NVAL,
     :                  STATUS )
        CALL BDI_GET0C( FID, 'Title', T_TITLE,STATUS)
        CALL BDI_GET0C( FID ,'Label',T_LABEL,STATUS)
        CALL BDI_GET0C( FID,'Units',T_UNITS,STATUS)

*  axis values
        CALL DYN_MAPR(1,T_NVAL,T_APTR,STATUS)
        CALL BDI_AXGET1R( FID, 1, 'Data', T_NVAL, %VAL(T_APTR),
     :                    NVAL, STATUS )
        CALL ARR_CHKREG( %VAL(T_APTR), T_NVAL, T_REG, T_ABASE, T_ASCALE,
     :                   STATUS )

*  axis widths
        CALL DYN_MAPR(1,T_NVAL,T_WPTR,STATUS)
        CALL BDI_AXGET1R( FID, 1, 'WIDTH', T_NVAL, %VAL(T_WPTR),
     :                    NVAL, STATUS )

*  axis ancilliaries
        CALL BDI_AXGET0L(FID,1,'Normalised',T_NORM,STATUS)
        CALL BDI_AXGET0C(FID,1,'Label',T_ALABEL,STATUS)
        CALL BDI_AXGET0C(FID,1,'Units',T_AUNITS,STATUS)
*  variance
        CALL DYN_MAPR(1,T_NVAL,T_VPTR,STATUS)
        IF (.NOT.T_VOK) THEN
          CALL ARR_COP1R(T_NVAL,%VAL(T_DPTR),%VAL(T_VPTR),STATUS)
        ELSE
          CALL BDI_GET1R( FID, 'Variance', T_NVAL, %VAL(T_VPTR),
     :                    NVAL, STATUS )
        ENDIF
*  QUALITY
        CALL DYN_MAPB(1,T_NVAL,T_QPTR,STATUS)
        IF ( T_QOK ) THEN
          CALL BDI_GET1UB( FID, 'Quality', T_NVAL,
     :                     %VAL(T_QPTR), NVAL, STATUS )
          CALL BDI_GET0UB( FID, 'QualityMask', T_MASK, STATUS )
        ELSE
          CALL ARR_INIT1B(QUAL__GOOD,T_NVAL,%VAL(T_QPTR),STATUS)
          T_MASK=QUAL__MASK
        ENDIF

*  Header
        CALL TCI_GETID( FID, TIMID, STATUS )
        IF ( (TIMID.NE.ADI__NULLID) .AND. (STATUS .EQ.SAI__OK) ) THEN
          CALL ADI_THERE( TIMID, 'MJDObs', OK, STATUS )
          IF ( OK ) THEN
            CALL ADI_CGET0D( TIMID, 'MJDObs', T_BASEMJD, STATUS )
          ELSE
            CALL MSG_PRNT(
     :       '*** BASE_MJD not present - using 0 ***')
            T_BASEMJD = 0.0D0
          ENDIF
        ELSE
          CALL MSG_PRNT(
     :      '*** no Header information - setting Base MJD to 0.0 ***')
          T_BASEMJD=0.0D0
        END IF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'TIM_MAP',STATUS)
        ENDIF

      ENDIF


      END


      SUBROUTINE  TIM_CHOP(Q,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
      BYTE Q(*)
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
      BYTE BIT_ORUB,BIT_ANDUB
*    Local constants :
*    Local variables :
      BYTE MASK
      INTEGER IVAL
      INTEGER ISECT
*-
      T_NSECT=0
      MASK=BIT_ORUB(QUAL__MISSING,QUAL__IGNORE)


      IF (STATUS.EQ.SAI__OK) THEN

*  no QUALITY so only one section
        IF (.NOT.T_QOK) THEN

          T_NSECT=1
          T_SECTPIX(1,T_NSECT)=1
          T_SECTPIX(2,T_NSECT)=T_NVAL

        ELSE

          T_NSECT=0
          IVAL=1
          DO WHILE (IVAL.LT.T_NVAL)

*  find good (not missing) data
            DO WHILE (BIT_ANDUB(Q(IVAL),MASK).NE.QUAL__GOOD.AND.
     :                                            IVAL.LT.T_NVAL)
              IVAL=IVAL+1
            ENDDO
            IF (IVAL.LT.T_NVAL) THEN
              T_NSECT=T_NSECT+1
              T_SECTPIX(1,T_NSECT)=IVAL
            ENDIF
*  find end of good data
            DO WHILE (BIT_ANDUB(Q(IVAL),MASK).EQ.QUAL__GOOD.AND.
     :                                            IVAL.LT.T_NVAL)
              IVAL=IVAL+1
            ENDDO
            IF (IVAL.LT.T_NVAL) THEN
              T_SECTPIX(2,T_NSECT)=IVAL-1
            ELSE
              IF (BIT_ANDUB(Q(IVAL),MASK).NE.QUAL__GOOD) THEN
                T_SECTPIX(2,T_NSECT)=IVAL-1
              ELSE
                T_SECTPIX(2,T_NSECT)=IVAL
              ENDIF
            ENDIF
          ENDDO

        ENDIF

*  default to all sections selected
        CALL ARR_INIT1L(.FALSE.,T_MAXSECT,T_SEL,STATUS)
        T_NSEL=T_NSECT
        DO ISECT=1,T_NSECT
          T_SEL(ISECT)=.TRUE.
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('TIM_CHOP',STATUS)
        ENDIF

      ENDIF


      END


      SUBROUTINE  TIM_NOCHOP(STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ISECT
*-
      T_NSECT=0


      IF (STATUS.EQ.SAI__OK) THEN

*  treat whole series as one section
        T_NSECT=1
        T_SECTPIX(1,T_NSECT)=1
        T_SECTPIX(2,T_NSECT)=T_NVAL

*  default to all sections selected
        CALL ARR_INIT1L(.FALSE.,T_MAXSECT,T_SEL,STATUS)
        T_NSEL=T_NSECT
        DO ISECT=1,T_NSECT
          T_SEL(ISECT)=.TRUE.
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT( 'TIM_NOCHOP',STATUS)
        ENDIF

      ENDIF


      END


*+ TIM_SCALE - scales x-axes of each segment
      SUBROUTINE  TIM_SCALE(A,W,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
      REAL A(T_NVAL)
      REAL W(T_NVAL)
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      REAL EDGE,GAP
      PARAMETER (EDGE=0.05,GAP=0.02)
*    Local variables :
      REAL XLO,XHI
      REAL RANGE,RANGESUM
      REAL GAPSUM
      INTEGER ISECT
      INTEGER LASTSEL
      LOGICAL FIRST
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get data range
        CALL TIM_DRANGE(%VAL(T_DPTR),%VAL(T_VPTR),%VAL(T_QPTR),STATUS)

*  add ranges of each selected segment
        RANGESUM=0.0
        DO ISECT=1,T_NSECT
          IF (T_SEL(ISECT)) THEN
            XLO=A(T_SECTPIX(1,ISECT))-W(T_SECTPIX(1,ISECT))/2.0
            XHI=A(T_SECTPIX(2,ISECT))+W(T_SECTPIX(2,ISECT))/2.0
            T_SECTXAX(1,ISECT)=XLO
            T_SECTXAX(2,ISECT)=XHI
            RANGESUM=RANGESUM+(XHI-XLO)
          ENDIF
        ENDDO

*  add spaces for gaps in graph
        GAPSUM=EDGE+T_NSEL*GAP

*  get overall scaling factor
        T_SCALE=(1.0-GAPSUM)/RANGESUM

*  set NDC positions for each selected segment
        FIRST=.TRUE.
        DO ISECT=1,T_NSECT
          IF (T_SEL(ISECT)) THEN
            RANGE=T_SECTXAX(2,ISECT)-T_SECTXAX(1,ISECT)
            IF (FIRST) THEN
              T_SECTNDC(1,ISECT)=EDGE
              T_SECTNDC(2,ISECT)=EDGE+RANGE*T_SCALE
              FIRST=.FALSE.
            ELSE
              T_SECTNDC(1,ISECT)=T_SECTNDC(2,LASTSEL)+GAP
              T_SECTNDC(2,ISECT)=T_SECTNDC(1,ISECT)+RANGE*T_SCALE
            ENDIF
            LASTSEL=ISECT
          ENDIF
        ENDDO

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('TIM_SCALE',STATUS)
        ENDIF

      ENDIF


      END



*+  TIM_PLOT - plot t/s segments
      SUBROUTINE  TIM_PLOT(STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER ISECT
      LOGICAL POLY,STEP,ERRS,POINTS
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  get plotting style
        CALL GFX_1DSTYLE(POLY,STEP,ERRS,POINTS,STATUS)
        IF (.NOT.(POLY.OR.STEP.OR.ERRS.OR.POINTS)) THEN
          ERRS=.TRUE.
        ENDIF

        DO ISECT=1,T_NSECT

*  plot each selected segment
          IF (T_SEL(ISECT)) THEN
*  set view port and window for this segment
            CALL TIM_SETTR(ISECT,STATUS)

*  plot axes
            CALL GCB_SETC('XAXIS_OPT','BNTS',STATUS)
            CALL GCB_SETC('YAXIS_OPT','BNTS',STATUS)
            CALL GFX_AXES(STATUS)
            CALL GCB_CAN('XAXIS_OPT,YAXIS_OPT',STATUS)

            IF (POLY) THEN
              CALL GFX_POLYQ(
     :                T_NVAL,T_SECTPIX(1,ISECT),T_SECTPIX(2,ISECT),
     :                %VAL(T_APTR),%VAL(T_DPTR),%VAL(T_QPTR),T_MASK,
     :                                                        STATUS)
            ENDIF
            IF (STEP) THEN
              CALL GFX_STEPQ(
     :                T_NVAL,T_SECTPIX(1,ISECT),T_SECTPIX(2,ISECT),
     :                %VAL(T_APTR),%VAL(T_WPTR),%VAL(T_DPTR),
     :                                       %VAL(T_QPTR),T_MASK,
     :                                                     STATUS)
            ENDIF
            IF (ERRS) THEN
              CALL GFX_ERRQ(
     :                T_NVAL,T_SECTPIX(1,ISECT),T_SECTPIX(2,ISECT),
     :                %VAL(T_APTR),%VAL(T_WPTR),%VAL(T_DPTR),
     :                           %VAL(T_VPTR),%VAL(T_QPTR),T_MASK,
     :                                                      STATUS)
            ENDIF
            IF (POINTS) THEN
              CALL GFX_POINTQ(
     :                T_NVAL,T_SECTPIX(1,ISECT),T_SECTPIX(2,ISECT),
     :                %VAL(T_APTR),%VAL(T_DPTR),%VAL(T_QPTR),T_MASK,
     :                                                       STATUS)
            ENDIF
          ENDIF
        ENDDO

*  set viewport to whole display and NDC coords
        CALL TIM_SETTR(0,STATUS)

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from TIM_PLOT',STATUS)
        ENDIF

      ENDIF


      END

*+  TIM_SETTR - set transformations
      SUBROUTINE  TIM_SETTR(ISECT,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
      INTEGER ISECT
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      REAL BOT,TOP
      PARAMETER (BOT=0.1,TOP=0.98)
*    Local variables :
      REAL X1,X2,Y1,Y2
*-

      IF (STATUS.EQ.SAI__OK) THEN


*  specific sector
        IF (ISECT.GT.0) THEN
          X1=T_SECTNDC(1,ISECT)
          X2=T_SECTNDC(2,ISECT)
          Y1=BOT
          Y2=TOP

          CALL PGSVP(X1,X2,Y1,Y2)
          X1=T_SECTXAX(1,ISECT)
          X2=T_SECTXAX(2,ISECT)
          Y1=T_YMIN
          Y2=T_YMAX
          CALL PGSWIN(X1,X2,Y1,Y2)

        ELSE
*  whole display and NDC coords
          X1=0.0
          X2=1.0
          Y1=0.0
          Y2=1.0
          CALL PGSVP(X1,X2,Y1,Y2)
          CALL PGSWIN(X1,X2,Y1,Y2)
        ENDIF

      ENDIF


      END



*+  TIM_DRANGE - get data range
      SUBROUTINE  TIM_DRANGE(D,V,Q,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
      REAL D(T_NVAL)
      REAL V(T_NVAL)
      BYTE Q(T_NVAL)
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER IVAL
*-

      IF (STATUS.EQ.SAI__OK) THEN

        T_YMIN=1.0E30
        T_YMAX=-1.0E30

        IF (T_QOK) THEN

          DO IVAL=1,T_NVAL
            IF (Q(IVAL).EQ.QUAL__GOOD) THEN
              T_YMIN=MIN(T_YMIN,D(IVAL)-SQRT(V(IVAL)))
              T_YMAX=MAX(T_YMAX,D(IVAL)+SQRT(V(IVAL)))
            ENDIF
          ENDDO

        ELSE

          DO IVAL=1,T_NVAL
            T_YMIN=MIN(T_YMIN,D(IVAL)-SQRT(V(IVAL)))
            T_YMAX=MAX(T_YMAX,D(IVAL)+SQRT(V(IVAL)))
          ENDDO

        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from TIM_DRANGE',STATUS)
        ENDIF

      ENDIF


      END



*+  TIM_CURSTOSECT
      SUBROUTINE  TIM_CURSTOSECT(XC,SECT,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
      REAL XC
*    Import-Export :
*    Export :
      INTEGER SECT
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER SEL(T_MAXSECT),ISEL,NSEL
      INTEGER ISECT
      LOGICAL FOUND
*-

      IF (STATUS.EQ.SAI__OK) THEN

        SECT=0

*  get list of currently selected segments
        NSEL=0
        DO ISECT=1,T_NSECT
          IF (T_SEL(ISECT)) THEN
            NSEL=NSEL+1
            SEL(NSEL)=ISECT
          ENDIF
        ENDDO

*  locate new segment
        FOUND=.FALSE.
        ISEL=0
        DO WHILE (.NOT.FOUND.AND.ISEL.LT.NSEL)
          ISEL=ISEL+1
          IF (ISEL.LT.NSEL) THEN
            FOUND=(XC.GE.T_SECTNDC(1,SEL(ISEL)).AND.
     :             XC.LT.T_SECTNDC(1,SEL(ISEL+1)))
          ELSE
            FOUND=(XC.GE.T_SECTNDC(1,SEL(ISEL)))
          ENDIF
        ENDDO
        IF (FOUND) THEN
          SECT=SEL(ISEL)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from TIM_CURSTOSECT',STATUS)
        ENDIF

      ENDIF


      END




      SUBROUTINE  TIM_CURSTOX(XC,X,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
      REAL XC
*    Import-Export :
*    Export :
      REAL X
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      INTEGER SECT
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL TIM_CURSTOSECT(XC,SECT,STATUS)
        X=T_SECTXAX(1,SECT)+(XC-T_SECTNDC(1,SECT))/T_SCALE

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from TIM_CURSTOX',STATUS)
        ENDIF

      ENDIF


      END



      SUBROUTINE TIM_SAVEALL(FID,STATUS)

      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*  Import :
      INTEGER			FID
*  Export :
*  Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'TIM_CMN'
*  Local constants :
*  Local variables :
      REAL	SPARR(2)
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create interface object
      CALL BDI_LINK( 'TimeSeries', 1, T_NVAL, 'REAL', FID, STATUS )

*  text
      CALL BDI_PUT0C( FID, 'Title', T_TITLE,STATUS)
      CALL BDI_PUT0C( FID, 'Label', T_LABEL,STATUS)
      CALL BDI_PUT0C( FID, 'Units', T_UNITS,STATUS)

*  Write data
      CALL BDI_PUT1R( FID, 'Data', T_NVAL, %VAL(T_DPTR), STATUS )

*  Write variance
      IF (T_VOK) THEN
        CALL BDI_PUT1R( FID, 'Variance', T_NVAL, %VAL(T_VPTR), STATUS )
      END IF

*  quality
      IF (T_QOK) THEN
        CALL BDI_PUT1UB( FID, 'Quality', T_NVAL, %VAL(T_QPTR), STATUS )
        CALL BDI_PUT0UB( FID, 'QualityMask', T_MASK, STATUS )
      ENDIF

*  axis values
      IF (T_REG) THEN
        SPARR(1) = T_ABASE
        SPARR(2) = T_ASCALE
        CALL BDI_AXPUT1R( FID, 1, 'SpacedData', 2, SPARR, STATUS )
      ELSE
        CALL BDI_AXPUT1R( FID, 1, 'Data', T_NVAL, %VAL(T_APTR), STATUS )
      END IF

*  axis widths
      IF (T_UNIF) THEN
        CALL BDI_AXPUT0R( FID, 1, 'ScalarWidth', T_AWIDTH, STATUS )
      ELSE
        CALL BDI_AXPUT1R( FID, 1, 'Width', T_NVAL, %VAL(T_WPTR),
     :                    STATUS )
      ENDIF

*  Axis ancilliary stuff
      CALL BDI_AXPUT0L(FID,1,'Normalised', T_NORM,STATUS)
      CALL BDI_AXPUT0C(FID,1,'Label',T_ALABEL,STATUS)
      CALL BDI_AXPUT0C(FID,1,'Units',T_AUNITS,STATUS)

*  Copy ancilliary stuff from input
c      CALL UDI_COPANC(T_FID,'grf',FID,STATUS)

      IF (STATUS.NE.SAI__OK) THEN
        CALL AST_REXIT('TIM_SAVEALL',STATUS)
      ENDIF

      END



      SUBROUTINE TIM_SAVESEG(ISEG,FID,STATUS)

      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
*  Import :
      INTEGER ISEG,FID
*  Export :
*  Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'TIM_CMN'
*  Local constants :
*  Local variables :
      REAL	SPARR(2)
      INTEGER DPTR,VPTR,QPTR,APTR,WPTR
      INTEGER NVAL
      INTEGER I1,I2
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

      I1 = T_SECTPIX(1,ISEG)
      I2 = T_SECTPIX(2,ISEG)
      NVAL = I2-I1+1

*  Create interface object
      CALL BDI_LINK( 'TimeSeries', 1, NVAL, 'REAL', FID, STATUS )

*  Text
      CALL BDI_PUT0C( FID, 'Title', T_TITLE,STATUS)
      CALL BDI_PUT0C( FID, 'Label', T_LABEL,STATUS)
      CALL BDI_PUT0C( FID, 'Units', T_UNITS,STATUS)

*  create data array
      CALL BDI_MAPR( FID, 'Data', 'WRITE', DPTR, STATUS )

*  variance
      IF (T_VOK) THEN
        CALL BDI_MAPR( FID, 'Variance', 'WRITE', VPTR, STATUS )
      ENDIF

*  quality
      IF (T_QOK) THEN
        CALL BDI_MAPUB( FID, 'Quality', 'WRITE', QPTR, STATUS )
        CALL BDI_PUT0UB( FID, 'QualityMask', T_MASK, STATUS )
      ENDIF

*  axis values
      IF ( T_REG ) THEN
        CALL ARR_ELEM1R(T_APTR,T_NVAL,I1,SPARR(1),STATUS)
        SPARR(2) = T_ASCALE
        CALL BDI_AXPUT1R( FID, 1, 'SpacedData', 2, SPARR, STATUS )
      ELSE
        CALL BDI_AXMAPR( FID, 1, 'Data', 'WRITE', APTR, STATUS )
      ENDIF

*  axis widths
      IF (T_UNIF) THEN
        CALL BDI_AXPUT0R( FID, 1, 'ScalarWidth', T_AWIDTH, STATUS )
      ELSE
        CALL BDI_AXMAPR( FID, 1, 'Width', 'WRITE', WPTR, STATUS )
      ENDIF

*  Axis ancilliary stuff
      CALL BDI_AXPUT0L(FID,1,'Normalised', T_NORM,STATUS)
      CALL BDI_AXPUT0C(FID,1,'Label',T_ALABEL,STATUS)
      CALL BDI_AXPUT0C(FID,1,'Units',T_AUNITS,STATUS)

*  Copy ancilliary stuff from input
      CALL UDI_COPANC(T_FID,'grf',FID,STATUS)

      CALL TIM_SAVESEG_COP(I1,NVAL,%VAL(T_DPTR),%VAL(T_VPTR),
     :                  %VAL(T_QPTR),%VAL(T_APTR),%VAL(T_WPTR),
     :                  %VAL(DPTR),%VAL(VPTR),%VAL(QPTR),
     :                  %VAL(APTR),%VAL(WPTR),STATUS)

      IF (STATUS.NE.SAI__OK) THEN
        CALL AST_REXIT('TIM_SAVESEG',STATUS)
      ENDIF

      END


      SUBROUTINE  TIM_SAVESEG_COP(I,N,D,V,Q,A,W,DS,VS,QS,AS,WS,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
      INTEGER I,N
      REAL D(*),V(*),A(*),W(*)
      BYTE Q(*)
*    Import-Export :
*    Export :
      REAL DS(*),VS(*),AS(*),WS(*)
      BYTE QS(*)
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL ARR_COP1R(N,D(I),DS,STATUS)
        IF (T_VOK) THEN
          CALL ARR_COP1R(N,V(I),VS,STATUS)
        ENDIF
        CALL ARR_COP1B(N,Q(I),QS,STATUS)
        IF (.NOT.T_REG) THEN
          CALL ARR_COP1R(N,A(I),AS,STATUS)
        ENDIF
        IF (.NOT.T_UNIF) THEN
          CALL ARR_COP1R(N,W(I),WS,STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL AST_REXIT('TIM_SAVESEG_COP',STATUS)
        ENDIF

      ENDIF


      END


      SUBROUTINE  TIM_SAVEMULT(OFID,STATUS)
*    Description :
*    Authors :
*      BHVAD::RJV
*    History :
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'TIM_CMN'
*    Structure definitions :
*    Import :
      INTEGER			OFID
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
      REAL BOT,TOP
      PARAMETER (BOT=0.1,TOP=0.98)
*    Local variables :
      INTEGER ISEG,GFID,MGID,IGRAF
*-

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL ADI_NEW0( 'MultiGraph', MGID, STATUS )
      CALL ADI_SETLNK( MGID, OFID, STATUS )
      OFID = MGID

      CALL GMI_CREMULT( OFID, T_NSEL, STATUS )
      IGRAF=0
      DO ISEG=1,T_MAXSECT
        IF (T_SEL(ISEG)) THEN
          IGRAF=IGRAF+1
          CALL GMI_LOCNDF( OFID, IGRAF, '*', GFID, STATUS )
          CALL TIM_SAVESEG(ISEG,GFID,STATUS)

          CALL GCB_SETR('POS_X1',T_SECTNDC(1,ISEG),STATUS)
          CALL GCB_SETR('POS_X2',T_SECTNDC(2,ISEG),STATUS)
          CALL GCB_SETR('POS_Y1',BOT,STATUS)
          CALL GCB_SETR('POS_Y2',TOP,STATUS)
          CALL GCB_SETR('XAXIS_LO',T_SECTXAX(1,ISEG),STATUS)
          CALL GCB_SETR('XAXIS_HI',T_SECTXAX(2,ISEG),STATUS)
          CALL GCB_SETR('YAXIS_LO',T_YMIN,STATUS)
          CALL GCB_SETR('YAXIS_HI',T_YMAX,STATUS)
          CALL GCB_SETC('XAXIS_OPT','BNTS',STATUS)
          CALL GCB_SETC('YAXIS_OPT','BNTS',STATUS)
          IF (IGRAF.GT.1) THEN
            CALL GCB_SETC('YLABEL_TEXT',' ',STATUS)
          ENDIF

          CALL GCB_FSAVE(GFID,STATUS)

          CALL GCB_CAN('POS_X1,POS_X2',STATUS)
          CALL GCB_CAN('XAXIS_LO,XAXIS_HI,YAXIS_LO,YAXIS_HI',STATUS)
          CALL GCB_CAN('XAXIS_OPT,YAXIS_OPT',STATUS)

          CALL ADI_ERASE(GFID,STATUS)

        ENDIF
      END DO

      IF (STATUS.NE.SAI__OK) THEN
        CALL AST_REXIT('TIM_SAVEMULT',STATUS)
      END IF

      END
