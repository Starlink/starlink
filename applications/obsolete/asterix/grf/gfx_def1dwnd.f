*+  GXF_DEF1DWND - get default 1D axis ranges
      SUBROUTINE GFX_DEF1DWND(NVAL,X,Y,XW1,XW2,YW1,YW2,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*      16 Mar 94 : changes to improve performance for log axes (RJV)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER NVAL			! number of values
      REAL X(*),Y(*)
*    Import-export :
*    Export :
      REAL XW1,XW2,YW1,YW2		! plot window in world coords
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      INTEGER I
      LOGICAL   MONOTONIC
      LOGICAL XLOG,YLOG,OK
      REAL XGAP,YGAP
      REAL LXW1,LXW2,LYW1,LYW2
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  see if log axes flagged
        CALL GCB_GETL('XAXIS_LOG',OK,XLOG,STATUS)
        IF (.NOT.OK) THEN
          XLOG=.FALSE.
        ENDIF
        CALL GCB_GETL('YAXIS_LOG',OK,YLOG,STATUS)
        IF (.NOT.OK) THEN
          YLOG=.FALSE.
        ENDIF

*  X values monotonic?
        CALL ARR_CHKMONOT( NVAL, X, MONOTONIC, STATUS )

*  if so, normal axis so take extrema
        IF ( MONOTONIC ) THEN
          XW1=X(1)
          XW2=X(NVAL)

*  check and adjust for log axes
          IF (XLOG) THEN
            IF (XW2.LE.VAL__SMLR) THEN
              CALL MSG_PRNT(
     :          'AST_ERR: x-axis values too small for log axis')
              STATUS=SAI__ERROR
            ELSE
              I=1
              DO WHILE (X(I).LT.VAL__SMLR)
                I=I+1
              ENDDO
              XW1=X(I)
            ENDIF

          ENDIF

*  otherwise probably a scatter or phase plot
        ELSE

*  if log axis then find range > VAL__SMLR
          IF (XLOG) THEN
            XW1=X(1)
            XW2=VAL__SMLR
            DO I=1,NVAL
              IF (X(I).GT.VAL__SMLR) THEN
                XW1=MIN(XW1,X(I))
                XW2=MAX(XW2,X(I))
              ENDIF
            ENDDO
            IF (XW2.LE.VAL__SMLR) THEN
              CALL MSG_PRNT(
     :          'AST_ERR: x-axis values too small for log axis')
              STATUS=SAI__ERROR
            ENDIF
          ELSE
*        get min and max x values
            CALL ARR_RANG1R(NVAL,X,XW1,XW2,STATUS)
          ENDIF

        ENDIF

*  get range of y
        IF (YLOG) THEN
          YW1=VAL__MAXR
          YW2=VAL__SMLR
          DO I=1,NVAL
            IF (Y(I).GT.VAL__SMLR) THEN
              YW1=MIN(YW1,Y(I))
              YW2=MAX(YW2,Y(I))
            ENDIF
          ENDDO
          IF (YW2.LE.VAL__SMLR) THEN
            CALL MSG_PRNT(
     :          'AST_ERR: y-axis values too small for log axis')
            STATUS=SAI__ERROR
          ENDIF
        ELSE
*        get min and max x values
          CALL ARR_RANG1R(NVAL,Y,YW1,YW2,STATUS)
        ENDIF


        IF (STATUS.EQ.SAI__OK) THEN
*  add a bit either end
          IF (NVAL.EQ.1) THEN
            XW1=XW1-0.5*X(1)
            XW2=XW2+0.5*X(1)
            YW1=YW1-0.5*Y(1)
            YW2=YW2+0.5*Y(1)

          ELSE

            IF (XLOG) THEN
              LXW1=LOG10(XW1)
              LXW2=LOG10(XW2)
              XGAP=ABS((LXW2-LXW1)/30.0)
              XW1=10.0**(LXW1-XGAP)
              XW2=10.0**(LXW2+XGAP)
            ELSE
              XGAP=(XW2-XW1)/30.0
              XW1=XW1-XGAP
              XW2=XW2+XGAP

            ENDIF

            IF (YLOG) THEN
              LYW1=LOG10(YW1)
              LYW2=LOG10(YW2)
              YGAP=ABS((LYW2-LYW1)/30.0)
              YW1=10.0**(LYW1-YGAP)
              YW2=10.0**(LYW2+YGAP)
            ELSE
              YGAP=(YW2-YW1)/30.0
              YW1=YW1-YGAP
              YW2=YW2+YGAP
            ENDIF

          ENDIF

        ELSE
          CALL ERR_REP(' ','from GFX_DEF1DWND',STATUS)
        ENDIF

      ENDIF

      END

*+  GXF_DEF1DWNDQ - get default 1D axis ranges ignoring bad quality
      SUBROUTINE GFX_DEF1DWNDQ(NVAL,X,Y,Q,MASK,XW1,XW2,YW1,YW2,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER NVAL			! number of values
      REAL X(*),Y(*)
      BYTE Q(*),MASK
*    Import-export :
*    Export :
      REAL XW1,XW2,YW1,YW2		! plot window in world coords
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local Constants :
*    Local variables :
      INTEGER I,J,IJ
      LOGICAL MONOTONIC
      LOGICAL XLOG,YLOG,OK
      REAL XGAP,YGAP
      REAL LXW1,LXW2,LYW1,LYW2
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  see if log axes flagged
        CALL GCB_GETL('XAXIS_LOG',OK,XLOG,STATUS)
        IF (.NOT.OK) THEN
          XLOG=.FALSE.
        ENDIF
        CALL GCB_GETL('YAXIS_LOG',OK,YLOG,STATUS)
        IF (.NOT.OK) THEN
          YLOG=.FALSE.
        ENDIF


*  find first and last good point
        I=1
        DO WHILE (BIT_ANDUB(Q(I),MASK).NE.QUAL__GOOD.AND.I.LT.NVAL)
          I=I+1
        ENDDO
        J=NVAL
        DO WHILE (BIT_ANDUB(Q(J),MASK).NE.QUAL__GOOD.AND.J.GT.1)
          J=J-1
        ENDDO

*      X values monotonic?
        CALL ARR_CHKMONOTQ( NVAL, X, Q, MASK, MONOTONIC, STATUS )

        IF (MONOTONIC) THEN
*  take extrema of X axis
          XW1=X(I)
          XW2=X(J)

          IF (XLOG) THEN
            IF (XW2.LE.VAL__SMLR) THEN
              CALL MSG_PRNT(
     :            'AST_ERR: x-axis values too small for log axis')
              STATUS=SAI__ERROR
            ELSE
              IJ=I
              DO WHILE (IJ.LE.J.AND.(X(IJ).LT.VAL__SMLR.OR.
     :                        BIT_ANDUB(Q(IJ),MASK).NE.QUAL__GOOD))
                IJ=IJ+1
              ENDDO
              XW1=X(IJ)
            ENDIF
          ENDIF

*  unless not monotonic
        ELSE
          XW1=X(I)
          XW2=VAL__SMLR
          IF (XLOG) THEN
            DO IJ=I,J
              IF (BIT_ANDUB(Q(IJ),MASK).EQ.QUAL__GOOD.AND.
     :                                  X(IJ).GT.VAL__SMLR) THEN
                XW1=MIN(XW1,X(IJ))
                XW2=MAX(XW2,X(IJ))
              ENDIF
            ENDDO
            IF (XW2.LE.VAL__SMLR) THEN
              CALL MSG_PRNT(
     :           'AST_ERR: x-axis values too small for log axis')
              STATUS=SAI__ERROR
            ENDIF
          ELSE
            DO IJ=I,J
              IF (BIT_ANDUB(Q(IJ),MASK).EQ.QUAL__GOOD) THEN
                XW1=MIN(XW1,X(IJ))
                XW2=MAX(XW2,X(IJ))
              ENDIF
            ENDDO
          ENDIF
        ENDIF

*  get range of y
        YW1=Y(I)
        YW2=VAL__SMLR
        IF (YLOG) THEN
          DO IJ=I,J
            IF (BIT_ANDUB(Q(IJ),MASK).EQ.QUAL__GOOD.AND.
     :                                  Y(IJ).GT.VAL__SMLR) THEN
              YW1=MIN(YW1,Y(IJ))
              YW2=MAX(YW2,Y(IJ))
            ENDIF
          ENDDO
          IF (YW2.LE.VAL__SMLR) THEN
            CALL MSG_PRNT(
     :           'AST_ERR: x-axis values too small for log axis')
            STATUS=SAI__ERROR
          ENDIF
        ELSE
          DO IJ=I,J
            IF (BIT_ANDUB(Q(IJ),MASK).EQ.QUAL__GOOD) THEN
              YW1=MIN(YW1,Y(IJ))
              YW2=MAX(YW2,Y(IJ))
            ENDIF
          ENDDO
        ENDIF


        IF (STATUS.EQ.SAI__OK) THEN
*  add a bit either end
          IF (NVAL.EQ.1) THEN
            XW1=XW1-0.5*X(1)
            XW2=XW2+0.5*X(1)
            YW1=YW1-0.5*Y(1)
            YW2=YW2+0.5*Y(1)

          ELSE

            IF (XLOG) THEN
              LXW1=LOG10(XW1)
              LXW2=LOG10(XW2)
              XGAP=ABS((LXW2-LXW1)/30.0)
              XW1=10.0**(LXW1-XGAP)
              XW2=10.0**(LXW2+XGAP)
            ELSE
              XGAP=(XW2-XW1)/30.0
              XW1=XW1-XGAP
              XW2=XW2+XGAP

            ENDIF

            IF (YLOG) THEN
              LYW1=LOG10(YW1)
              LYW2=LOG10(YW2)
              YGAP=ABS((LYW2-LYW1)/30.0)
              YW1=10.0**(LYW1-YGAP)
              YW2=10.0**(LYW2+YGAP)
            ELSE
              YGAP=(YW2-YW1)/30.0
              YW1=YW1-YGAP
              YW2=YW2+YGAP
            ENDIF

          ENDIF

        ELSE
          CALL ERR_REP(' ','from GFX_DEF1DWNDQ',STATUS)
        ENDIF


      ENDIF
      END

*+  GXF_DEF1DWNDV - get default 1D axis ranges taking errors into account
      SUBROUTINE GFX_DEF1DWNDV(NVAL,X,Y,V,XW1,XW2,YW1,YW2,STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER NVAL			! number of values
      REAL X(*),Y(*),V(*)
*    Import-export :
*    Export :
      REAL XW1,XW2,YW1,YW2		! plot window in world coords
*    Status :
      INTEGER STATUS
*    Functions :
*    Local Constants :
*    Local variables :
      INTEGER I
      LOGICAL MONOTONIC
      LOGICAL XLOG,YLOG,OK
      REAL E,HW1,HW2
      REAL XGAP,YGAP
      REAL LXW1,LXW2,LYW1,LYW2
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  see if log axes flagged
        CALL GCB_GETL('XAXIS_LOG',OK,XLOG,STATUS)
        IF (.NOT.OK) THEN
          XLOG=.FALSE.
        ENDIF
        CALL GCB_GETL('YAXIS_LOG',OK,YLOG,STATUS)
        IF (.NOT.OK) THEN
          YLOG=.FALSE.
        ENDIF


*  X values monotonic?
        CALL ARR_CHKMONOT( NVAL, X, MONOTONIC, STATUS )

*  if so, normal axis so take extrema
        IF ( MONOTONIC ) THEN
          XW1=X(1)
          XW2=X(NVAL)
          IF (XLOG) THEN
            IF (XW2.LE.VAL__SMLR) THEN
              CALL MSG_PRNT(
     :            'AST_ERR: x-axis values too small for log axis')
              STATUS=SAI__ERROR
            ELSE
              I=1
              DO WHILE (X(I).LT.VAL__SMLR)
                I=I+1
              ENDDO
              XW1=X(I)
            ENDIF

          ENDIF

*  otherwise probably a scatter or phase plot
        ELSE

*  if log axis then find range > VAL__SMLR
          IF (XLOG) THEN
            XW1=VAL__MAXR
            XW2=VAL__SMLR
            DO I=1,NVAL
              IF (X(I).GT.VAL__SMLR) THEN
                XW1=MIN(XW1,X(I))
                XW2=MAX(XW2,X(I))
              ENDIF
            ENDDO
            IF (XW2.LE.VAL__SMLR) THEN
              CALL MSG_PRNT(
     :          'AST_ERR: x-axis values too small for log axis')
              STATUS=SAI__ERROR
            ENDIF
          ELSE
*   get min and max x values
            CALL ARR_RANG1R(NVAL,X,XW1,XW2,STATUS)
          ENDIF

        ENDIF

*  find bin half-width and adjust bounds to allow for error bars
        IF (NVAL.EQ.1) THEN
          HW1=1.0
          HW2=1.0
        ELSE
          IF (MONOTONIC) THEN
            HW1=(X(2)-X(1))/2.0
            HW2=(X(NVAL)-X(NVAL-1))/2.0
          ELSE
            HW1=(XW2-XW1)/REAL(NVAL-1)/2.0
            HW2=HW1
          ENDIF
        ENDIF
        IF (XLOG) THEN
          IF (XW1-HW1.GT.VAL__SMLR) THEN
            XW1=XW1-HW1
          ENDIF
        ELSE
          XW1=XW1-HW1
        ENDIF
        XW2=XW2+HW2


*  get range of y
        IF (YLOG) THEN
          YW1=VAL__MAXR
          YW2=VAL__SMLR
          DO I=1,NVAL
            E=SQRT(V(I))
            IF (Y(I).GT.VAL__SMLR) THEN
              IF (Y(I)+E.GT.YW2) THEN
                YW2=Y(I)+E
              ENDIF
              IF (Y(I)-E.LT.YW1.AND.Y(I)-E.GT.VAL__SMLR) THEN
                YW1=Y(I)-E
              ELSEIF (Y(I).LT.YW1) THEN
                YW1=Y(I)
              ENDIF
            ENDIF
          ENDDO
          IF (YW2.LE.VAL__SMLR) THEN
            CALL MSG_PRNT(
     :          'AST_ERR: y-axis values too small for log axis')
            STATUS=SAI__ERROR
          ENDIF

        ELSE
          YW1=Y(1)-SQRT(V(1))
          YW2=Y(1)+SQRT(V(1))
          DO I=2,NVAL
            E=SQRT(V(I))
            IF (Y(I)+E.GT.YW2) THEN
              YW2=Y(I)+E
            ELSEIF (Y(I)-E.LT.YW1) THEN
              YW1=Y(I)-E
            ENDIF
          ENDDO
        ENDIF


        IF (STATUS.EQ.SAI__OK) THEN
*  add a bit either end
          IF (NVAL.EQ.1) THEN
            XW1=XW1-0.5*X(1)
            XW2=XW2+0.5*X(1)
            YW1=YW1-0.5*Y(1)
            YW2=YW2+0.5*Y(1)

          ELSE

            IF (XLOG) THEN
              LXW1=LOG10(XW1)
              LXW2=LOG10(XW2)
              XGAP=ABS((LXW2-LXW1)/30.0)
              XW1=10.0**(LXW1-XGAP)
              XW2=10.0**(LXW2+XGAP)
            ELSE
              XGAP=(XW2-XW1)/30.0
              XW1=XW1-XGAP
              XW2=XW2+XGAP

            ENDIF

            IF (YLOG) THEN
              LYW1=LOG10(YW1)
              LYW2=LOG10(YW2)
              YGAP=ABS((LYW2-LYW1)/30.0)
              YW1=10.0**(LYW1-YGAP)
              YW2=10.0**(LYW2+YGAP)
            ELSE
              YGAP=(YW2-YW1)/30.0
              YW1=YW1-YGAP
              YW2=YW2+YGAP
            ENDIF

          ENDIF

        ELSE
          CALL ERR_REP(' ','from GFX_DEF1DWNDV',STATUS)
        ENDIF

      ENDIF

      END


*+  GXF_DEF1DWNDVQ - get default 1D axis ranges with errors ignoring bad quality
      SUBROUTINE GFX_DEF1DWNDVQ(NVAL,X,Y,V,Q,MASK,XW1,XW2,YW1,YW2,
     :                                                      STATUS)

*    Description :
*    Parameters :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD::RJV)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'QUAL_PAR'
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER NVAL			! number of values
      REAL X(*),Y(*),V(*)
      BYTE Q(*),MASK
*    Import-export :
*    Export :
      REAL XW1,XW2,YW1,YW2		! plot window in world coords
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local Constants :
*    Local variables :
      INTEGER I,J,IJ
      LOGICAL MONOTONIC
      LOGICAL XLOG,YLOG,OK
      REAL E,HW1,HW2
      REAL XGAP,YGAP
      REAL LXW1,LXW2,LYW1,LYW2
*-

      IF (STATUS.EQ.SAI__OK) THEN

*  see if log axes flagged
        CALL GCB_GETL('XAXIS_LOG',OK,XLOG,STATUS)
        IF (.NOT.OK) THEN
          XLOG=.FALSE.
        ENDIF
        CALL GCB_GETL('YAXIS_LOG',OK,YLOG,STATUS)
        IF (.NOT.OK) THEN
          YLOG=.FALSE.
        ENDIF



*  find first and last good point
        I=1
        DO WHILE (BIT_ANDUB(Q(I),MASK).NE.QUAL__GOOD.AND.I.LT.NVAL)
          I=I+1
        ENDDO
        J=NVAL
        DO WHILE (BIT_ANDUB(Q(J),MASK).NE.QUAL__GOOD.AND.J.GT.1)
          J=J-1
        ENDDO

*      X values monotonic?
        CALL ARR_CHKMONOTQ( NVAL, X, Q, MASK, MONOTONIC, STATUS )

        IF (MONOTONIC) THEN
*  take extrema of X axis
          XW1=X(I)
          XW2=X(J)

          IF (XLOG) THEN
            IF (XW2.LE.VAL__SMLR) THEN
              CALL MSG_PRNT(
     :            'AST_ERR: x-axis values too small for log axis')
              STATUS=SAI__ERROR
            ELSE
              IJ=I
              DO WHILE (IJ.LE.J.AND.(X(IJ).LT.VAL__SMLR.OR.
     :                        BIT_ANDUB(Q(IJ),MASK).NE.QUAL__GOOD))
                IJ=IJ+1
              ENDDO
              XW1=X(IJ)
            ENDIF
          ENDIF

*  unless not monotonic
        ELSE
          XW1=X(I)
          XW2=VAL__SMLR
          IF (XLOG) THEN
            DO IJ=I,J
              IF (BIT_ANDUB(Q(IJ),MASK).EQ.QUAL__GOOD.AND.
     :                                  X(IJ).GT.VAL__SMLR) THEN
                XW1=MIN(XW1,X(IJ))
                XW2=MAX(XW2,X(IJ))
              ENDIF
            ENDDO
            IF (XW2.LE.VAL__SMLR) THEN
              CALL MSG_PRNT(
     :           'AST_ERR: x-axis values too small for log axis')
              STATUS=SAI__ERROR
            ENDIF
          ELSE
            DO IJ=I,J
              IF (BIT_ANDUB(Q(IJ),MASK).EQ.QUAL__GOOD) THEN
                XW1=MIN(XW1,X(IJ))
                XW2=MAX(XW2,X(IJ))
              ENDIF
            ENDDO
          ENDIF
        ENDIF

*  find bin half-width and adjust bounds to allow for error bars
        IF (NVAL.EQ.1) THEN
          HW1=1.0
          HW2=1.0
        ELSE
          IF (MONOTONIC) THEN
            HW1=(X(I+1)-X(I))/2.0
            HW2=(X(J)-X(J-1))/2.0
          ELSE
            HW1=(XW2-XW1)/REAL(J-I)/2.0
            HW2=HW1
          ENDIF
        ENDIF
        IF (XLOG) THEN
          IF (XW1-HW1.GT.VAL__SMLR) THEN
            XW1=XW1-HW1
          ENDIF
        ELSE
          XW1=XW1-HW1
        ENDIF
        XW2=XW2+HW2


*  get range of y
        IF (YLOG) THEN
          YW1=VAL__MAXR
          YW2=VAL__SMLR
          DO IJ=I,J
            IF (BIT_ANDUB(Q(IJ),MASK).EQ.QUAL__GOOD) THEN
              E=SQRT(V(IJ))
              IF (Y(IJ).GT.VAL__SMLR) THEN
                IF (Y(IJ)+E.GT.YW2) THEN
                  YW2=Y(IJ)+E
                ENDIF
                IF (Y(IJ)-E.LT.YW1.AND.Y(IJ)-E.GT.VAL__SMLR) THEN
                  YW1=Y(IJ)-E
                ELSEIF (Y(IJ).LT.YW1) THEN
                  YW1=Y(IJ)
                ENDIF
              ENDIF
            ENDIF
          ENDDO
          IF (YW2.LE.VAL__SMLR) THEN
            CALL MSG_PRNT(
     :          'AST_ERR: y-axis values too small for log axis')
            STATUS=SAI__ERROR
          ENDIF

        ELSE
          YW1=Y(I)-SQRT(V(I))
          YW2=Y(I)+SQRT(V(I))
          DO IJ=I,J
            IF (BIT_ANDUB(Q(IJ),MASK).EQ.QUAL__GOOD) THEN
              E=SQRT(V(IJ))
              IF (Y(IJ)+E.GT.YW2) THEN
                YW2=Y(IJ)+E
              ELSEIF (Y(IJ)-E.LT.YW1) THEN
                YW1=Y(IJ)-E
              ENDIF
            ENDIF
          ENDDO
        ENDIF


        IF (STATUS.EQ.SAI__OK) THEN
*  add a bit either end
          IF (NVAL.EQ.1) THEN
            XW1=XW1-0.5*X(1)
            XW2=XW2+0.5*X(1)
            YW1=YW1-0.5*Y(1)
            YW2=YW2+0.5*Y(1)

          ELSE

            IF (XLOG) THEN
              LXW1=LOG10(XW1)
              LXW2=LOG10(XW2)
              XGAP=ABS((LXW2-LXW1)/30.0)
              XW1=10.0**(LXW1-XGAP)
              XW2=10.0**(LXW2+XGAP)
            ELSE
              XGAP=(XW2-XW1)/30.0
              XW1=XW1-XGAP
              XW2=XW2+XGAP

            ENDIF

            IF (YLOG) THEN
              LYW1=LOG10(YW1)
              LYW2=LOG10(YW2)
              YGAP=ABS((LYW2-LYW1)/30.0)
              YW1=10.0**(LYW1-YGAP)
              YW2=10.0**(LYW2+YGAP)
            ELSE
              YGAP=(YW2-YW1)/30.0
              YW1=YW1-YGAP
              YW2=YW2+YGAP
            ENDIF

          ENDIF

        ELSE
          CALL ERR_REP(' ','from GFX_DEF1DWNDVQ',STATUS)
        ENDIF

      ENDIF
      END
