*+  IZED - bin in z-direction
      SUBROUTINE IZED(STATUS)
*    Description :
*    Deficiencies :
*    Bugs :
*    Authors :
*     BHVAD::RJV
*    History :
*      26 Jan 93 : V1.7-0 GCB, GFX used (RJV)
*       1 Jul 93 : V1.7-1 GTR used (RJV)
*      14 Jun 94 : V1.7-2 bug in QUALITY checking (RJV)
*    Type definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Status :
      INTEGER STATUS
*    Function declarations :
*    Local constants :
*    Local variables :
      CHARACTER*1 CH
      REAL XC,YC
      REAL XR,YR,RAD
      REAL BASE,SCALE
      INTEGER ZPTR
      INTEGER DPTR,VPTR,QPTR
      LOGICAL	REG
*    Version :
      CHARACTER*30 VERSION
      PARAMETER (VERSION = 'IZED Version 2.0-0')
*-
      CALL USI_INIT()

      CALL MSG_PRNT(VERSION)

      IF (.NOT.I_OPEN) THEN
        CALL MSG_PRNT('AST_ERR: image processing system not active')
      ELSEIF (.NOT.I_CUBE) THEN
        CALL MSG_PRNT('AST_ERR: current data not a cube')
      ELSEIF (.NOT.I_DISP) THEN
        CALL MSG_PRNT('AST_ERR: no image currently displayed')
      ELSE

*  ensure transformations correct
        CALL GTR_RESTORE(STATUS)
        CALL GCB_ATTACH('IMAGE',STATUS)
        CALL IMG_2DGCB(STATUS)


*  cursor mode
        IF (I_MODE.EQ.1) THEN
*  get centre
          CALL MSG_PRNT(' ')
          CALL MSG_PRNT('Select centre...')
          XC=I_X
          YC=I_Y
          CALL PGCURSE(XC,YC,CH)
          CALL PGPOINT(1,XC,YC,2)

*  get radius
          CALL MSG_PRNT('Select radius...')
          XR=XC
          YR=YC
          CALL PGCURSE(XR,YR,CH)
          RAD=SQRT((XR-XC)**2 + (YR-YC)**2)

*  keyboard mode
        ELSE
          CALL USI_DEF0R('X',I_X,STATUS)
          CALL USI_GET0R('X',XC,STATUS)
          CALL USI_DEF0R('Y',I_Y,STATUS)
          CALL USI_GET0R('Y',YC,STATUS)
          CALL USI_DEF0R('RAD',I_R,STATUS)
          CALL USI_GET0R('RAD',RAD,STATUS)

        ENDIF

*  plot  circle
        CALL IMG_CIRCLE(XC,YC,RAD,STATUS)

*  create and map 1D data array
        I_N_1D=I_IZ2-I_IZ1+1
        CALL IMG_GET1D(I_N_1D,STATUS)

*  remap cube
        CALL BDI_MAPR(I_FID,'Data','READ',DPTR,STATUS)
        IF (I_VOK) THEN
          CALL BDI_MAPR(I_FID,'Variance','READ',VPTR,STATUS)
        ENDIF
        IF (I_QOK) THEN
          CALL BDI_MAP(I_FID,'Quality','UBYTE','READ',QPTR,STATUS)
        ENDIF

*  do it
        CALL IZED_DOIT(%VAL(DPTR),%VAL(VPTR),%VAL(QPTR),XC,YC,RAD,
     :                 %VAL(I_DPTR_1D),%VAL(I_VPTR_1D),%VAL(I_QPTR_1D),
     :                                                          STATUS)


*  axis and labels
        CALL BDI_AXMAPR( I_FID, I_ZAX, 'Data', 'READ', ZPTR, STATUS )
        CALL ARR_CHKREG( %VAL(ZPTR), I_N_1D, REG, BASE, SCALE,
     :                   STATUS )
        CALL BDI_AXGET0C(I_FID,I_ZAX,'Label',I_XLABEL_1D,STATUS)
        IF ( REG ) THEN
          I_XBASE_1D=BASE+REAL(I_IZ1-1)*SCALE
          I_XSCALE_1D=SCALE
          I_XWID_1D=ABS(SCALE)
          CALL BDI_AXGET0C(I_FID,I_ZAX,'Units',I_XUNITS_1D,STATUS)
        ELSE
          CALL MSG_PRNT( 'WARNING : Input cube has non-regular axis' )
          I_XBASE_1D = REAL(I_IZ1)
          I_XSCALE_1D = 1.0
          I_XWID_1D = 1.0
          I_XUNITS_1D = 'bin number'
        END IF
        I_LABEL_1D = I_LABEL
        I_UNITS_1D = I_UNITS
        I_TITLE_1D = ' '
        CALL ARR_REG1R(I_XBASE_1D,I_XSCALE_1D,I_N_1D,%VAL(I_APTR_1D),
     :                                                          STATUS)
        CALL ARR_INIT1R(ABS(I_XSCALE_1D),I_N_1D,%VAL(I_WPTR_1D),STATUS)

*  unmap cube
        CALL BDI_UNMAP( I_FID, 'Data', DPTR, STATUS )
        IF ( I_VOK ) THEN
          CALL BDI_UNMAP( I_FID, 'Variance', VPTR, STATUS )
        END IF
        IF ( I_QOK ) THEN
          CALL BDI_UNMAP( I_FID, 'Quality', QPTR, STATUS )
        END IF

*  set default axis ranges
        I_X1_1D=I_XBASE_1D-I_XSCALE
        I_X2_1D=I_XBASE_1D+(REAL(I_N_1D-1)+0.6)*I_XSCALE_1D
        CALL ARR_RANG1R(I_N_1D,%VAL(I_DPTR_1D),I_Y1_1D,I_Y2_1D,STATUS)
        I_Y2_1D=I_Y2_1D*1.1

*  reset auxiliary plot
        I_N_AUX=0

*  plot it
        CALL IMG_1DGCB(STATUS)
        CALL GCB_SETL('ERR_FLAG',.TRUE.,STATUS)
        CALL GCB_SETL('STEP_FLAG',.FALSE.,STATUS)
        CALL GCB_SETL('POLY_FLAG',.FALSE.,STATUS)
        CALL GCB_SETL('POINT_FLAG',.FALSE.,STATUS)
        CALL GDV_CLEAR(STATUS)
        CALL IMG_PLOT(STATUS)

        CALL GCB_CACHE(I_CACHE_1D,STATUS)

*  flag current plotting status
        I_DISP_1D=.TRUE.
        I_DISP=.FALSE.
        I_CLEAR=.FALSE.

        CALL IMG_SETPOS(XC,YC,STATUS)

      ENDIF

      CALL USI_CLOSE()

      END





      SUBROUTINE IZED_DOIT(D,V,Q,XC,YC,R,D1,V1,Q1,STATUS)

*    Description :
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'QUAL_PAR'
*    Global variables :
      INCLUDE 'IMG_CMN'
*    Import :
      REAL D(I_NX,I_NY,I_NZ),V(I_NX,I_NY,I_NZ)
      BYTE Q(I_NX,I_NY,I_NZ)
      REAL XC,YC,R
*    Import-Export :
*    Export :
      REAL D1(I_N_1D),V1(I_N_1D)
      BYTE Q1(I_N_1D)
*    Status :
      INTEGER STATUS
*    Functions :
      LOGICAL IMG_INCIRC
      BYTE BIT_ANDUB,BIT_ORUB
*    Local variables :
      REAL VAL,VAR
      INTEGER IX,IY,IZ,JZ
      INTEGER I,J,K
      INTEGER I1,I2,J1,J2
      INTEGER NGOOD
      BYTE QUAL
      LOGICAL GOOD
*-
      IF (STATUS.NE.SAI__OK) RETURN

      JZ=0
      DO IZ=I_IZ1,I_IZ2

        JZ=JZ+1

        VAL=0.0
        VAR=0.0
        QUAL=QUAL__GOOD
        NGOOD=0

* Loop over all pixels within rectangle enclosing circle
        CALL IMG_CIRCTOBOX(XC,YC,R,I1,I2,J1,J2,STATUS)
        DO IY=J1,J2
          DO IX=I1,I2

            IF (I_ZAX.EQ.3) THEN
              I=IX
              J=IY
              K=IZ
            ELSE
              I=IZ
              J=IX
              K=IY
            ENDIF

* check pixel is within circle
            IF (IMG_INCIRC(IX,IY,XC,YC,R)) THEN

*   Test pixel quality
              IF (I_QOK) THEN
                GOOD=(BIT_ANDUB(Q(I,J,K),I_MASK).EQ.QUAL__GOOD)
                QUAL=BIT_ORUB(QUAL,Q(I,J,K))
              ELSE
                GOOD=.TRUE.
              ENDIF

              IF (GOOD) THEN

                NGOOD=NGOOD+1

                VAL=VAL+D(I,J,K)
                IF (I_VOK) THEN
                  VAR=VAR+V(I,J,K)
                ELSE
                  VAR=VAR+D(I,J,K)
                ENDIF

              ENDIF

            ENDIF

          ENDDO
        ENDDO

        IF (NGOOD.GT.0) THEN
          Q1(JZ)=QUAL__GOOD
        ELSE
          Q1(JZ)=QUAL
        ENDIF
        D1(JZ)=VAL
        V1(JZ)=VAR

      ENDDO


      END
