*+  GFX_PIXELQ2 - plots pixels - bad quality pixels not plotted
      SUBROUTINE GFX_PIXELQ2(NX,NY,I1,I2,J1,J2,X,Y,XW,YW,
     :                           Z,ZMIN,ZMAX,Q,MASK,STATUS)

*    Description :
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
*    Import :
      INTEGER NX,NY
      INTEGER I1,I2,J1,J2
      REAL X(*),Y(*)
      REAL XW(*),YW(*)
      REAL Z(NX,NY)
      BYTE Q(NX,NY)
      BYTE MASK
*    Import-export :
      REAL ZMIN,ZMAX
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local Constants :
*    Local variables :
      CHARACTER*10 SCALING
      REAL RVAL
      REAL ZRAN
      REAL X1,X2,Y1,Y2
      REAL XHWID,YHWID
      REAL DIRX,DIRY
      INTEGER BG,FIRST,LAST
      INTEGER NCOL
      INTEGER I,J
      INTEGER IA
      LOGICAL COLOUR
      LOGICAL OK
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL PGBBUF()

*  get scaling
        CALL GCB_GETC('PIX_SCALING',OK,SCALING,STATUS)
        IF (.NOT.OK) THEN
          SCALING='LIN'
        ELSE
          CALL CHR_UCASE(SCALING)
          SCALING=SCALING(:3)
        ENDIF
        IF (.NOT.(SCALING.EQ.'LIN'.OR.SCALING.EQ.'LOG'.OR.
     :            SCALING.EQ.'SQR'.OR.SCALING.EQ.'HIS'.OR.
     :            SCALING.EQ.'CYC'.OR.SCALING.EQ.'PAR'.OR.
     :            SCALING.EQ.'SIN')) THEN
          CALL MSG_PRNT('AST_ERR: invalid scaling type')
          STATUS=SAI__ERROR
        ENDIF

*  get min/max scaling values if set
        CALL GCB_GETR('PIX_MIN',OK,RVAL,STATUS)
        IF (OK) THEN
          ZMIN=RVAL
        ENDIF
        CALL GCB_GETR('PIX_MAX',OK,RVAL,STATUS)
        IF (OK) THEN
          ZMAX=RVAL
        ENDIF
        ZRAN=ZMAX-ZMIN
        IF (ZRAN.LE.0.0) THEN
          CALL MSG_PRNT('AST_ERR: invalid scaling range')
          STATUS=SAI__ERROR
        ENDIF

*  get colour capability of device
        CALL GDV_COLOURS(BG,FIRST,LAST,STATUS)
        NCOL=LAST-FIRST+1
        IF (NCOL.LT.8) THEN
          NCOL=8
          COLOUR=.FALSE.
        ELSE
          COLOUR=.TRUE.
        ENDIF

        IF (SCALING.EQ.'HIS') THEN
          CALL GFX_PIXELQ_HIST1(Z,NX,NY,Q,MASK,ZMIN,ZMAX,FIRST,LAST,
     :                                                       STATUS)
        ENDIF

        IF (STATUS.EQ.SAI__OK) THEN
*  plot pixel at a time

*  find if axes increasing or decreasing
          IF (NX.GT.1) THEN
            IF (X(2).GT.X(1)) THEN
              DIRX=1.0
            ELSE
              DIRX=-1.0
            ENDIF
          ELSE
            DIRX=1.0
          ENDIF
          IF (NY.GT.1) THEN
            IF (Y(2).GT.Y(1)) THEN
              DIRY=1.0
            ELSE
              DIRY=-1.0
            ENDIF
          ELSE
            DIRY=1.0
          ENDIF

          DO J=J1,J2
            DO I=I1,I2

*  check QUALITY
              IF (BIT_ANDUB(Q(I,J),MASK).EQ.QUAL__GOOD) THEN

*  set boundaries of pixel
                XHWID=XW(I)*DIRX/2.0
                X1=X(I)-XHWID
                X2=X(I)+XHWID
                YHWID=YW(J)*DIRY/2.0
                Y1=Y(J)-YHWID
                Y2=Y(J)+YHWID
*  set colour
                IF (SCALING.EQ.'LIN') THEN
                  CALL GFX_PIXEL_LIN(Z,NX,NY,I,J,IA,1,1,1,1,
     :                                ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'LOG') THEN
                  CALL GFX_PIXEL_LOG(Z,NX,NY,I,J,IA,1,1,1,1,
     :                                ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'SQR') THEN
                  CALL GFX_PIXEL_SQRT(Z,NX,NY,I,J,IA,1,1,1,1,
     :                                ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'PAR') THEN
                  CALL GFX_PIXEL_PARAB(Z,NX,NY,I,J,IA,1,1,1,1,
     :                                ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'SIN') THEN
                  CALL GFX_PIXEL_SIN(Z,NX,NY,I,J,IA,1,1,1,1,
     :                                ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'HIS') THEN
                  CALL GFX_PIXEL_HIST2(Z,NX,NY,I,J,IA,1,1,1,1,
     :                                ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'CYC') THEN
                  CALL GFX_PIXEL_CYC(Z,NX,NY,I,J,IA,1,1,1,1,
     :                                ZMIN,ZMAX,FIRST,LAST,STATUS)
                ENDIF

*  plot it
                IF (COLOUR) THEN
                  CALL PGPIXL(IA,1,1,1,1,1,1,X1,X2,Y1,Y2)
                ELSE
                  CALL GFX_PIXEL_DOTTY(IA,1,1,1,1,1,1,X1,X2,Y1,Y2,
     :                                                      STATUS)
                ENDIF

              ENDIF

            ENDDO
          ENDDO

        ENDIF

        CALL PGEBUF()

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_PIXELQ2',STATUS)
        ENDIF

      ENDIF
      END
