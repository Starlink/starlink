*+  GFX_PIXEL - plots pixels
      SUBROUTINE GFX_PIXEL(WKPTR,NX,NY,I1,I2,J1,J2,REG,X,Y,XW,YW,
     :                                        Z,ZMIN,ZMAX,STATUS)

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
*    Import :
      INTEGER WKPTR
      INTEGER NX,NY
      INTEGER I1,I2,J1,J2
      LOGICAL REG
      REAL X(*),Y(*)
      REAL XW(*),YW(*)
      REAL Z(NX,NY)
*    Import-export :
      REAL ZMIN,ZMAX
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      CHARACTER*10 SCALING
      REAL RVAL
      REAL ZRAN
      REAL X1,X2,Y1,Y2
      REAL XHWID,YHWID
      REAL DIRX,DIRY
      INTEGER IX,JY
      INTEGER DIMS(2)
      INTEGER BG,FIRST,LAST
      INTEGER NCOL
      INTEGER I,J,II,JJ
      INTEGER NI,NJ
      INTEGER SNX,SNY
      INTEGER IXT,NXT,JYT,NYT
      INTEGER IPTR,IA
      LOGICAL COLOUR
      LOGICAL OK
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        CALL PGBBUF()

*  see if work space sent from outside
        IF (WKPTR.EQ.0) THEN
          IX=NX
          JY=NY
          DIMS(1)=NX
          DIMS(2)=NY
          CALL DYN_MAPI(2,DIMS,IPTR,STATUS)
        ELSE
          IPTR=WKPTR
          IX=NX
          JY=NY
        ENDIF

*  get scaling type
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
     :                                SCALING.EQ.'SIN')) THEN
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
*  if less than 8 treat as monochrome and do a dotty plot
        IF (NCOL.LT.8) THEN
          NCOL=8
          FIRST=1
          LAST=8
          COLOUR=.FALSE.
        ELSE
          COLOUR=.TRUE.
        ENDIF

*  for histogram equalisation need to equalise whole image
        IF (SCALING.EQ.'HIS') THEN
          CALL GFX_PIXEL_HIST1(Z,NX,NY,ZMIN,ZMAX,FIRST,LAST,
     :                                                STATUS)
        ENDIF

        IF (STATUS.EQ.SAI__OK) THEN
*  regular axes - plot a tile at a time
          IF (REG) THEN

            XHWID=(X(2)-X(1))/2.0
            YHWID=(Y(2)-Y(1))/2.0

*  how many tiles in each direction
            SNX=I2-I1+1
            NXT=SNX/IX
            IF (MOD(SNX,IX).NE.0) THEN
              NXT=NXT+1
            ENDIF
            SNY=J2-J1+1
            NYT=SNY/JY
            IF (MOD(SNY,JY).NE.0) THEN
              NYT=NYT+1
            ENDIF

            J=J1
            DO JYT=1,NYT

              I=I1
              DO IXT=1,NXT

                NI=MIN(IX,I2-I+1)
                NJ=MIN(JY,J2-J+1)
                II=I+NI-1
                JJ=J+NJ-1
                Y1=Y(J)-YHWID
                X1=X(I)-XHWID
                X2=X(II)+XHWID
                Y2=Y(JJ)+YHWID

*  scale
                IF (SCALING.EQ.'LIN') THEN
                  CALL GFX_PIXEL_LIN(Z,NX,NY,I,J,%VAL(IPTR),IX,JY,
     :                          NI,NJ,ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'CYC') THEN
                  CALL GFX_PIXEL_CYC(Z,NX,NY,I,J,%VAL(IPTR),IX,JY,
     :                          NI,NJ,ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'LOG') THEN
                  CALL GFX_PIXEL_LOG(Z,NX,NY,I,J,%VAL(IPTR),IX,JY,
     :                          NI,NJ,ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'SQR') THEN
                  CALL GFX_PIXEL_SQRT(Z,NX,NY,I,J,%VAL(IPTR),IX,JY,
     :                          NI,NJ,ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'PAR') THEN
                  CALL GFX_PIXEL_PARAB(Z,NX,NY,I,J,%VAL(IPTR),IX,JY,
     :                          NI,NJ,ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'SIN') THEN
                  CALL GFX_PIXEL_SIN(Z,NX,NY,I,J,%VAL(IPTR),IX,JY,
     :                          NI,NJ,ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'HIS') THEN
                  CALL GFX_PIXEL_HIST2(Z,NX,NY,I,J,%VAL(IPTR),IX,JY,
     :                                       NI,NJ,FIRST,LAST,STATUS)
                ENDIF

*  plot this bit
                IF (COLOUR) THEN
                  CALL PGPIXL(%val(IPTR),IX,JY,1,NI,1,NJ,X1,X2,Y1,Y2)
                ELSE
                  CALL GFX_PIXEL_DOTTY(%val(IPTR),
     :                              IX,JY,1,NI,1,NJ,X1,X2,Y1,Y2,STATUS)
                ENDIF

                I=II
              ENDDO
              J=JJ
            ENDDO

*  irregular axes - plot pixel at a time
          ELSE

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
     :                           ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'CYC') THEN
                  CALL GFX_PIXEL_CYC(Z,NX,NY,I,J,IA,1,1,1,1,
     :                           ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'LOG') THEN
                  CALL GFX_PIXEL_LOG(Z,NX,NY,I,J,IA,1,1,1,1,
     :                           ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'SQR') THEN
                  CALL GFX_PIXEL_SQRT(Z,NX,NY,I,J,IA,1,1,1,1,
     :                           ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'PAR') THEN
                  CALL GFX_PIXEL_PARAB(Z,NX,NY,I,J,IA,1,1,1,1,
     :                           ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'SIN') THEN
                  CALL GFX_PIXEL_SIN(Z,NX,NY,I,J,IA,1,1,1,1,
     :                           ZMIN,ZMAX,FIRST,LAST,STATUS)
                ELSEIF (SCALING.EQ.'HIS') THEN
                  CALL GFX_PIXEL_HIST2(Z,NX,NY,I,J,IA,1,1,1,1,
     :                                      FIRST,LAST,STATUS)
                ENDIF

*  plot it
                IF (COLOUR) THEN
                  CALL PGPIXL(IA,1,1,1,1,1,1,X1,X2,Y1,Y2)
                ELSE
                  CALL GFX_PIXEL_DOTTY(IA,1,1,1,1,1,1,X1,X2,Y1,Y2,
     :                                                      STATUS)
                ENDIF

              ENDDO
            ENDDO

          ENDIF

        ENDIF

        CALL PGEBUF()

        IF (WKPTR.EQ.0) THEN
          CALL DYN_UNMAP(IPTR,STATUS)
        ENDIF

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_PIXEL',STATUS)
        ENDIF

      ENDIF
      END


*+  GFX_PIXEL_LIN - plots pixels
      SUBROUTINE GFX_PIXEL_LIN(Z,NX,NY,I,J,IA,IX,IY,NI,NJ,
     :                          ZMIN,ZMAX,COL1,COL2,STATUS)

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
*    Import :
      INTEGER NX,NY
      INTEGER I,J
      REAL Z(NX,NY)
      REAL ZMAX,ZMIN
      INTEGER IX,IY
      INTEGER NI,NJ
      INTEGER COL1,COL2
*    Import-export :
      INTEGER IA(IX,IY)
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_PIX_CMN'
*    Local Constants :
*    Local variables :
      REAL ZRAN
      REAL SCALE
      INTEGER ICOL,NCOL
      INTEGER II,JJ,III,JJJ
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  set scaling factor
        NCOL=COL2-COL1+1
        ZRAN=ZMAX-ZMIN
        SCALE=REAL(NCOL)/ZRAN

*  set bounds for plotting key
        G_BOUNDS(1,1)=ZMIN
        DO ICOL=2,NCOL
          G_BOUNDS(1,ICOL)=ZMIN+REAL(ICOL-1)/SCALE
          G_BOUNDS(2,ICOL-1)=G_BOUNDS(1,ICOL)
        ENDDO
        G_BOUNDS(2,NCOL)=ZMAX
        G_NBOUNDS=NCOL

        JJJ=J
        DO JJ=1,NJ

          III=I
          DO II=1,NI

*  scale linearly between limits
            IA(II,JJ)=COL1 + INT((Z(III,JJJ)-ZMIN)*SCALE)
            IA(II,JJ)=MAX(IA(II,JJ),COL1)
            IA(II,JJ)=MIN(IA(II,JJ),COL2)
            III=III+1
          ENDDO

          JJJ=JJJ+1
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_PIXEL_LIN',STATUS)
        ENDIF

      ENDIF
      END

*+
      SUBROUTINE GFX_PIXEL_CYC(Z,NX,NY,I,J,IA,IX,IY,NI,NJ,
     :                          ZMIN,ZMAX,COL1,COL2,STATUS)

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
*    Import :
      INTEGER NX,NY
      INTEGER I,J
      REAL Z(NX,NY)
      REAL ZMAX,ZMIN
      INTEGER IX,IY
      INTEGER NI,NJ
      INTEGER COL1,COL2
*    Import-export :
      INTEGER IA(IX,IY)
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
      REAL ZRAN
      REAL ZVAL
      REAL CYCRAN
      REAL SCALE
      INTEGER NCOL
      INTEGER NCYCLE
      INTEGER II,JJ,III,JJJ
      LOGICAL OK
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN

*  get cycling range
        CALL GCB_GETI('PIX_CYCLES',OK,NCYCLE,STATUS)
        IF (.NOT.OK) THEN
          NCYCLE=1
        ENDIF
        ZRAN=ZMAX-ZMIN
        CYCRAN=ZRAN/REAL(NCYCLE)

*  set scaling factor
        NCOL=COL2-COL1+1
        SCALE=REAL(NCOL)/CYCRAN

        JJJ=J
        DO JJ=1,NJ

          III=I
          DO II=1,NI

*  scale between limits
            ZVAL=Z(III,JJJ)
            IF (ZVAL.GT.ZMAX) THEN
              IA(II,JJ)=COL2
            ELSEIF (ZVAL.LT.ZMIN) THEN
              IA(II,JJ)=COL1
            ELSE
              ZVAL=MOD(ZVAL-ZMIN,CYCRAN)
              IA(II,JJ)=COL1 + INT(ZVAL*SCALE)
              IA(II,JJ)=MAX(COL1,IA(II,JJ))
              IA(II,JJ)=MIN(COL2,IA(II,JJ))
            ENDIF
            III=III+1
          ENDDO

          JJJ=JJJ+1
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_PIXEL_CYC',STATUS)
        ENDIF

      ENDIF
      END


*+
      SUBROUTINE GFX_PIXEL_LOG(Z,NX,NY,I,J,IA,IX,IY,NI,NJ,
     :                          ZMIN,ZMAX,COL1,COL2,STATUS)

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
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER NX,NY
      INTEGER I,J
      REAL Z(NX,NY)
      REAL ZMAX,ZMIN
      INTEGER IX,IY
      INTEGER NI,NJ
      INTEGER COL1,COL2
*    Import-export :
      INTEGER IA(IX,IY)
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_PIX_CMN'
*    Local Constants :
*    Local variables :
      REAL ZRAN
      REAL RVAL,ZZMIN,ZZMAX
      REAL SCALE,OFFSET
      INTEGER ICOL,NCOL
      INTEGER II,JJ,III,JJJ
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  set scaling factor
        NCOL=COL2-COL1+1
        IF (ZMIN.LT.VAL__SMLR) THEN
          OFFSET=VAL__SMLR-ZMIN
        ELSE
          OFFSET=0.0
        ENDIF
        ZZMIN=LOG10(ZMIN+OFFSET)
        ZZMAX=LOG10(ZMAX+OFFSET)
        ZRAN=ZZMAX-ZZMIN
        SCALE=REAL(NCOL)/ZRAN

*  set bounds for plotting key
        G_BOUNDS(1,1)=ZMIN
        DO ICOL=2,NCOL
          G_BOUNDS(1,ICOL)=10.0**(ZZMIN+REAL(ICOL-1)/SCALE)-OFFSET
          G_BOUNDS(2,ICOL-1)=G_BOUNDS(1,ICOL)
        ENDDO
        G_BOUNDS(2,NCOL)=ZMAX
        G_NBOUNDS=NCOL

        JJJ=J
        DO JJ=1,NJ

          III=I
          DO II=1,NI

*  scale between limits
            RVAL=Z(III,JJJ)+OFFSET
            RVAL=MAX(VAL__SMLR,RVAL)
            RVAL=LOG10(RVAL)
            IA(II,JJ)=COL1 + INT((RVAL-ZZMIN)*SCALE)
            IA(II,JJ)=MAX(IA(II,JJ),COL1)
            IA(II,JJ)=MIN(IA(II,JJ),COL2)
            III=III+1

          ENDDO

          JJJ=JJJ+1
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_PIXEL_LOG',STATUS)
        ENDIF

      ENDIF
      END


*+
      SUBROUTINE GFX_PIXEL_SQRT(Z,NX,NY,I,J,IA,IX,IY,NI,NJ,
     :                          ZMIN,ZMAX,COL1,COL2,STATUS)

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
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER NX,NY
      INTEGER I,J
      REAL Z(NX,NY)
      REAL ZMAX,ZMIN
      INTEGER IX,IY
      INTEGER NI,NJ
      INTEGER COL1,COL2
*    Import-export :
      INTEGER IA(IX,IY)
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_PIX_CMN'
*    Local Constants :
*    Local variables :
      REAL ZRAN
      REAL RVAL,ZZMIN,ZZMAX
      REAL SCALE,OFFSET
      INTEGER ICOL,NCOL
      INTEGER II,JJ,III,JJJ
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  set scaling factor
        NCOL=COL2-COL1+1
        IF (ZMIN.LE.VAL__SMLR) THEN
          OFFSET=VAL__SMLR-ZMIN
        ELSE
          OFFSET=0.0
        ENDIF
        ZZMIN=SQRT(ZMIN+OFFSET)
        ZZMAX=SQRT(ZMAX+OFFSET)
        ZRAN=ZZMAX-ZZMIN
        SCALE=REAL(NCOL)/ZRAN

*  set bounds for plotting key
        G_BOUNDS(1,1)=ZMIN
        DO ICOL=2,NCOL
          G_BOUNDS(1,ICOL)=(ZZMIN+REAL(ICOL-1)/SCALE)**2 -OFFSET
          G_BOUNDS(2,ICOL-1)=G_BOUNDS(1,ICOL)
        ENDDO
        G_BOUNDS(2,NCOL)=ZMAX
        G_NBOUNDS=NCOL

        JJJ=J
        DO JJ=1,NJ

          III=I
          DO II=1,NI

*  scale between limits
            RVAL=Z(III,JJJ)+OFFSET
            RVAL=MAX(VAL__SMLR,RVAL)
            RVAL=SQRT(RVAL)
            IA(II,JJ)=COL1 + INT((RVAL-ZZMIN)*SCALE)
            IA(II,JJ)=MAX(IA(II,JJ),COL1)
            IA(II,JJ)=MIN(IA(II,JJ),COL2)
            III=III+1

          ENDDO

          JJJ=JJJ+1
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_PIXEL_SQRT',STATUS)
        ENDIF

      ENDIF
      END


*+
      SUBROUTINE GFX_PIXEL_PARAB(Z,NX,NY,I,J,IA,IX,IY,NI,NJ,
     :                          ZMIN,ZMAX,COL1,COL2,STATUS)

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
      INCLUDE 'PRM_PAR'
*    Import :
      INTEGER NX,NY
      INTEGER I,J
      REAL Z(NX,NY)
      REAL ZMAX,ZMIN
      INTEGER IX,IY
      INTEGER NI,NJ
      INTEGER COL1,COL2
*    Import-export :
      INTEGER IA(IX,IY)
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_PIX_CMN'
*    Local Constants :
*    Local variables :
      REAL ZRAN
      REAL RVAL,ZZMIN,ZZMAX
      REAL SCALE,OFFSET
      INTEGER ICOL,NCOL
      INTEGER II,JJ,III,JJJ
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  set scaling factor
        NCOL=COL2-COL1+1
        IF (ZMIN.LE.VAL__SMLR) THEN
          OFFSET=VAL__SMLR-ZMIN
        ELSE
          OFFSET=0.0
        ENDIF
        ZZMIN=(ZMIN+OFFSET)**2
        ZZMAX=(ZMAX+OFFSET)**2
        ZRAN=ZZMAX-ZZMIN
        SCALE=REAL(NCOL)/ZRAN

*  set bounds for plotting key
        G_BOUNDS(1,1)=ZMIN
        DO ICOL=2,NCOL
          G_BOUNDS(1,ICOL)=SQRT(ZZMIN+REAL(ICOL-1)/SCALE)-OFFSET
          G_BOUNDS(2,ICOL-1)=G_BOUNDS(1,ICOL)
        ENDDO
        G_BOUNDS(2,NCOL)=ZMAX
        G_NBOUNDS=NCOL

        JJJ=J
        DO JJ=1,NJ

          III=I
          DO II=1,NI

*  scale between limits
            RVAL=(Z(III,JJJ)+OFFSET)**2
            IA(II,JJ)=COL1 + INT((RVAL-ZZMIN)*SCALE)
            IA(II,JJ)=MAX(IA(II,JJ),COL1)
            IA(II,JJ)=MIN(IA(II,JJ),COL2)
            III=III+1

          ENDDO

          JJJ=JJJ+1
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_PIXEL_PARAB',STATUS)
        ENDIF

      ENDIF
      END



*+
      SUBROUTINE GFX_PIXEL_SIN(Z,NX,NY,I,J,IA,IX,IY,NI,NJ,
     :                          ZMIN,ZMAX,COL1,COL2,STATUS)

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
*    Import :
      INTEGER NX,NY
      INTEGER I,J
      REAL Z(NX,NY)
      REAL ZMAX,ZMIN
      INTEGER IX,IY
      INTEGER NI,NJ
      INTEGER COL1,COL2
*    Import-export :
      INTEGER IA(IX,IY)
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_PIX_CMN'
*    Local Constants :
      REAL PI,PIBY2
      PARAMETER (PI=3.141593,PIBY2=PI/2.0)
*    Local variables :
      REAL ZRAN
      REAL RVAL
      INTEGER ICOL,NCOL
      INTEGER II,JJ,III,JJJ
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        NCOL=COL2-COL1+1
        ZRAN=ZMAX-ZMIN

*  set bounds for plotting key
        G_BOUNDS(1,1)=ZMIN
        DO ICOL=2,NCOL
          G_BOUNDS(1,ICOL)=ZMIN +
     :           (ASIN(REAL(ICOL-1)*2.0/REAL(NCOL)-1.0)+PIBY2)/PI*ZRAN
          G_BOUNDS(2,ICOL-1)=G_BOUNDS(1,ICOL)
        ENDDO
        G_BOUNDS(2,NCOL)=ZMAX
        G_NBOUNDS=NCOL

        JJJ=J
        DO JJ=1,NJ

          III=I
          DO II=1,NI

*  scale between limits
            RVAL=Z(III,JJJ)
            IF (RVAL.LE.ZMIN) THEN
              IA(II,JJ)=COL1
            ELSEIF (RVAL.GE.ZMAX) THEN
              IA(II,JJ)=COL2
            ELSE
              IA(II,JJ)=COL1 +
     :            INT((SIN((RVAL-ZMIN)/ZRAN*PI-PIBY2)+1.0)/2.0*NCOL)
            ENDIF

            III=III+1

          ENDDO

          JJJ=JJJ+1
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_PIXEL_SIN',STATUS)
        ENDIF

      ENDIF
      END




*+  GFX_PIXEL_HIST - histogram equalisation scaling
      SUBROUTINE GFX_PIXEL_HIST1(Z,NX,NY,ZMIN,ZMAX,COL1,COL2,STATUS)

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
*    Import :
      INTEGER NX,NY
      REAL Z(NX,NY)
      REAL ZMAX,ZMIN
      INTEGER COL1,COL2
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_PIX_CMN'
*    Local Constants :
      INTEGER OVERSAMPLE
      PARAMETER (OVERSAMPLE=1)
      INTEGER MAXCOL
      PARAMETER (MAXCOL=256)
*    Local variables :
      REAL ZRAN
      REAL BWID
      INTEGER HIST(MAXCOL*OVERSAMPLE)
      INTEGER NCOL,ICOL
      INTEGER NMAX,TOT,RESID
      INTEGER IBIN,NBIN,NPIX
      INTEGER II,JJ
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN


*  do equalisation
        NCOL=COL2-COL1+1
        NBIN=NCOL*OVERSAMPLE
        DO IBIN=1,NBIN
          HIST(IBIN)=0
        ENDDO
        ZRAN=ZMAX-ZMIN
        BWID=ZRAN/REAL(NBIN)
        NPIX=NX*NY
        NMAX=NPIX/NCOL
*  histogram data
        DO JJ=1,NY
          DO II=1,NX
            IBIN=INT((Z(II,JJ)-ZMIN)/ZRAN*REAL(NBIN))+1
            IBIN=MIN(IBIN,NBIN)
            HIST(IBIN)=HIST(IBIN)+1
          ENDDO
        ENDDO
*  reform bins of equal population
        ICOL=1
        G_BOUNDS(1,ICOL)=ZMIN
        RESID=0
        IBIN=0
        DO WHILE (ICOL.LT.NCOL)
          TOT=RESID
          DO WHILE (TOT.LT.NMAX)
            IBIN=IBIN+1
            TOT=TOT+HIST(IBIN)
          ENDDO
          RESID=TOT-NMAX
          G_BOUNDS(2,ICOL)=ZMIN+REAL(IBIN)*BWID -
     :                       REAL(RESID)/REAL(HIST(IBIN))*BWID
          ICOL=ICOL+1
          G_BOUNDS(1,ICOL)=G_BOUNDS(2,ICOL-1)
        ENDDO
        G_BOUNDS(2,ICOL)=ZMAX


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_PIXEL_HIST1',STATUS)
        ENDIF

      ENDIF
      END

      SUBROUTINE GFX_PIXEL_HIST2(Z,NX,NY,I,J,IA,IX,IY,NI,NJ,
     :                                      COL1,COL2,STATUS)

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
*    Import :
      INTEGER NX,NY
      INTEGER I,J
      REAL Z(NX,NY)
      INTEGER IX,IY
      INTEGER NI,NJ
      INTEGER COL1,COL2
*    Import-export :
      INTEGER IA(IX,IY)
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      INCLUDE 'GFX_PIX_CMN'
*    Local Constants :
*    Local variables :
      INTEGER NCOL,ICOL
      INTEGER II,JJ,III,JJJ
      INTEGER B1,B2
      LOGICAL FOUND
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN

        NCOL=COL2-COL1+1

        JJJ=J
        DO JJ=1,NJ

          III=I
          DO II=1,NI

*  assign colours according to bin boundaries
            B1=1
            B2=NCOL
            FOUND=.FALSE.
            DO WHILE (.NOT.FOUND)

              ICOL=(B1+B2)/2
              IF (Z(III,JJJ).GE.G_BOUNDS(2,ICOL)) THEN
                B1=ICOL+1
                IF (B1.GE.NCOL) THEN
                  IA(II,JJ)=COL2
                  FOUND=.TRUE.
                ENDIF
              ELSEIF (Z(III,JJJ).LT.G_BOUNDS(1,ICOL)) THEN
                B2=ICOL-1
                IF (B2.LE.1) THEN
                  IA(II,JJ)=COL1
                  FOUND=.TRUE.
                ENDIF
              ELSE
                IA(II,JJ)=COL1+ICOL-1
                FOUND=.TRUE.
              ENDIF

            ENDDO

            III=III+1
          ENDDO

          JJJ=JJJ+1
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_PIXEL_HIST2',STATUS)
        ENDIF

      ENDIF
      END



      SUBROUTINE GFX_PIXEL_DOTTY(IA,NX,NY,I1,I2,J1,J2,X1,X2,Y1,Y2,
     :                                                     STATUS)

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
*    Import :
      INTEGER NX,NY
      INTEGER IA(NX,NY)
      INTEGER I1,I2,J1,J2
      REAL X1,X2,Y1,Y2
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
      REAL GFX_PIXEL_RNDINT
*    Local Constants :
      INTEGER NP
      PARAMETER (NP=64)
*    Local variables :
      REAL X(NP),Y(NP)
      REAL XX1,YY1,XX2,YY2
      REAL DX,DY
      INTEGER I,J,K
      INTEGER NDOT
*-
      IF (STATUS.NE.SAI__OK) RETURN

      CALL GCB_SETDEF(STATUS)

      DX=(X2-X1)/REAL(I2-I1+1)
      DY=(Y2-Y1)/REAL(J2-J1+1)

      YY1=Y1
      DO J=J1,J2
        XX1=X1
        YY2=YY1+DY
        DO I=I1,I2
          NDOT=IA(I,J)**2
          XX2=XX1+DX

          DO K=1,NDOT
            X(K)= GFX_PIXEL_RNDINT( XX1, XX2 )
            Y(K)= GFX_PIXEL_RNDINT( YY1, YY2 )
          ENDDO

          CALL PGPOINT(NDOT,X,Y,1)

          XX1=XX2

        ENDDO

        YY1=YY2

      ENDDO

      END


*+  GFX_PIXEL_RNDINT - Return random number in interval LO to HI
      REAL FUNCTION GFX_PIXEL_RNDINT( LO, HI )
*    Description :
*    Method :
*
*    Deficiencies :
*    Bugs :
*    Authors :
*             (BHVAD:DJA)
*    History :
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      REAL LO,HI
*    Functions :
      REAL MATH_RND
*-
      GFX_PIXEL_RNDINT = MATH_RND()*(HI-LO)+LO

      END
