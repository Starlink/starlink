*+  GFX_QPIXEL - plots quality mask
      SUBROUTINE GFX_QPIXEL(WKPTR,NX,NY,I1,I2,J1,J2,REG,X,Y,XW,YW,
     :                                               Q,MASK,STATUS)

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
      BYTE Q(NX,NY)
      BYTE MASK
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
*    Local variables :
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

*  get colour capability of device
        CALL GDV_COLOURS(BG,FIRST,LAST,STATUS)
        NCOL=LAST-FIRST+1
        IF (NCOL.LT.8) THEN
          NCOL=8
          FIRST=1
          LAST=8
          BG=0
          COLOUR=.FALSE.
        ELSE
          COLOUR=.TRUE.
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

*  set colours
                CALL GFX_QPIXEL_SET(NX,NY,I,J,Q,MASK,%VAL(IPTR),
     :                          IX,JY,NI,NJ,BG,FIRST,LAST,STATUS)

*  plot this bit
                IF (COLOUR) THEN
                  CALL PGPIXL(%val(IPTR),IX,JY,1,NI,1,NJ,X1,X2,Y1,Y2)
                ELSE
                  CALL GFX_PIXEL_DOTTY(%val(IPTR),
     :                             IX,JY,1,NI,1,NJ,X1,X2,Y1,Y2,STATUS)
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
                CALL GFX_QPIXEL_SET(NX,NY,I,J,Q,MASK,IA,1,1,1,1,
     :                                      BG,FIRST,LAST,STATUS)

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
          CALL ERR_REP(' ','from GFX_QPIXEL',STATUS)
        ENDIF

      ENDIF
      END


*+
      SUBROUTINE GFX_QPIXEL_SET(NX,NY,I,J,Q,MASK,IA,IX,IY,NI,NJ,
     :                                       BG,COL1,COL2,STATUS)

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
      INTEGER I,J
      INTEGER IX,IY
      INTEGER NI,NJ
      INTEGER BG,COL1,COL2
      BYTE Q(NX,NY)
      BYTE MASK
*    Import-export :
      INTEGER IA(IX,IY)
*    Export :
*    Status :
      INTEGER STATUS
*    Functions :
      BYTE BIT_ANDUB
*    Local Constants :
*    Global variables :
*    Local variables :
      INTEGER II,JJ,III,JJJ
*    Local data :
*-
      IF (STATUS.EQ.SAI__OK) THEN


        JJJ=J
        DO JJ=1,NJ

          III=I
          DO II=1,NI

            IF (BIT_ANDUB(Q(III,JJJ),MASK).EQ.QUAL__GOOD) THEN
*  good quality - set to maximum colour
              IA(II,JJ)=COL2

            ELSE
*  if bad quality set to background colour
              IA(II,JJ)=BG
            ENDIF

            III=III+1

          ENDDO

          JJJ=JJJ+1
        ENDDO


        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_QPIXEL_SET',STATUS)
        ENDIF

      ENDIF
      END
