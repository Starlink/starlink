*+  GFX_QCONTOUR - plot contours around areas of bad quality
      SUBROUTINE GFX_QCONTOUR(NX,NY,I1,I2,J1,J2,X,Y,
     :                                 Q,MASK,STATUS)

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
*    Import :
      INTEGER NX,NY
      INTEGER I1,I2,J1,J2
      REAL X(*),Y(*)
      BYTE Q(NX,NY)
      BYTE MASK
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Global variables :
      COMMON /GFX_CONT_CMN/ XX,YY
      REAL XX(0:51),YY(0:51)
*    Functions :
      BYTE BIT_ANDUB
*    Local Constants :
      INTEGER IX,JY
      PARAMETER (IX=50,JY=50)
*    Local variables :
      REAL TILE(IX,JY)
      INTEGER I,J,L
      INTEGER II,JJ,III,JJJ,NI,NJ
      INTEGER IXT,JYT,NXT,NYT
      INTEGER SXT,SYT,SNX,SNY
*    External references :
      EXTERNAL GFX_CONTOUR_PLOT
*-

      IF (STATUS.EQ.SAI__OK) THEN

        CALL PGBBUF()

        CALL GCB_SETDEF(STATUS)

*  estimate number of tiles in each direction and alter size to make equal
        SNX=I2-I1+1
        IF (SNX.LE.IX) THEN
          NXT=1
          SXT=SNX
        ELSE
          NXT=SNX/(IX-1) + 1
          SXT=SNX/NXT + 1
        ENDIF
        SNY=J2-J1+1
        IF (SNY.LE.JY) THEN
          NYT=1
          SYT=SNY
        ELSE
          NYT=SNY/(JY-1) + 1
          SYT=SNY/NYT + 1
        ENDIF
*  contour a tile at a time
        J=J1
        DO JYT=1,NYT

          I=I1
          DO IXT=1,NXT

            NJ=MIN(SYT,J2-J+1)
            JJJ=J
            DO JJ=1,NJ

              NI=MIN(SXT,I2-I+1)
              III=I

              DO II=1,NI

                IF (BIT_ANDUB(Q(III,JJJ),MASK).NE.QUAL__GOOD) THEN
                  TILE(II,JJ)=1.0
                ELSE
                  TILE(II,JJ)=0.0
                ENDIF

                XX(II)=X(III)
                III=III+1

              ENDDO

              YY(JJ)=Y(JJJ)
              JJJ=JJJ+1
            ENDDO

            YY(0)=2.0*YY(1)-YY(2)
            YY(NJ+1)=2.0*YY(NJ)-YY(NJ-1)
            XX(0)=2.0*XX(1)-XX(2)
            XX(NI+1)=2.0*XX(NI)-XX(NI-1)

            CALL PGCNSC(TILE,IX,JY,1,NI,1,NJ,1.0,
     :                                  GFX_CONTOUR_PLOT)


            I=III-1
          ENDDO
          J=JJJ-1
        ENDDO


*  restore default attributes
        CALL GCB_SETDEF(STATUS)

        CALL PGEBUF()

        IF (STATUS.NE.SAI__OK) THEN
          CALL ERR_REP(' ','from GFX_QCONTOUR',STATUS)
        ENDIF

      ENDIF

      END
