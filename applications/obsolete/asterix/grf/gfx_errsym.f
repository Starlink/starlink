*+  GFX_ERRSYM - plots specified error symbol
      SUBROUTINE GFX_ERRSYM(X,X1,X2,Y,Y1,Y2,SYMBOL,STATUS)

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
*    Import :
      REAL X,X1,X2
      REAL Y,Y1,Y2
      INTEGER SYMBOL
*    Import-export :
*    Export :
*    Status :
      INTEGER STATUS
*    Local Constants :
      REAL CONV
      PARAMETER (CONV=3.141593/180.0)
*    Local variables :
      REAL RX,RY
      REAL XX,YY
      REAL A
      INTEGER IA
*-
      IF (STATUS.NE.SAI__OK) RETURN


*  simple cross
      IF (SYMBOL.EQ.1) THEN
        CALL PGERRX(1,X1,X2,Y,0.0)
        CALL PGERRY(1,X,Y1,Y2,0.0)

*  barred cross
      ELSEIF (SYMBOL.EQ.2) THEN
        CALL PGERRX(1,X1,X2,Y,1.0)
        CALL PGERRY(1,X,Y1,Y2,1.0)

*  box
      ELSEIF (SYMBOL.EQ.3) THEN
        CALL PGMOVE(X1,Y1)
        CALL PGDRAW(X2,Y1)
        CALL PGDRAW(X2,Y2)
        CALL PGDRAW(X1,Y2)
        CALL PGDRAW(X1,Y1)

*  diamond
      ELSEIF (SYMBOL.EQ.4) THEN
        CALL PGMOVE(X,Y1)
        CALL PGDRAW(X2,Y)
        CALL PGDRAW(X,Y2)
        CALL PGDRAW(X1,Y)
        CALL PGDRAW(X,Y1)

*  ellipse
      ELSEIF (SYMBOL.EQ.5) THEN
        RX=(X2-X1)/2.0
        RY=(Y2-Y1)/2.0
        CALL PGMOVE(X2,Y)
        DO IA=1,360,4
          A=REAL(IA)*CONV
          XX=X+RX*COS(A)
          YY=Y+RY*SIN(A)
          CALL PGDRAW(XX,YY)
        ENDDO

*  vertical line
      ELSEIF (SYMBOL.EQ.6) THEN
        CALL PGMOVE(X,Y1)
        CALL PGDRAW(X,Y2)

      ENDIF

*  if error bar goes outside plotting window put on arrow
      IF (SYMBOL.EQ.1.OR.SYMBOL.EQ.2.OR.SYMBOL.EQ.6) THEN

*  get limits of plotting window
C        CALL PGQWIN(XW1,XW2,YW1,YW2)
C        IF (Y1.LT.YW1) THEN
C          CALL PGSAH(1,45.0,0.5)
C          CALL PGARRO(X,Y2,X,YW1)
C        ELSEIF (Y2.GT.YW2) THEN
C          CALL PGSAH(1,45.0,0.5)
C          CALL PGARRO(X,Y1,X,YW2)
C        ENDIF

      ENDIF

      END
