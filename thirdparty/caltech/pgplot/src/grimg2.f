C*GRIMG2 -- image of a 2D data array (pixel-primitive devices)
C+
      SUBROUTINE GRIMG2 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, PA, MININD, MAXIND, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND, MODE
      REAL    A(IDIM,JDIM)
      REAL    A1, A2
      REAL    PA(6)
C
C (This routine is called by GRIMG0.)
C--
C 7-Sep-1994  New routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I,IV,IX,IX1,IX2,IY,IY1,IY2,J, NPIX, LCHR
      REAL     DEN, AV, SFAC, SFACL
      REAL     XXAA,XXBB,YYAA,YYBB,XYAA,XYBB,YXAA,YXBB,XYAAIY,YXAAIY
      REAL     BUFFER(1026)
      CHARACTER*1 CHR
      INTRINSIC NINT, LOG
      PARAMETER (SFAC=65000.0)
C-----------------------------------------------------------------------
C
C Location of current window in device coordinates.
C
      IX1 = NINT(GRXMIN(GRCIDE))+1
      IX2 = NINT(GRXMAX(GRCIDE))-1
      IY1 = NINT(GRYMIN(GRCIDE))+1
      IY2 = NINT(GRYMAX(GRCIDE))-1
C
C Transformation from array coordinates to device coordinates.
C
      DEN = PA(2)*PA(6)-PA(3)*PA(5)
      XXAA = (-PA(6))*PA(1)/DEN
      XXBB = PA(6)/DEN
      XYAA = (-PA(3))*PA(4)/DEN
      XYBB = PA(3)/DEN
      YYAA = (-PA(2))*PA(4)/DEN
      YYBB = PA(2)/DEN
      YXAA = (-PA(5))*PA(1)/DEN
      YXBB = PA(5)/DEN
C
C Start a new page if necessary.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
C
C Run through every device pixel (IX, IY) in the current window and
C determine which array pixel (I,J) it falls in.
C
      SFACL = LOG(1.0+SFAC)
      DO 120 IY=IY1,IY2
          XYAAIY = XXAA-XYAA-XYBB*IY
          YXAAIY = YYAA+YYBB*IY-YXAA
          NPIX = 0
          BUFFER(2) = IY
          DO 110 IX=IX1,IX2
            I = NINT(XYAAIY+XXBB*IX)
            IF (I.LT.I1.OR.I.GT.I2) GOTO 110
            J = NINT(YXAAIY-YXBB*IX)
            IF (J.LT.J1.OR.J.GT.J2) GOTO 110
C
C           -- determine color index IV of this pixel
C
            AV = A(I,J)
            IF (A2.GT.A1) THEN
                AV = MIN(A2, MAX(A1,AV))
            ELSE
                AV = MIN(A1, MAX(A2,AV))
            END IF
            IF (MODE.EQ.0) THEN
                IV = NINT((MININD*(A2-AV) + MAXIND*(AV-A1))/(A2-A1))
            ELSE IF (MODE.EQ.1) THEN
                IV = MININD + NINT((MAXIND-MININD)*
     :               LOG(1.0+SFAC*ABS((AV-A1)/(A2-A1)))/SFACL)
            ELSE IF (MODE.EQ.2) THEN
                IV = MININD + NINT((MAXIND-MININD)*
     :                             SQRT(ABS((AV-A1)/(A2-A1))))
            ELSE
                IV = MININD
            END IF
C
            IF (NPIX.LE.1024) THEN
C               -- drop pixels if buffer too small (to be fixed!)
                NPIX = NPIX+1
                IF (NPIX.EQ.1) BUFFER(1) = IX
                BUFFER(NPIX+2) = IV
            END IF
  110     CONTINUE
          IF (NPIX.GT.0) CALL 
     :                   GREXEC(GRGTYP, 26, BUFFER, NPIX+2, CHR, LCHR)
  120 CONTINUE
C-----------------------------------------------------------------------
      END
