C*GRIMG1 -- image of a 2D data array (image-primitive devices)
C+
      SUBROUTINE GRIMG1 (A, IDIM, JDIM, I1, I2, J1, J2,
     1                   A1, A2, PA, MININD, MAXIND, MODE)
      INTEGER IDIM, JDIM, I1, I2, J1, J2, MININD, MAXIND, MODE
      REAL    A(IDIM,JDIM), A1, A2, PA(6)
C
C (This routine is called by GRIMG0.)
C--
C 7-Sep-1994  New routine [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER NBUF, LCHR
      REAL    RBUF(21), FAC, AV, SFAC, SFACL
      CHARACTER*1 CHR
      INTEGER  I, J, II, NXP, NYP, IV
      INTRINSIC NINT, LOG
      PARAMETER (SFAC=65000.0)
C-----------------------------------------------------------------------
C Size of image.
C
      NXP = I2 - I1 + 1
      NYP = J2 - J1 + 1
      RBUF(1) = 0.0
      RBUF(2) = NXP
      RBUF(3) = NYP
C
C Clipping rectangle.
C
      RBUF(4) = GRXMIN(GRCIDE)
      RBUF(5) = GRXMAX(GRCIDE)
      RBUF(6) = GRYMIN(GRCIDE)
      RBUF(7) = GRYMAX(GRCIDE)
C
C Image transformation matrix.
C
      FAC = PA(2)*PA(6) - PA(3)*PA(5)
      RBUF(8)  =  PA(6)/FAC
      RBUF(9)  = (-PA(5))/FAC
      RBUF(10) = (-PA(3))/FAC
      RBUF(11) =  PA(2)/FAC
      RBUF(12) = (PA(3)*PA(4) - PA(1)*PA(6))/FAC - (I1-0.5)
      RBUF(13) = (PA(5)*PA(1) - PA(4)*PA(2))/FAC - (J1-0.5)
C
C Send setup info to driver.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
      CALL GRTERM
      NBUF = 13
      LCHR = 0
      CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
C
C Convert image array to color indices and send to driver.
C
      SFACL = LOG(1.0+SFAC)
      II = 0
      DO 20 J = J1,J2
          DO 10 I = I1,I2
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
              II = II + 1
              RBUF(II+1) = IV
              IF (II.EQ.20) THEN
                  NBUF = II + 1
                  RBUF(1) = II
                  CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
                  II = 0
              END IF
   10     CONTINUE
   20 CONTINUE
      IF (II.GT.0) THEN
          NBUF = II + 1
          RBUF(1) = II
          CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
          II = 0
      END IF
C
C Send termination code to driver.
C
      NBUF = 1
      RBUF(1) = -1
      CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
C-----------------------------------------------------------------------
      END
