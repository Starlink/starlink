C*GRPXPS -- pixel dump for color or grey PostScript.
C+
      SUBROUTINE GRPXPS (IA, IDIM, JDIM, I1, I2, J1, J2,
     :                   XMIN, XMAX, YMIN, YMAX)
      INTEGER IDIM, JDIM, I1, I2, J1, J2
      INTEGER IA(IDIM,JDIM)
      REAL XMIN, XMAX, YMIN, YMAX
C
C This routine is called by GRPIXL.
C--
C  4-Apr-93 - Created from GRGRAY by Remko Scharroo - DUT/SSRT
C  8-Apr-93 - Bugs fixed.
C  6-Jul-94 - Aligned with PGPLOT V4.9H
C  7-Sep-94 - updated for V5.0 [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER  I, J, NXP, NYP, NBUF, LCHR, II
      REAL     DX,DY,RBUF(32)
      CHARACTER*32 CHR
C-----------------------------------------------------------------------
      NXP = I2 - I1 + 1
      NYP = J2 - J1 + 1
C
C Build an image transformation matrix.
C
      DX = (XMAX-XMIN)/NXP
      DY = (YMAX-YMIN)/NYP
      RBUF(1) = 0
      RBUF(2) = NXP
      RBUF(3) = NYP
      RBUF(4) = GRXMIN(GRCIDE)
      RBUF(5) = GRXMAX(GRCIDE)
      RBUF(6) = GRYMIN(GRCIDE)
      RBUF(7) = GRYMAX(GRCIDE)
      RBUF(8) = 1.0/DX
      RBUF(9) = 0.0
      RBUF(10) = 0.0
      RBUF(11) = 1.0/DY
      RBUF(12) = (-XMIN)/DX
      RBUF(13) = (-YMIN)/DY
C
C Send setup info to driver.
C
      IF (.NOT.GRPLTD(GRCIDE)) CALL GRBPIC
      CALL GRTERM
      NBUF = 13
      LCHR = 0
      CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
C
C Send the array of color indices to the driver.
C
      II = 0
      DO 20 J=J1,J2
         DO 10 I=I1,I2
            II = II + 1
            RBUF(II+1) = IA(I,J)
            IF (II.EQ.20) THEN
               NBUF = II+1
               RBUF(1) = II
               CALL GREXEC(GRGTYP, 26, RBUF, NBUF, CHR, LCHR)
               II = 0
            END IF
 10      CONTINUE
 20   CONTINUE
      IF (II.GT.0) THEN
         NBUF = II+1
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
