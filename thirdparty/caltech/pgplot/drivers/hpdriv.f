      SUBROUTINE HPDRIV (IFUNC, RBUF, NBUF, CHR, LCHR)
      INTEGER IFUNC, NBUF, LCHR
      REAL    RBUF(*)
      CHARACTER*(*) CHR
C-----------------------------------------------------------------------
C PGPLOT driver for Hewlett Packard HP 7221 pen plotter.
C-----------------------------------------------------------------------
C Version 1.0  - 1987 May 26 - T. J. Pearson.
C Version 1.1  - 1994 Dec 01 - TJP (revised for standard Fortran-77).
C
C-----------------------------------------------------------------------
      CHARACTER*(*) DEVICE, DEFNAM
      PARAMETER (DEVICE='HP7221 (Hewlett-Packard HP7221 pen plotter')
      PARAMETER (DEFNAM='pgplot.hpplot')
      CHARACTER*10 MSG
      CHARACTER*11 GRHPLI
      CHARACTER*5  GRHPCP
      CHARACTER*1  HPCOL(7)
      INTEGER      IER, I0, J0, I1, J1, LASTI, LASTJ, UNIT, N1, N2, IC
      INTEGER      GRHPNC
      INTEGER      GROPTX
      DATA  HPCOL /'A', 'B', 'C', 'D', 'D', 'B', 'C'/
C-----------------------------------------------------------------------
C
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     1     110,120,130,140,150,160,170,180,190,200,
     2     210,220,230), IFUNC
      GOTO 900
C
C--- IFUNC = 1, Return device name.-------------------------------------
C
   10 CHR = DEVICE
      LCHR = LEN(DEVICE)
      RETURN
C
C--- IFUNC = 2, Return physical min and max for plot device, and range
C               of color indices.---------------------------------------
C
   20 RBUF(1) = 0
      RBUF(2) = 16000
      RBUF(3) = 0
      RBUF(4) = 11400
      RBUF(5) = 1
      RBUF(6) = 7
      NBUF = 6
      RETURN
C
C--- IFUNC = 3, Return device resolution. ------------------------------
C
   30 RBUF(1) = 1016.0
      RBUF(2) = 1016.0
      RBUF(3) = 2
      NBUF = 3
      RETURN
C
C--- IFUNC = 4, Return misc device info. -------------------------------
C    (This device is Hardcopy, No cursor, No dashed lines, No area fill,
C    No thick lines)
C
   40 CHR = 'HNNNNNNNNN'
      LCHR = 10
      RETURN
C
C--- IFUNC = 5, Return default file name. ------------------------------
C
   50 CHR = DEFNAM
      LCHR = LEN(DEFNAM)
      RETURN
C
C--- IFUNC = 6, Return default physical size of plot. ------------------
C
   60 RBUF(1) = 0
      RBUF(2) = 13208
      RBUF(3) = 0
      RBUF(4) = 10160
      NBUF = 4
      RETURN
C
C--- IFUNC = 7, Return misc defaults. ----------------------------------
C
   70 RBUF(1) = 10
      NBUF = 1
      RETURN
C
C--- IFUNC = 8, Select plot. -------------------------------------------
C
   80 CONTINUE
      RETURN
C
C--- IFUNC = 9, Open workstation. --------------------------------------
C
   90 CONTINUE
      CALL GRGLUN(UNIT)
      IER = GROPTX(UNIT, CHR(1:LCHR), DEFNAM, 1)
      RBUF(1) = UNIT
      NBUF = 2
      IF (IER.NE.0) THEN
          CALL GRWARN('Cannot open file for HP7221 plot')
          RBUF(2) = 0
          RETURN
      END IF
      RBUF(2) = 1
      LASTI = -1
      LASTJ = -1
      WRITE (UNIT, '(A)') CHAR(27)//'.(~W`o(B2H}vA}~So(B2H}p`}'
      WRITE (UNIT, '(A)') CHAR(27)//'.N0;19:'
      WRITE (UNIT, '(A)') CHAR(27)//'.I128;5;17:'
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
C
  100 CONTINUE
      WRITE (UNIT, '(A)') 'po(B2H}v@'//CHAR(27)//'.)}'
      CLOSE (UNIT)
      CALL GRFLUN(UNIT)
      RETURN
C
C--- IFUNC=11, Begin picture. ------------------------------------------
C
  110 CONTINUE
      WRITE (UNIT,'(A)') 'po(B2H}v@'//CHAR(27)//'.)}'
      WRITE (UNIT,'(A)') CHAR(12)
      WRITE (UNIT,'(A)') CHAR(27)//'.(~W`o(B2H}~So(B2H}p`}'
      WRITE (UNIT,'(A)') CHAR(27)//'.N0;19:'
      WRITE (UNIT,'(A)') CHAR(27)//'.I128;5;17:'
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
C
  120 CONTINUE
      I0 = NINT(RBUF(1))
      J0 = NINT(RBUF(2))
      I1 = NINT(RBUF(3))
      J1 = NINT(RBUF(4))
  121 CONTINUE
      IF ( (I0.EQ.LASTI) .AND. (J0.EQ.LASTJ) ) THEN
C         -- pen down
          GRHPLI(1:1) = 'q'
      ELSE
C         -- pen up
          GRHPLI(1:1) = 'p'
      END IF
C     -- Encode the coordinate into the command string
      CALL GRHP02(I0,J0,GRHPCP,GRHPNC)
      N1 = GRHPNC + 1
      IF  (GRHPNC .GT. 0) GRHPLI(2:N1) = GRHPCP(1:GRHPNC)
      CALL GRHP02(I1,J1,GRHPCP,GRHPNC)
      N2 = GRHPNC + N1
      IF  (GRHPNC .GT. 0) GRHPLI(N1+1:N2) = GRHPCP(1:GRHPNC)
C     -- Write the command string to the plot file
      WRITE (UNIT,'(A)') CHAR(5)//GRHPLI(1:N2)//'}'
      LASTI = I1
      LASTJ = J1
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
C
  130 CONTINUE
      I1 = NINT(RBUF(1))
      J1 = NINT(RBUF(2))
      I0 = I1
      J0 = J1
      GOTO 121
C
C--- IFUNC=14, End picture. --------------------------------------------
C
  140 CONTINUE
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C
  150 CONTINUE
      IC = NINT(RBUF(1))
      IF (IC.LT.1) IC = 1
      IF (IC.GT.7) IC = 1
      WRITE (UNIT,'(A)') 'v'//HPCOL(IC)//'}'
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
C    (Null operation: buffering is not implemented.)
C
  160 CONTINUE
      RETURN
C
C--- IFUNC=17, Read cursor. --------------------------------------------
C    (Not implemented: should not be called.)
C
  170 GOTO 900
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
C    (Null operation: there is no alpha screen.)
C
  180 CONTINUE
      RETURN
C
C--- IFUNC=19, Set line style. -----------------------------------------
C    (Not implemented: should not be called.)
C
  190 GOTO 900
C
C--- IFUNC=20, Polygon fill. -------------------------------------------
C    (Not implemented: should not be called.)
C
  200 GOTO 900
C
C--- IFUNC=21, Set color representation. -------------------------------
C    (Not implemented)
C
  210 CONTINUE
      RETURN
C
C--- IFUNC=22, Set line width. -----------------------------------------
C    (Not implemented: should not be called.)
C
  220 GOTO 900
C
C--- IFUNC=23, Escape. -------------------------------------------------
C
  230 CONTINUE
      WRITE (UNIT, '(A)') CHR(:LCHR)
      LASTI = -1
      RETURN
C-----------------------------------------------------------------------
C Error: unimplemented function.
C
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in HP device driver: '//MSG)
      NBUF = -1
      RETURN
C-----------------------------------------------------------------------
      END

      SUBROUTINE GRHP02(GRHPNX,GRHPNY,GRHPCP,GRHPNC)
C-----------------------------------------------------------------------
C GRPCKG (internal routine, HP7221):
C       This subroutine translates the INTEGER coordinates into the
C       appropriate HP MBP format, and returns them as a single
C       character*5 string.
C       Reference: HP 7221B Graphics Plotter Operating and
C                  Programming Manual, HP Part # 07221-90014
C
C Arguments:    GRHPNX : X coord of point in plotter units (<= 16383)
C               GRHPNY : ditto Y
C               GRHPCP : output character string with coded position
C               GRHPNC : number of bytes used in GRHPNC
C
C (18-July-1983, R. S. Simon)
C-----------------------------------------------------------------------
      INTEGER GRHPNX, GRHPNY, GRHPNC, N, NX1, NX2, NX3, NXR
      INTEGER NY2, NY3, NY4, NY5, NYR, NP1, NP2, NP3, NP4, NP5
      CHARACTER*5 GRHPCP
C
      GRHPNC = 0
      NP1 = 0
      NP2 = 0
      NP3 = 0
      NP4 = 0
      NP5 = 0
C
      IF (GRHPNX.LT.0.OR.GRHPNY.LT.0) THEN
          CALL GRWARN('GRHPNX and/or GRHPNY is <0.  Point not coded.')
          RETURN
      END IF
C
      N = MAX(GRHPNX,GRHPNY)
C
      IF (N.GT.16383) THEN
          CALL GRWARN('GRHPNX and/or GRHPNY too big.  Point not coded.')
          RETURN
      ELSE IF (N .GT. 2047) THEN
          GRHPNC = 5
          NX1 = GRHPNX/1024
          NXR = GRHPNX - 1024 * NX1
          NX2 = NXR/16
          NX3 = NXR - 16 * NX2
          NY3 = GRHPNY/4096
          NYR = GRHPNY - 4096 * NY3
          NY4 = NYR/64
          NY5 = NYR - 64 * NY4
          NP1 = NX1 + 96
          NP2 = NX2
          IF (NP2.LE.31) NP2 = NP2 + 64
          NP3 = NY3 + 4 * NX3
          IF (NP3.LE.31) NP3 = NP3 + 64
          NP4 = NY4
          IF (NP4.LE.31) NP4 = NP4 + 64
          NP5 = NY5
          IF (NP5.LE.31) NP5 = NP5 + 64
      ELSE IF (N .GT. 255) THEN
          GRHPNC = 4
          NX1 = GRHPNX/128
          NXR = GRHPNX - 128 * NX1
          NX2 = NXR/2
          NX3 = NXR - 2 * NX2
          NY3 = GRHPNY/64
          NY4 = GRHPNY - 64 * NY3
          NP1 = NX1 + 96
          NP2 = NX2
          IF (NP2.LE.31) NP2 = NP2 + 64
          NP3 = NY3 + 32 * NX3
          IF (NP3.LE.31) NP3 = NP3 + 64
          NP4 = NY4
          IF (NP4.LE.31) NP4 = NP4 + 64
      ELSE IF (N .GT. 31) THEN
         GRHPNC = 3
          NX1 = GRHPNX/16
          NX2 = GRHPNX - 16 * NX1
          NY2 = GRHPNY/64
          NY3 = GRHPNY - 64 * NY2
          NP1 = NX1 + 96
          NP2 = NY2 + 4 * NX2
          IF (NP2.LE.31) NP2 = NP2 + 64
          NP3 = NY3
          IF (NP3.LE.31) NP3 = NP3 + 64
      ELSE IF (N .GT. 3) THEN
          GRHPNC = 2
          NX1 = GRHPNX/2
          NX2 = GRHPNX - 2 *NX1
          NP1 = NX1 + 96
          NP2 = GRHPNY + 32 * NX2
          IF (NP2.LE.31) NP2 = NP2 + 64
      ELSE IF (N .GE. 0) THEN
          GRHPNC = 1
          NP1 = GRHPNY + 96 + 4 * GRHPNX
      END IF
C
      GRHPCP = CHAR(NP1)//CHAR(NP2)//CHAR(NP3)//CHAR(NP4)//CHAR(NP5)
      RETURN
      END
