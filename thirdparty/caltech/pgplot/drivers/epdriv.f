      SUBROUTINE EPDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
C GRPCKG driver for EPSON FX100 dot matrix printer.
C
C    Apr-1987 - Floating-point input version Apr 1987 [PSB].
C 16-Jan-1988 - Compile with /WARN=(DECLARE) switch [AFT].
C---
      CHARACTER ESC, DUAL
      PARAMETER (ESC=CHAR(27))
      PARAMETER (DUAL=CHAR(1))
      REAL       PL,    PL1
      PARAMETER (PL=765,PL1=PL-1)
      REAL      RBUF(6)
      INTEGER   IFUNC, NBUF, LCHR
      CHARACTER CHR*(*)
C
      INTEGER   GRGMEM, GRFMEM, GRTRIM
      INTEGER   XYMAP, LENOLD, IST, IXDIM, IYDIM, LENBUF
      INTEGER   I, J, N, ICOL, LUN
      INTEGER*2 BUF(0:1632)
      CHARACTER NN*2
      SAVE LUN,XYMAP,ICOL,IXDIM,IYDIM,LENOLD,LENBUF
      DATA LENOLD/0/
C---
      GOTO (100,200,300,400,500,600,700,800,900,1000,
     :      1100,1200,1300,1400,1500) IFUNC
      GOTO 999
C
C       1: Return device name:
100   CHR = 'EPSON (Epson dot matrix printer)'
      LCHR = GRTRIM(CHR)
      RETURN
C
C       2: Return physical min & max for device:
200   RBUF(1) = 0
      RBUF(2) = 1631  
C                     ! dual-density 120/"
      RBUF(3) = 0
      RBUF(4) = -1
C                     ! as long as a box of paper...
      RBUF(5) = 0
C                     ! min colour
      RBUF(6) = 1
C                     ! max colour
      NBUF = 6
      RETURN
C
C       3: Return device resolution:
300   RBUF(1) = 120.0
C                     ! horiz dots per inch
      RBUF(2) = 72.0
C                     ! veric dots per inch
      RBUF(3) = 1.0
C                     ! thick lines
      RETURN
C
C       4: Return misc info:
C H= Hardcopy device
C N= No cursor
C N= No hard dash
C N= No area fill
C N= No hard thick lines
400   CHR(1:10) = 'HNNNNNNNNN'
      RETURN
C
C       5: Return default file name:
500   CHR = 'PGPLOT.EPS'
      LCHR = LEN(CHR)
      RETURN
C
C       6: Return default size of plot:
600   RBUF(1) = 0
      RBUF(2) = 1631
      RBUF(3) = 0
      RBUF(4) = PL1
C                     ! 72 ./" -> 11" PAGE.
      RETURN
C
C       7: Return misc defaults:
700   RBUF(1) = 1.
      ICOL = 1
      RETURN
C
C       8: Select Plot:
800   RETURN
C
C       9: Open device:
900   CALL GRGLUN(LUN)
      OPEN(LUN,FILE=CHR(:LCHR),STATUS='NEW',
     1  RECORDTYPE='VARIABLE',RECL=4000)
      RBUF(1) = LUN
      RBUF(2) = 1
      RETURN
C
C       10: Close device:
1000  CLOSE(UNIT=LUN)
      CALL GRFLUN(LUN)
      IF(LENOLD.GT.0) THEN
          IST = GRFMEM(LENOLD,XYMAP)
          IF(IST.NE.1) STOP 'error freeing memory in EPDRIV'
          LENOLD=0
      ENDIF
      RETURN
C
C       11: Initialise plot:
1100  IXDIM = RBUF(1) + 1
      IYDIM = RBUF(2)/9 + 1
      LENBUF = IXDIM*IYDIM*2
C                       ! length of buffer in bytes
      IF(LENBUF.NE.LENOLD) THEN
          IF(LENOLD.GT.0) THEN
            IST = GRFMEM(LENOLD,XYMAP)
            IF(IST.NE.1) STOP 'error freeing memory in EPDRIV'
            LENOLD=0
          ENDIF
          IST = GRGMEM(LENBUF,XYMAP)
          IF(IST.NE.1) STOP 'error allocating memory in EPDRIV'
          LENOLD = LENBUF
      ENDIF
      CALL GREP03(LENBUF,%VAL(XYMAP))
      RETURN
C
C       12: Draw a line:
1200  CALL GREP01(RBUF,ICOL,IXDIM-1,IYDIM-1,%VAL(XYMAP))
      RETURN
C
C       13: Draw a dot:
1300  CALL GREP02(RBUF,ICOL,IXDIM-1,IYDIM-1,%VAL(XYMAP))
      RETURN
C
C       14: Close plot:
1400  CONTINUE
C
C       Initialise printer:
      WRITE(LUN,1411) ESC,'A',CHAR(9)
C                    ! 9 dots per line;
1411  FORMAT(1X,3A1)
      DO 1460 J=0,IYDIM-1
          CALL GREP04(%VAL(XYMAP+IXDIM*J*2),IXDIM,BUF)
C
C         Find last non-zero dot position:
          DO 1430 I=IXDIM-1,0,-1
            N = I + 1
            IF(BUF(I).NE.0) GOTO 1440
1430      CONTINUE
1440      CONTINUE
          NN(1:1) = CHAR(N.AND.255)
          NN(2:2) = CHAR(N/256)
          BUF(N) = '0A0D'X
C                  ! CR LF
          WRITE(LUN,1441) ESC,';',DUAL,NN,(BUF(I),I=0,N)
1441      FORMAT(1X,3A1,A2,1632A2)
1460  CONTINUE
C
C       Reset printer to normal:
      WRITE(LUN,1461)ESC,'2',CHAR(13)
C                  ! 1/6 line spacing
1461  FORMAT(1X,3A1)
      RETURN
C
C       15: Set colour:
1500  ICOL = MAX(MIN(NINT(RBUF(1)),1),0)
C                  ! only black or white.
      RBUF(1) = ICOL
      RETURN
C---
C--- Flag function not implemented.
999   NBUF=-1
      RETURN
      END

      SUBROUTINE GREP01(RBUF,ICOL,IXDIM,IYDIM,XYMAP)
C- Draw a line on Epson:
      REAL      RBUF(6)
      INTEGER   ICOL, IXDIM, IYDIM
      INTEGER*2 XYMAP(0:IXDIM,0:IYDIM)
C
      REAL      XL, YL, D, XP, YP, XINC, YINC
      INTEGER   L, LENGTH, IX, IY, IYBIT
      INTEGER*2 BITS(0:8)
      DATA BITS/128,64,32,16,8,4,2,1,-32768/
C---
      XL = RBUF(3) - RBUF(1)
      YL = RBUF(4) - RBUF(2)
      D = MAX(ABS(XL),ABS(YL),1.0)
      LENGTH = NINT(D)
      XP = RBUF(1)
      YP = RBUF(2)
      XINC = XL/D
      YINC = YL/D
      DO 180 L = 0,LENGTH
          IX = NINT(XP)
          IY = IYDIM*9 - NINT(YP)
          IYBIT = MOD(IY,9)
          IF(ICOL.GT.0) THEN
            XYMAP(IX,IY/9) = 
     :      XYMAP(IX,IY/9).OR.BITS(IYBIT)
          ELSE
            XYMAP(IX,IY/9) = 
     :      XYMAP(IX,IY/9).AND.(.NOT.BITS(IYBIT))
          ENDIF
          XP = XP + XINC
          YP = YP + YINC
180   CONTINUE
      RETURN
      END

      SUBROUTINE GREP02(RBUF,ICOL,IXDIM,IYDIM,XYMAP)
C
C- Draw a dot:
      REAL      RBUF(6)
      INTEGER   ICOL, IXDIM, IYDIM
      INTEGER*2 XYMAP(0:IXDIM,0:IYDIM)
C
      INTEGER   IY, IYBIT
      INTEGER*2 BITS(0:8)
      DATA BITS/128,64,32,16,8,4,2,1,-32768/
C---
      IY = IYDIM*9 - NINT(RBUF(2))
      IYBIT = MOD(IY,9)
      XYMAP(NINT(RBUF(1)),IY/9) = 
     :XYMAP(NINT(RBUF(1)),IY/9).OR.BITS(IYBIT)
      RETURN
      END

      SUBROUTINE GREP03(LENBUF,XYMAP)
C- Erase bitmap
      INTEGER   LENBUF, XYMAP(*)
      INTEGER   I
C---
      DO 180 I=1,LENBUF/4
          XYMAP(I) = 0
180   CONTINUE
      RETURN
      END

      SUBROUTINE GREP04(XYMAP,IXDIM,BUF)
C- Copy a line of output to buf
      INTEGER   IXDIM
      INTEGER*2 XYMAP(IXDIM), BUF(IXDIM)
      INTEGER   I
C---
      DO 180 I=1,IXDIM
          BUF(I) = XYMAP(I)
180   CONTINUE
      RETURN
      END
