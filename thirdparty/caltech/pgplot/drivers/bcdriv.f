      SUBROUTINE BCDRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
      INTEGER   IFUNC, NBUF, LCHR
      REAL      RBUF(*)
      CHARACTER CHR*(*)
C
C PGPLOT driver for Canon Laser printer.
C---
C This driver produces a bitmap that then can be printed on the
C Canon.  The default size is 1556 blocks and takes 5 min
C (parallel) or 15 min (serial 9600 baud) to print.  Thus for
C simple line graphs CADRIVER produces much smaller files
C (typically <100 blocks) that that plot in <30 sec.  However, for
C complex graphs, for example those obtained with PGGRAY, BCDRIVER
C will produce the smaller file and plot faster.  Therefore, it is
C suggested that sites with Canon laser printers should support
C both drivers.
C---
C Supported device:  Canon LBP-8/A2 Laser printer.
C   Conforms to ISO646,2022,2375 and 6429 specifications.
C   VDM (graphics) conforms to proposed American National
C   Standard VDM mode.
C
C Device type code:  /BCanon (landscape mode only).
C
C Default file name:  PGPLOT.CAN
C
C Default view surface dimensions:  24 cm by 19 cm.
C Resolution:  300 pixels per inch in both directions.
C
C Color capability:  Color indices 0 (erase) and 1 (black) are
C   supported.  Note, hardware polygon fill is used and colors
C   0-11 control the fill pattern.
C
C Input capability:  None.
C
C File format:  Variable length records with Carriage control
C   of LIST.
C
C Obtaining hardcopy:  If printer is connected to a terminal
C   line (RS-232 option) then printing the file on the corresponding
C   queue should suffice.  If the printer is connected using
C   the Centronics interface that appears the to VAX as an
C   LP device then it is important to ensure that (1) all 8 bit
C   characters are passed to the printer (2) lines longer than
C   132 bytes are not truncated, and (3) no extra formatting
C   commands (e.g. form-feeds) are sent to the printer.
C   This can be done with the VMS command:
C   $ SET PRINT/PASSALL/LOWER/CR <device>
C   Note, some interface boards have a option to append a carriage
C   return after a formfeed or LF character, it is necessary
C   that this be disabled.
C   The file should be printed with the /PASSALL qualifier i.e.,
C   $ PRINT/PASSALL <filename>
C   Note, SET PRINT/PASSALL and PRINT/PASSALL do not do the
C   same things and hence PASSALL is required in both locations.
C---
C 13-Mar-1987 - [AFT].
C  4-MAR-1988 - Tidy code [AFT]
C-----------------------------------------------------------------------
      CHARACTER*(*) TYPE
      PARAMETER (TYPE=
     :     'BCANON (Canon laser printer, bitmap mode, landscape)')
      CHARACTER MSG*10
      INTEGER   GRGMEM, GRFMEM
      INTEGER   LUN, IXDIM, IYDIM, LENBUF, IBADR, IER
      INTEGER   ICOL, IREC, LENOLD
      SAVE      LUN, IXDIM, IYDIM, LENBUF, IBADR, IER
      SAVE      ICOL, IREC, LENOLD
      DATA LENOLD/0/
C---
      GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     >       110,120,130,140,150,160,900,180) IFUNC
  900 WRITE (MSG,'(I10)') IFUNC
      CALL GRWARN('Unimplemented function in BC device driver: '//MSG)
      NBUF=-1
      RETURN
C
C--- IFUNC= 1, Return device name. -------------------------------------
   10 CHR=TYPE
      LCHR=LEN(TYPE)
      RETURN
C
C--- IFUNC= 2, Return Physical min and max for plot device. ------------
   20 RBUF(1)=0
      RBUF(2)=3320
      RBUF(3)=0
      RBUF(4)=2335
      RBUF(5)=0
      RBUF(6)=1
      NBUF=6
      RETURN
C
C--- IFUNC= 3, Return device resolution. -------------------------------
   30 RBUF(1)=300.0
      RBUF(2)=300.0
      RBUF(3)=1
      NBUF=3
      LCHR=LEN(CHR)
      RETURN
C
C--- IFUNC= 4, Return misc device info. --------------------------------
C H= Hardcopy
C N= No cursor
C N= No hard dash
C N= No area fill
C N= No hard thick lines
   40 CHR='HNNNNNNNNN'
      LCHR=10
      RETURN
C
C--- IFUNC= 5, Return default file name. -------------------------------
   50 CHR='PGPLOT.CAN'
      LCHR=LEN(CHR)
      RETURN
C
C--- IFUNC= 6, Return default physical size of plot. -------------------
   60 RBUF(1)=0
      RBUF(2)=2834
      RBUF(3)=0
      RBUF(4)=2244
      RETURN
C
C--- IFUNC= 7, Return misc defaults. -----------------------------------
   70 RBUF(1)=1
      NBUF=1
      RETURN
C
C--- IFUNC= 8, Select plot. --------------------------------------------
   80 RETURN
C
C--- IFUNC= 9, Open workstation. ---------------------------------------
   90 CALL GRGLUN(LUN)
      OPEN(UNIT=LUN,FILE=CHR(:LCHR),STATUS='NEW',ACCESS='DIRECT',
     >    RECL=128,IOSTAT=IER)
      IF(IER.NE.0) THEN
         CALL GRWARN('Cannot open graphics device '//CHR(:LCHR))
         RBUF(1)=LUN
         RBUF(2)=0
         CALL GRFLUN(LUN)
      ELSE
         RBUF(1)=0
         RBUF(2)=1
      END IF
      IREC=0
      RETURN
C
C--- IFUNC=10, Close workstation. --------------------------------------
  100 CLOSE(UNIT=LUN)
      CALL GRFLUN(LUN)
      IF(LENOLD.GT.0) THEN
         IER=GRFMEM(LENBUF, IBADR)
         IF(IER.NE.1) THEN
            CALL GRGMSG(IER)
            CALL GRWARN('Failed to deallocate plot buffer.')
            RETURN
         END IF
         LENOLD=0
      END IF
      RETURN
C
C--- IFUNC=11, Begin Picture. ------------------------------------------
C- Note, IXMIN=0 and IXMAX=RBUF(1) so, IXDIM=IXMAX-IXMIN+1=RBUF(1)+1
  110 IXDIM=RBUF(1)+1
      IYDIM=INT(RBUF(2)/8.)+1
      LENBUF=IXDIM*IYDIM
      IF(LENBUF.NE.LENOLD) THEN
         IF(LENOLD.GT.0) THEN
            IER=GRFMEM(LENOLD, IBADR)
            IF(IER.NE.1) THEN
               CALL GRGMSG(IER)
               CALL GRWARN('Failed to deallocate plot buffer.')
               RETURN
            END IF
         END IF
         IER=GRGMEM(LENBUF, IBADR)
         IF(IER.NE.1) THEN
            CALL GRGMSG(IER)
            CALL GRWARN('Failed to allocate plot buffer.')
            RETURN
         END IF
         CALL GRBC05(LENBUF,%val(IBADR))
         LENOLD=LENBUF
      END IF
      RETURN
C
C--- IFUNC=12, Draw line. ----------------------------------------------
  120 CALL GRBC01(1,RBUF,ICOL,IYDIM,%val(IBADR))
      RETURN
C
C--- IFUNC=13, Draw dot. -----------------------------------------------
  130 CALL GRBC01(0,RBUF,ICOL,IYDIM,%val(IBADR))
      RETURN
C
C--- IFUNC=14, End Picture. --------------------------------------------
  140 CALL GRBC04(LUN,IREC,%val(IBADR),IXDIM,IYDIM)
      RETURN
C
C--- IFUNC=15, Select color index. -------------------------------------
C- Save pen number (up to 11) for possible use in pattern interior.
  150 ICOL=MAX(0,MIN(NINT(RBUF(1)),11))
      RBUF(1)=MAX(0,MIN(ICOL,1))
      RETURN
C
C--- IFUNC=16, Flush buffer. -------------------------------------------
  160 RETURN
C
C--- IFUNC=18, Erase alpha screen. -------------------------------------
  180 RETURN
C-----------------------------------------------------------------------
      END

      SUBROUTINE GRBC01(LINE,RBUF,ICOL,IXDIM,QXYBUF)
C-----------------------------------------------------------------------
C GRPCKG (internal routine, Bitmap Canon): draw a (visible)
C straight line segment from absolute pixel coordinates
C (RBUF(1),RBUF(2)) to (RBUF(3),RBUF(4)).  The line either overwrites
C (sets to black) or erases (sets to white) the previous contents
C of the bitmap, depending on the current color index. Setting bits
C is accomplished with a VMS BISB2 instruction, expressed in
C Fortran as .OR.; clearing bits is accomplished with a VMS BICB2
C instruction, expressed in Fortran as .AND..NOT.. The line is
C generated with a Simple Digital Differential Analyser (ref:
C Newman & Sproull). This routine is called by basic plotting
C routines GRDOT0 and GRLIN2.
C
C Arguments:
C
C LINE            I   I  =0 for dot, =1 for line.
C RBUF(1),RBUF(2) I   R  Starting point of line.
C RBUF(3),RBUF(4) I   R  End point of line.
C IXDIM           I   I  First array dimension of the frame buffer
C                        -considered as a two-dimensional byte array.
C QXYBUF          I/O B  (address of) the frame buffer.
C
C 13-Mar-1987 - Copied from GRVE01 routine for use with GEDRIVER [AFT].
C-----------------------------------------------------------------------
      BYTE    QXYBUF(0:*), QMASK(0:7)
      REAL    RBUF(4)
      INTEGER LINE, ICOL, IXDIM, LENGTH, KX, KY, K
      REAL    D, XINC, YINC, XP, YP
      DATA    QMASK /'80'x,'40'x,'20'x,'10'x,'08'x,'04'x,'02'x,'01'x/
C
      IF(LINE.GT.0) THEN
        LENGTH=NINT(MAX(ABS(RBUF(3)-RBUF(1)),ABS(RBUF(4)-RBUF(2))))
        D=MAX(1,LENGTH)
        XINC=(RBUF(3)-RBUF(1))/D
        YINC=(RBUF(4)-RBUF(2))/D
      ELSE
         LENGTH=0
         XINC=0.
         YINC=0.
      END IF
      XP=RBUF(1)
      YP=RBUF(2)
      IF (ICOL.GT.0) THEN
         DO K=0,LENGTH
            KX=NINT(XP)
            KY=NINT(YP)
            QXYBUF(KX*IXDIM+KY/8)=
     >       QXYBUF(KX*IXDIM+KY/8).OR.QMASK(MOD(KY,8))
            XP=XP+XINC
            YP=YP+YINC
         END DO
      ELSE
         DO K=0,LENGTH
            KX=NINT(XP)
            KY=NINT(YP)
            QXYBUF(KX*IXDIM+KY/8)=
     >       QXYBUF(KX*IXDIM+KY/8).AND.(.NOT.QMASK(MOD(KY,8)))
            XP=XP+XINC
            YP=YP+YINC
         END DO
      END IF
      RETURN
      END

      SUBROUTINE GRBC04(LUN,IREC,QBUF,IXDIM,IYDIM)
C-----------------------------------------------------------------------
C GRPCKG internal routine for Canon laser printer.
C Dumps bitmap to file, clearing bitmap.
C-----------------------------------------------------------------------
      INTEGER    IESC,    ICSI
      PARAMETER (IESC=27, ICSI=155)
      INTEGER   LUN, IREC, IXDIM, IYDIM
      BYTE      QBUF(*)
      INTEGER   LENBUF, ITMP, I, JC
      CHARACTER CREC*512, CTMP*16
      BYTE      QREC(512)
      EQUIVALENCE (QREC,CREC)
C---
      JC=0
C- First record, reset printer, set to ISO mode.
      CREC(JC+1:JC+6)=CHAR(IESC)//';'//CHAR(IESC)//'c'//
     :    CHAR(IESC)//';'
      JC=JC+6
      LENBUF=IXDIM*IYDIM
C- Work out absolute vertical position.
      ITMP=1+(3320-IXDIM)/100
      ITMP=MIN(ITMP,5)
      IF(ITMP.GT.1) THEN
         CREC(JC+1:JC+1)=CHAR(ICSI)
         JC=JC+1
         WRITE(CTMP,101) ITMP
  101    FORMAT(I16)
         DO I=1,LEN(CTMP)
            IF(CTMP(I:I).NE.' ') THEN
               JC=JC+1
               CREC(JC:JC)=CTMP(I:I)
            END IF
         END DO
         CREC(JC+1:JC+1)='d'
         JC=JC+1
      END IF
C- Work out absolute horizontal position.
      ITMP=1+(2400-IYDIM*8)/60
      ITMP=MIN(ITMP,4)
      IF(ITMP.GT.1) THEN
         CREC(JC+1:JC+1)=CHAR(ICSI)
         JC=JC+1
         WRITE(CTMP,101) ITMP
         DO I=1,LEN(CTMP)
            IF(CTMP(I:I).NE.' ') THEN
               JC=JC+1
               CREC(JC:JC)=CTMP(I:I)
            END IF
         END DO
         CREC(JC+1:JC+1)='`'
         JC=JC+1
      END IF
C- Drawing box command.
      CREC(JC+1:JC+1)=CHAR(ICSI)
      JC=JC+1
      WRITE(CTMP,101) LENBUF
      DO I=1,LEN(CTMP)
         IF(CTMP(I:I).NE.' ') THEN
            JC=JC+1
            CREC(JC:JC)=CTMP(I:I)
         END IF
      END DO
      JC=JC+1
      CREC(JC:JC)=';'
      WRITE(CTMP,101) IYDIM
      DO I=1,LEN(CTMP)
         IF(CTMP(I:I).NE.' ') THEN
            JC=JC+1
            CREC(JC:JC)=CTMP(I:I)
         END IF
      END DO
      CREC(JC+1:JC+6)=';300.r'
      JC=JC+6
C- Send binary data.
      DO I=1,LENBUF
         JC=JC+1
         QREC(JC)=QBUF(I)
         QBUF(I)=0
         IF(JC.EQ.512) THEN
            IREC=IREC+1
            WRITE(LUN,REC=IREC) QREC
            JC=0
         END IF
      END DO
C- Dump last record, if necessary.
      IF(JC.NE.0) THEN
         DO I=JC+1,512
            QREC(I)=0
         END DO
         IREC=IREC+1
         WRITE(LUN,REC=IREC) QREC
      END IF
      RETURN
      END

C*GRBC05 -- zero fill buffer
C+
      SUBROUTINE GRBC05 (BUFSIZ,BUFFER)
C
C GRPCKG (internal routine): fill a buffer with a given character.
C
C Arguments:
C
C BUFFER (byte array, input): (address of) the buffer.
C BUFSIZ (integer, input): number of bytes in BUFFER.
C--
C (1-Feb-1983)
C-----------------------------------------------------------------------
      INTEGER  BUFSIZ, I
      BYTE     BUFFER(BUFSIZ), FILL
      DATA     FILL /0/
C
      DO 10 I=1,BUFSIZ
          BUFFER(I) = FILL
   10 CONTINUE
      END
