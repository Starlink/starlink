	SUBROUTINE GODRIV(IFUNC,RBUF,NBUF,CHR,LCHR)
C- GRPCKG driver for GOC(Sigma) terminal.
C---
C Supported device:  Sigma, T5670 terminal.
C
C Device type code: /GOC
C
C Default file name: TT(logical name, usually equivalent to the
C   logged in terminal).
C
C Default view surface dimensions:  38 cm display.
C
C Resolution: The full view surface is 768 by 512 pixels.
C
C Color capability: Color indices 0 (erase) and 1 are supported.
C
C Input capability: Cursor is a cross-hair and can be moved using
C   the joystick or the cursor keys to the left of the keyboard.
C   Terminate cursor motion and send the cursor position to the
C   program by typing any printable character on the keyboard.
C
C File format:  It is not possible to send GOC plots to a disk file.
C
C Obtaining hardcopy: A hardcopy of the plot may be obtained using
C   a Tekronix hardcopy unit attached to the terminal.
C
C  5-Aug-1986 - [AFT].
C-----------------------------------------------------------------------
	CHARACTER  GOCERA*(*)
	PARAMETER (GOCERA='BH'//CHAR(12)//'+-*/')
	INTEGER   IFUNC,NBUF,LCHR,I0,J0,I1,J1
	REAL      RBUF(6)
	CHARACTER CHR*(*)
	INTEGER   GRGE00
        CHARACTER GOCSTR*2, GOCERG*2
	DATA GOCERG/'DA'/
C
	INTEGER LUN,MXCNT,ICNT,IBADR,ICOL
	SAVE    LUN,MXCNT,ICNT,IBADR,ICOL
	LOGICAL   APPEND
	SAVE      APPEND
C---
	GOTO( 10, 20, 30, 40, 50, 60, 70, 80, 90,100,
     &       110,120,130,140,150,160,170,180) IFUNC
	GOTO 999
C---
C- IFUNC= 1, Return device name.
10	CHR='GOC'
	LCHR=LEN(CHR)
	RETURN
C---
C- IFUNC= 2, Return Physical min and max for plot device.
20	RBUF(1)=0
	RBUF(2)=768
	RBUF(3)=0
	RBUF(4)=511
	RBUF(5)=0
	RBUF(6)=1
	NBUF=6
	RETURN
C---
C- IFUNC= 3, Return device(X and Y) resolution in pixels per inch as
C- formatted numbers in CHR.
30	RBUF(1)=76.8
	RBUF(2)=76.8
	RBUF(3)=1
	NBUF=3
	RETURN
C---
C- IFUNC= 4, Return misc device info.
40	CHR='ICNNNNNNNN'
	LCHR=10
	RETURN
C---
C- IFUNC= 5, Return default file name.
50	CHR='TT'
	LCHR=LEN(CHR)
	NBUF=1
	RETURN
C---
C- IFUNC= 6, Return default physical size of plot.
60	RBUF(1)=0
	RBUF(2)=768
	RBUF(3)=0
	RBUF(4)=511
	RETURN
C---
C- IFUNC= 7, Return misc defaults.
70	RBUF(1)=2
	NBUF=1
	RETURN
C---
C- IFUNC= 8, Set active plot.
80	CALL INGO03(LUN)
	RETURN
C---
C- IFUNC= 9, Open workstation.
90	APPEND=RBUF(3).NE.0.0
	RBUF(2)=GRGE00('Q1 ',LUN,CHR,LCHR)
	RBUF(1)=LUN
	IF(RBUF(2).EQ.1) THEN
	  CALL INGO03(LUN)
	  MXCNT=130
	  CALL GRGMEM(MXCNT,IBADR)
	END IF
	RETURN
C---
C- IFUNC=10, Close workstation.
100	CALL GRFMEM(MXCNT,IBADR)
	RETURN
C---
C- IFUNC=11, Begin Picture.
110	CALL GRGO10
	IF(.NOT.APPEND) THEN
	  CALL GRGO02(%REF(GOCERG),LEN(GOCERG),%val(IBADR),ICNT,MXCNT)
	END IF
	APPEND=.FALSE.
	RETURN
C---
C- IFUNC=12, Draw line.
120	I0=NINT(RBUF(1))
	J0=NINT(RBUF(2))
	I1=NINT(RBUF(3))
	J1=NINT(RBUF(4))
	CALL GRGO01(I0,J0,I1,J1,%val(IBADR),ICNT,MXCNT)
	RETURN
C---
C- IFUNC=13, Draw dot.
130	I0=NINT(RBUF(1))
	J0=NINT(RBUF(2))
	CALL GRGO01(I0,J0,I0,J0,%val(IBADR),ICNT,MXCNT)
	RETURN
C---
C- IFUNC=14, End Picture.
140	RETURN
C---
C- IFUNC=15, Select pen.
150	RBUF(1) = MAX(0,MIN(NINT(RBUF(1)),1))
	ICOL=RBUF(1)
	IF(ICOL.EQ.0) THEN
	    GOCSTR='FB'
	ELSE
	    GOCSTR='FA'
	END IF
	CALL GRGO02(%REF(GOCSTR),LEN(GOCSTR),%VAL(IBADR),ICNT,MXCNT)
	RETURN
C---
C- IFUNC=16, Flush buffer.
160	CALL GRGO03(%val(IBADR),ICNT)
	RETURN
C---
C- IFUNC=17, Make cursor visible and read position.
170	I0=RBUF(1)
	J0=RBUF(2)
	CALL GRGO11(LUN,I0,J0,CHR)
	RBUF(1)=I0
	RBUF(2)=J0
	NBUF=2
	LCHR=1
	RETURN
C---
C- IFUNC=18, Erase alpha screen.
180	CALL GRGO03(%val(IBADR),ICNT)
	CALL GRGO02(%REF(GOCERA),LEN(GOCERA),%VAL(IBADR),ICNT,MXCNT)
	CALL GRGO03(%val(IBADR),ICNT)
	CALL INGO01
	RETURN
C---
C- Flag function not implemented.
999	NBUF=-1
	RETURN
C---
	END
     
	SUBROUTINE GRGO01(X0,Y0,X1,Y1,IBUF,ICNT,MXCNT)
C-----------------------------------------------------------------------
C GRPCKG(GOC) : draw a line from(X0,Y0) to(X1,Y1)
C
C( 4-Feb-1986 KS / AAO)
C-----------------------------------------------------------------------
	INTEGER   X0, Y0, X1, Y1, IBUF, ICNT, MXCNT
	INTEGER   IGNORE, PTR
	CHARACTER STRING*22
	INTEGER   LSTI,LSTJ
	SAVE      LSTI,LSTJ
C---
	IF(X0.NE.LSTI .OR. Y0.NE.LSTJ) THEN
	    STRING(1:2) = 'GI'          ! Pen up
            WRITE(STRING(3:8),'(2I3.3)',IOSTAT=IGNORE) X0+100,Y0+100
            STRING(9:10) = 'GJ'	       ! Pen down
	    PTR=11
	ELSE
	    PTR=1
	END IF
	WRITE(STRING(PTR:PTR+5),'(2I3.3)',IOSTAT=IGNORE)
     1						X1+100, Y1+100
	CALL GRGO02(%REF(STRING),PTR+5,IBUF,ICNT,MXCNT)
	LSTI = X1
	LSTJ = Y1
	RETURN
C---
	ENTRY INGO01
	LSTI=-1
	LSTJ=-1
	RETURN
C---
	END
C*********
	SUBROUTINE GRGO02(INSTR,N,QBUF,ICNT,MXCNT)
C-----------------------------------------------------------------------
C GRPCKG(GOC) : transfer N bytes to output QBUF
C from INSTR(1...N).  QBUF has to be passed since it is a
C dynamic array.  N should be less than 32, and the routine
C always ensures that at least 32 bytes are left spare in the
C buffer.
C
C( 4-Feb-1986 KS / AAO)
C-----------------------------------------------------------------------
C
	INTEGER N,ICNT,MXCNT
	BYTE    INSTR(N)
	BYTE    QBUF(*)
C---
	INTEGER   I
	CHARACTER INIT*6
	DATA INIT/'+-*/GJ'/
C
	IF(ICNT.EQ.0) THEN
	    DO I=1,LEN(INIT)
		QBUF(I) = ICHAR(INIT(I:I))
	    END DO
	    ICNT = LEN(INIT)
	END IF
	DO I=1,N
	    ICNT = ICNT+1
	    QBUF(ICNT) = INSTR(I)
	END DO
	IF(ICNT+32 .GE. MXCNT) CALL GRGO03(QBUF,ICNT)
	RETURN
	END
C*********
	SUBROUTINE GRGO03(QBUF,ICNT)
C-----------------------------------------------------------------------
C GRPCKG(GOC): flush buffer contents.  QBUF is passed as
C an argument because it is a dynamic array.
C
C( 4-Feb-1986 KS / AAO )
C-----------------------------------------------------------------------
	INCLUDE '($IODEF)'
	INCLUDE '($SSDEF)'
	INTEGER   SYS$QIOW
C
	INTEGER   ICNT,INLUN
	BYTE      QBUF(*)
C
	INTEGER   WRITE
	PARAMETER(WRITE=IO$_WRITEVBLK+IO$M_NOFORMAT+IO$M_CANCTRLO)
	INTEGER   RESULT, N
	INTEGER   IOSB(2)
	INTEGER*2 STBC(2), STATUS, COUNT
	INTEGER   LUN
	SAVE      LUN
C---
	EQUIVALENCE(STBC, IOSB(1)),(STATUS, STBC(1)),(COUNT, STBC(2))
C
C	Insert 'return to alpha mode' code into end of buffer.
C
	N = ICNT
	IF(N.LT.1) RETURN
	QBUF(N+1)=ICHAR('B')
	QBUF(N+2)=ICHAR('H')
	N=N+2
	CALL INGO01
	ICNT = 0
C
C	Now flush buffer
C
	RESULT = SYS$QIOW(,%VAL(LUN),%VAL(WRITE),IOSB,,
     1			  ,QBUF,%VAL(N),,,,)
	IF(RESULT.NE.SS$_NORMAL) THEN
	    CALL GRGMSG(RESULT)
	    CALL GRQUIT('SYS$QIOW failure writing to GOC terminal')
	END IF
	IF((STATUS.NE.SS$_NORMAL) .AND.
     1	   (STATUS.NE.SS$_CONTROLO) .AND.
     2	   (STATUS.NE.SS$_CONTROLY) .AND.
     3	   (STATUS.NE.SS$_CONTROLC)) THEN
	    RESULT = STATUS
	    CALL GRGMSG(RESULT)
	    CALL GRQUIT('SYS$QIOW failure writing to GOC terminal')
	END IF
	RETURN
C---
	ENTRY INGO03(INLUN)
	LUN=INLUN
	RETURN
C---
	END
C*********
	SUBROUTINE GRGO10
C--------------------------------------------------------------------
C GRPCKG(GOC) Initialise the terminal.  Note that the display
C is NOT erased.
C
C Initialisation sequence contains -
C
C Graphics flag sequence
C Allow any length graphics sequences
C Graphics cursor off
C Graphics display on
C Alpha display on
C Disable cursor prompt character.
C Set full screen cursor
C Recognise Graphics flag sequence any time in alpha mode
C Return to alpha mode   (appended by GRGO03)
C
C Note the way the GOC modes are controlled.  Assuming that the GOC is in
C reset or alpha mode initially, the initialisation sequence will leave it
C in alpha mode.  Outside the writing of GRPCKG buffers to the GOC, it is
C left in alpha mode.  Whenever a new buffer is started, it begins with
C the graphics flag sequence and a pen down code(inserted by GRGO02).
C Before a buffer is output, it is always terminated by a 'return to alpha
C mode' command(inserted by GRGO03).  Routines - such as GRCURS - that
C have to write directly to the GOC should always flush out the buffer
C first(using GRTERM) and make sure that they leave the GOC in alpha mode.
C
C( 4-Feb-1986 KS / AAO )
C--------------------------------------------------------------------
	CHARACTER  CR, LF
	PARAMETER (CR=CHAR(13),LF=CHAR(10))
C
	INTEGER   ICNT
	CHARACTER GCINIT*27
	DATA GCINIT/'  +-*/DD000CBGAAAHE999CK1GH'/
C---
	GCINIT(1:2)=CR//LF
	ICNT=LEN(GCINIT)
	CALL GRGO03(%ref(GCINIT),ICNT)
C
 	END
C*********
	SUBROUTINE GRGO11(LUN,IX,IY,CH)
C
C	GOC terminals: method is similar to that for Tektronix,
C	except that the control strings and reply are ASCII formatted
C	strings.
C
	INCLUDE  '($IODEF)'
	INCLUDE  '($SSDEF)'
	INTEGER    RPR
	PARAMETER (RPR=IO$_READPROMPT+IO$M_NOFORMAT+
     1			IO$M_PURGE+IO$M_NOECHO+IO$M_TIMED)
	CHARACTER  CR
	PARAMETER (CR=CHAR(13))
	INTEGER    WRT
	PARAMETER (WRT=IO$_WRITEVBLK+IO$M_NOFORMAT)
C
	INTEGER   LUN, IX, IY
	CHARACTER CH
	INTEGER   IOSB(2),RESULT,IER, SYS$QIOW
        CHARACTER GCREP*9, GCROMP*20, GCEND*11
	DATA      GCROMP/'+-*/GICECAxxxyyyCFHF'/
	DATA      GCEND/'+-*/CECBGJBH'/
C---
	WRITE(GCROMP(11:16),'(I3.3,I3.3)',IOSTAT=IER)
     1						IX+100,IY+100
	IX=LEN(GCROMP)
	RESULT = SYS$QIOW(,%VAL(LUN),%VAL(RPR),IOSB,
     1		,,%REF(GCREP),%VAL(9),%VAL(60),,
     2		%REF(GCROMP),%VAL(LEN(GCROMP)))
	IF(RESULT.NE.SS$_NORMAL) THEN
	    CALL GRGMSG(RESULT)
	    CALL GRQUIT('SYS$QIOW error reading from GOC terminal')
	END IF
	IF(GCREP(8:8).EQ.CR) THEN
	    CH = CHAR(0)
	    READ(GCREP,'(I3,1X,I3)',IOSTAT=IER) IX,IY
	ELSE
	    CH = GCREP(1:1)
	    READ(GCREP,'(1X,I3,1X,I3)',IOSTAT=IER) IX,IY
	END IF
	RESULT = SYS$QIOW(,%VAL(LUN),%VAL(WRT),IOSB,
     1			,,%REF(GCEND),%VAL(LEN(GCEND)),,,,)
	IF(RESULT.NE.SS$_NORMAL) THEN
	    CALL GRGMSG(RESULT)
	    CALL GRQUIT('SYS$QIOW error writing to GOC terminal')
	END IF
	CALL INGO01
	RETURN
	END
