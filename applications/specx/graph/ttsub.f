*  History:
*     12 Mar 1992 (jfl):
*        Starlink PGPLOT version.
*     22 Nov 1993 (hme):
*        Remove some routines to avoid VMS RTL.
*        Put the () into PARAMETER statements.
*     25 Nov 1993 (hme):
*        Attempt to disuse TTBUFOUT from TTWRITE or TTFLUSH.
*        Attempt to implement TTCLOSE as a dummy routine.
*     15 Feb 1995 (rpt)
*        Device names changed/added for use with generic PGPLOT
C----------------------------------------------------------------------------
C STARLINK PGPLOT version (REVAD::JFL) March 12th, 1992.

      SUBROUTINE TTWRITE(NCHAR,BUF)

C   Write NCHAR characters from BUF to the output buffer. If buffer is full
C   then put it out to terminal.

      PARAMETER (NBUF=256)
      BYTE BUF(1)
      BYTE OUTBUF(NBUF)
      INTEGER*4 IPOINT /0/

      IF(NCHAR+IPOINT.GT.NBUF) THEN
*        CALL TTBUFOUT(IPOINT,OUTBUF)
        WRITE (6,'(A)') OUTBUF
        IPOINT = 0
      ENDIF

      DO 10 J = 1,NCHAR
   10 OUTBUF(J+IPOINT) = BUF(J)
      IPOINT = IPOINT + NCHAR

      RETURN

      ENTRY TTFLUSH
C     -------------
C   Flush the output buffer

*      CALL TTBUFOUT(IPOINT,OUTBUF)
      WRITE (6,'(A)') OUTBUF
      IPOINT = 0

      RETURN
      END

C----------------------------------------------------------------------------

      SUBROUTINE TTCLOSE

C   Disconnect the terminal from the channel

*      INCLUDE '($IODEF)'
*      INCLUDE '($SSDEF)'

*      INTEGER*4 SYS$DASSGN, TTCHN, RETCOD
*      INTEGER*2 IOSB(4)
*      COMMON /TT/ TTCHN, RETCOD, IOSB
	
*      CALL TIDLE
*      CALL SYS$DASSGN(%VAL(TTCHN))
**     WRITE (6,*) 'Channel ',TTCHN,' deassigned'

      RETURN
      END
*
C----------------------------------------------------------------------------
*
*      SUBROUTINE TTBUFOUT(NCHAR,OUTBUF)
*
*C   Put NCHAR characters from the output buffer OUTBUF to the terminal
*
*      INCLUDE '($IODEF)'
*      INCLUDE '($SSDEF)'
*
*      INTEGER*4 CHARBUF(2)
*      BYTE OUTBUF(1)
*
*      INTEGER*4   TTCHN, SYS$QIOW, RETCOD
*      INTEGER*2   IOSB(4)
*      COMMON /TT/ TTCHN, RETCOD, IOSB
*
*      IF(NCHAR.EQ.0) RETURN
*
*C  Do the write 
*
*      RETCOD = SYS$QIOW(,%VAL(TTCHN),
*     &                   %VAL(IO$_WRITEVBLK.OR.IO$M_NOFORMAT),
*     &                   IOSB,,,%REF(OUTBUF),%VAL(NCHAR),,,,)
*
*      IF(RETCOD.NE.SS$_NORMAL) THEN
*        WRITE(6,*) ' TT QIO return code error'
*        CALL ERRMSG(RETCOD)
*        RETURN
*      ENDIF
*
*      IF(IOSB(1).NE.SS$_NORMAL) THEN
*        WRITE(6,*) ' TT error'
*        CALL ERRMSG(IOSB(1))
*        RETURN
*      ENDIF
*
*      RETURN
*      END
*
C----------------------------------------------------------------------------
*
*      SUBROUTINE ERRMSG(ICODE)
*
*C   Crude way to get system to print an error message.
*
*      CHARACTER*256 MSGBUF
*      CALL SYS$GETMSG(%VAL(ICODE),MSGLEN,%DESCR(MSGBUF),%VAL(15),)
*      WRITE(6,*) MSGBUF(1:MSGLEN)
*
*      RETURN
*      END
*
C----------------------------------------------------------------------------
*
*      SUBROUTINE TT_ASSIGN (TT_ERROR)
*
*C   Routine to assign a channel to a terminal.
*
*      INCLUDE '($SSDEF)'
*
*      LOGICAL     TT_ERROR
*      INTEGER     TTCHN, RETCOD
*      INTEGER*2   IOSB(4)
*      COMMON /TT/ TTCHN, RETCOD, IOSB
*
*      INTEGER*4   SYS$ASSIGN, SYS$QIOW
*
*C   Assign a channel to the terminal
*
*      TT_ERROR = .FALSE.
*
*      RETCOD = SYS$ASSIGN('TERMNAME',TTCHN,,)
*      IF(RETCOD.NE.SS$_NORMAL) THEN
*        WRITE(6,*) ' Problem with channel assignment'
*        TT_ERROR = .TRUE.
*        CALL ERRMSG(RETCOD)
*        RETURN
*      ELSE
**       WRITE (6,*) 'TERMNAME assigned to channel ',TTCHN
*      ENDIF
*
*      RETURN
*      END
*
*-----------------------------------------------------------------------

      SUBROUTINE TTGOTEK

*  Routine to change terminals like VT220 to Tek4010 emulator from
*  "native" terminal mode.

      IMPLICIT  NONE

*  Ok, go...

      BYTE T4010_INIT(6)  /27,'[','?','3','8','h'/

*     Include files:

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

C--------------------
C  Send change-mode sequence and flush graphics buffer
C--------------------

      IF ((IDEV.EQ.10) .OR. (IDEV.EQ.11)) THEN
        CALL TTWRITE (6,T4010_INIT)
        CALL TTFLUSH
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE TTGO220

*  Routine to return terminals like VT220 from Tek4010 emulator to
*  "native" terminal mode.

      IMPLICIT  NONE

*  Ok, go...

      BYTE VT240_EXIT(6)   /27,'[','?','3','8','l'/

*     Include files:

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

C--------------------
C  Send change-mode sequence and flush graphics buffer
C--------------------

      IF ((IDEV.EQ.1) .OR. (IDEV.EQ.9)) THEN
        CALL TTWRITE (6,VT240_EXIT)
        CALL TTFLUSH
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE TIDLE

*  Routine to flush graphics buffer and return dual screen alpha/graphics
*  device to alpha mode if in graphics mode.

      IMPLICIT  NONE

*  Ok, go...

      BYTE FF /12/, EC /27/, GS /29/, US /31/

      BYTE PERICOM_IDLE(1) /24/
      BYTE CIFERT5_IDLE(6) /27,'[','?','3','8','l'/
      BYTE GRAPHON_IDLE(1) /24/
      BYTE SG220_IDLE(6)   /27,'2',27,'[','4','i'/
      BYTE RETRO_IDLE(3)   /29,32,24/
      BYTE SELANAR_IDLE(2) /27,'2'/
      BYTE VT240_IDLE(1)   /31/

*     Include files:

      INCLUDE  'SXGPGPLOT.INC'

*  Ok, go...

C--------------------
C  Flush the graphics buffer so terminal can accept alpha characters
C--------------------

      IF (IDEV .EQ. 1) THEN
        CALL TTWRITE (2,SELANAR_IDLE)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 2) THEN
        CALL TTWRITE (3,RETRO_IDLE)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 3) THEN
        CALL TTWRITE (6,CIFERT5_IDLE)        
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 4) THEN
        CALL TTWRITE (1,GRAPHON_IDLE)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 5) THEN
        CALL TTWRITE (6,SG220_IDLE)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 6) THEN
        CALL TTWRITE (1,PERICOM_IDLE)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 7) THEN
        CALL TTWRITE (2,SELANAR_IDLE)
        CALL TTFLUSH
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE TTGRAPH

*  Routine to return dual screen alpha/graphics terminal to graphics
*  mode if now in alpha

      IMPLICIT  NONE

*     Include files

      INCLUDE  'SXGPGPLOT.INC'

*     Local variables:

      BYTE GS                /29/
      BYTE CIFERT5_GRAPH(6)  /27,'[','?','3','8','h'/
      BYTE SG220_GRAPH(6)    /27,'[','5','i',27,'1'/

*  Ok, go...

      IF (IDEV .EQ. 1) THEN
        CALL TTWRITE (1,GS)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 2) THEN
        CALL TTWRITE (1,GS)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 3) THEN
        CALL TTWRITE (6, CIFERT5_GRAPH)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 4) THEN
        CALL TTWRITE (1,GS)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 5) THEN
        CALL TTWRITE (6, SG220_GRAPH)
        CALL TTWRITE (1,GS)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 6) THEN
        CALL TTWRITE (1,GS)
        CALL TTFLUSH
      ELSE IF (IDEV .EQ. 7) THEN
        CALL TTWRITE (1,GS)
        CALL TTFLUSH
      END IF

      RETURN
      END

*-----------------------------------------------------------------------

