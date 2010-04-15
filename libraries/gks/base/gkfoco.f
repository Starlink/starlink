      SUBROUTINE GKFOCO(IFUNC, STRING, NLEFT)
*
* Copyright (C) SERC 1987
*
*---------------------------------------------------------------------
*
* Type of routine:  SYSTEM INTERFACE
* Author:           TAW
*
      INCLUDE '../include/check.inc'
*
* PURPOSE OF THE ROUTINE
* ----------------------
*     Handles character output with a fixed length buffer to a file.
*
* MAINTENANCE LOG
* ---------------
*     03/08/87  TAW   Original version stabilized.
*                     Based on gkioco() for GKS 7.4.
*     21/03/88  PLP   Adapted for Primos, with the following alterations:
*                     Fortran WRITEs used for buffer ouput; additional
*                     error checking introduced so as to avoid buffer
*                     overshooting; file's record length restricted
*                     to that required by the system printing utilities.
*     22/08/88  RMK   Changed KIOPB entry to send end bytes with buffer.
*                     Removed unused local variable and label.
*     12/03/91  PLP   Introduced calls to GKBUG for error -2004.
*
* ARGUMENTS
* ---------
*     INP  IFUNC   Function Code:
*                    KIOIT   Initialise buffers.
*                    KIOBB   Set up start buffer (beginning of CMBUFF).
*                    KIOEB   Set up end buffer (CEBUFF).
*                    KIOPB   Put string in CMBUFF, first send buffer if
*                            not enough room.
*                    KIOSN   Send CMBUFF and CEBUFF.
*                    KIOQS   Return amount of space left in CMBUFF.
*                    KIOER   Unused (see GKIOFO()).
*                    KIOSO   Unused.
*     INP  STRING  String of characters to be output.
*     OUT  NLEFT   Number of bytes available in buffer.
*
      INTEGER IFUNC
      CHARACTER*(*) STRING
      INTEGER NLEFT
*
* COMMON BLOCK USAGE
* ------------------
*     Read    /GKYWCA/  KWKIX
*             /GKYWKD/  KWCID
*             /GKYWCB/  KWKC
*     Modify  /GKYIO/   <All variables>
*             /GKZIO/   <All variables>
*             /GKYERR/  KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkio.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkio.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkwkd.cmn'
*
*  INTRINSIC FUNCTIONS
*  -------------------
*
      INTRINSIC MIN
*
* LOCALS
* ------
*     IOS     Status return for the WRITE to file operation
*     LENGTH  Length of STRING.
*     LINLTH  Maximum Line Length acceptable.
*     LUN     Output logical unit number.
*     MAXBUF  Maximum Buffer Length, excluding the End of Buffer bytes.
*     FORMT   Character variable to hold formats.
*
      INTEGER IOS, LENGTH, LINLTH, LUN, MAXBUF
      PARAMETER ( LINLTH=132 )
      CHARACTER*10 FORMT
*
* ERRORS
* ------
*     303   Input/Output error has occured while writing
*   -2004   Documented condition to be satisfied by parameter(s)
*           of internal routine is not satisfied. Used for buffer
*           overflow
*
*---------------------------------------------------------------------

* Initialise the locals.

      LUN=KWCID(KWKIX)
      LENGTH=LEN(STRING)
      MAXBUF = MIN(LINLTH,KBUF) - KEB(KWKIX)

* Initialise buffers.

      IF (IFUNC.EQ.KIOIT) THEN
         KB(KWKIX)  = 0
         KBB(KWKIX) = 0
         KEB(KWKIX) = 0

* Set up start of CMBUFF.

      ELSE IF (IFUNC.EQ.KIOBB) THEN
*        Check that string is not too long
         IF(LENGTH.LE.MAXBUF)THEN
*           OK - set the buffer
            CMBUFF(KWKIX)(1:LENGTH) = STRING(1:LENGTH)
            KB(KWKIX)  = LENGTH
            KBB(KWKIX) = LENGTH
         ELSE
*           Too long - set error message and return
            CALL GKBUG (-2004,'GKFOCO')
            RETURN
         ENDIF

* Set up CEBUFF.

      ELSE IF (IFUNC.EQ.KIOEB) THEN
*        Check that string is not too long
         IF(LENGTH.LE.KEND)THEN
*           OK - set the buffer
            CEBUFF(KWKIX)(1:LENGTH) = STRING(1:LENGTH)
            KEB(KWKIX) = LENGTH
         ELSE
*           Too long - set error message and return
            CALL GKBUG (-2004,'GKFOCO')
           RETURN
         ENDIF

* Buffer STRING, sending CMBUFF and CEBUFF first if necessary.

      ELSE IF (IFUNC.EQ.KIOPB) THEN
*        Check that string is not too long to fit in a complete buffer
         IF(KBB(KWKIX)+LENGTH.LE.MAXBUF)THEN
*           OK - put data into the buffer
*           But check whether there is room in the current buffer
            IF ((KB(KWKIX) + LENGTH).GT.MAXBUF) THEN
*              Include the end buffer if one is defined
               IF (KEB(KWKIX) .NE. 0) THEN
*                 Set the (Axxx,Axx) format up
                  WRITE(FORMT, '(A,I3,A,I2,A)') '(A', KB(KWKIX), ',A',
     :                                          KEB(KWKIX), ')'
*                 And output both the buffer and the end buffer
                  WRITE(LUN,FORMT,IOSTAT=IOS)CMBUFF(KWKIX),CEBUFF(KWKIX)
               ELSE
*                 End buffer is empty, so just write the buffer itself
*                 Set the (Axxx) format up
                  WRITE(FORMT,'(A,I3,A)') '(A', KB(KWKIX), ')'
*                 And output the buffer
                  WRITE(LUN, FORMT, IOSTAT=IOS) CMBUFF(KWKIX)
               ENDIF
               IF (IOS.NE.0) GOTO 999
               KB(KWKIX) = KBB(KWKIX)
            ENDIF
*           Put the data into the buffer
            CMBUFF(KWKIX)(KB(KWKIX)+1:KB(KWKIX)+LENGTH)=STRING(1:LENGTH)
            KB(KWKIX) = KB(KWKIX) + LENGTH
         ELSE
*           String too long to fit in a complete buffer - give error
            CALL GKBUG (-2004,'GKFOCO')
            RETURN
         ENDIF

*     Send buffer immediately (don't bother if there's no useful data)

      ELSE IF (IFUNC.EQ.KIOSN.AND.KB(KWKIX).GT.KBB(KWKIX)) THEN
*        Include the end buffer if one is defined
         IF(KEB(KWKIX).NE.0)THEN
*           Set the (Axxx,Axx) format up
            WRITE(FORMT, '(A,I3,A,I2,A)') '(A', KB(KWKIX), ',A',
     :                                      KEB(KWKIX), ')'
*           And output both the buffer and the end buffer.
            WRITE(LUN,FORMT,IOSTAT=IOS)CMBUFF(KWKIX),CEBUFF(KWKIX)
         ELSE
*           End buffer is empty, so just write the buffer itself
*           Set the (Axxx) format up
            WRITE(FORMT, '(A,I3,A)') '(A', KB(KWKIX), ')'
*           And output the buffer.
            WRITE(LUN,FORMT,IOSTAT=IOS)CMBUFF(KWKIX)
         ENDIF
         IF (IOS.NE.0) GOTO 999
*
         KB(KWKIX) = KBB(KWKIX)

      ENDIF

*     Normal Return - give available buffer space.

      NLEFT = KBUF - KB(KWKIX)

      RETURN

*     Error Return from the WRITE operation

999   CONTINUE
      KERROR = 303

      RETURN

      END
