      SUBROUTINE GKFOBO(IFUNC, NINTA, INTA, NLEFT)

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
*     Handles byte output with a fixed length buffer to a file.
*
* MAINTENANCE LOG
* ---------------
*     03/08/87  TAW   Original version stabilized.
*                     Based on gkiobo() for GKS 7.4.
*     21/03/88  PLP   Adapted for Primos, with the following alterations:
*                     Fortran WRITEs used for buffer ouput; additional
*                     error checking introduced so as to avoid buffer
*                     overshooting; file's record length restricted
*                     to that required by the system printing utilities.
*     24/08/88  RMK   Changed KIOPB entry to send end bytes with buffer.
*                     Changed variable NINT to NINTA to avoid confusion.
*
* ARGUMENTS
* ---------
*     INP  IFUNC  Function Code:
*                    KIOIT   Initialise buffers.
*                    KIOBB   Set up start buffer (beginning of CMBUFF).
*                    KIOEB   Set up end buffer (CEBUFF).
*                    KIOPB   Put string in CMBUFF, first send buffer if
*                            not enough room.
*                    KIOSN   Send CMBUFF and CEBUFF.
*                    KIOQS   Return amount of space left in CMBUFF.
*                    KIOER   Unused (see GKIOFO()).
*                    KIOSO   Unused in file output.
*     INP  NINTA   Number of integers in INTA.
*     INP  INTA    Array of integers containing ASCII codes.
*     OUT  NLEFT   Number of bytes available in buffer.
*
      INTEGER IFUNC, NINTA, INTA(NINTA), NLEFT
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
*     I       Loop control integer.
*     IOS     Status return for the WRITE to file operation
*     LINLTH  Maximum Line Length acceptable.
*     LUN     Output logical unit number.
*     MAXBUF  Maximum Buffer Length, excluding the End of Buffer bytes.
*     FORMT   Character variable to hold formats.
*
      INTEGER I, IOS, LINLTH, LUN, MAXBUF
      PARAMETER ( LINLTH=132 )
      CHARACTER*10 FORMT
*
* ERRORS
* ------
*     303   Input/Output error has occured while writing
*   -2004   Documented condition to be satisfied by parameter(s)
*           of internal routine is not satisfied
*
*---------------------------------------------------------------------

* Initialise locals.

      LUN = KWCID(KWKIX)
      MAXBUF = MIN(LINLTH,KBUF) - KEB(KWKIX)

* Initialise buffers.

      IF (IFUNC.EQ.KIOIT) THEN
         KB(KWKIX)  = 0
         KBB(KWKIX) = 0
         KEB(KWKIX) = 0

* Set up start of CMBUFF.

      ELSE IF (IFUNC.EQ.KIOBB) THEN
*        Check that string is not too long
         IF(NINTA.LE.MAXBUF)THEN
*           OK - set the buffer
            DO 10, I = 1, NINTA
               CMBUFF(KWKIX)(I:I) = CHAR(INTA(I))
   10       CONTINUE
            KB(KWKIX)  = NINTA
            KBB(KWKIX) = NINTA
         ELSE
*           Too long - set error message and return
            KERROR=-2004
            RETURN
         ENDIF

* Set up CEBUFF.

      ELSE IF (IFUNC.EQ.KIOEB) THEN
*        Check that string is not too long
         IF(NINTA.LE.KEND)THEN
*           OK - set the buffer
            DO 20, I = 1, NINTA
               CEBUFF(KWKIX)(I:I) = CHAR(INTA(I))
   20       CONTINUE
            KEB(KWKIX) = NINTA
         ELSE
*           Too long - set error message and return
            KERROR=-2004
            RETURN
         ENDIF

* Buffer INTA, sending CMBUFF and CEBUFF first if necessary.

      ELSE IF (IFUNC.EQ.KIOPB) THEN
*        Check that string is not too long to fit in a complete buffer
         IF(KBB(KWKIX)+NINTA.LE.MAXBUF)THEN
*           OK - put data into the buffer
*           But check whether there is room in the current buffer
            IF ((KB(KWKIX) + NINTA).GT.MAXBUF) THEN
*              Include the end buffer if one is defined
               IF(KEB(KWKIX).NE.0)THEN
*                 Set the (Axxx,Axx) format up
                  WRITE(FORMT, '(A,I3,A,I2,A)') '(A', KB(KWKIX), ',A',
     :                                            KEB(KWKIX), ')'
*                 And write both the buffer and the end buffer.
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
            DO 40, I = KB(KWKIX) + 1, KB(KWKIX) + NINTA
               CMBUFF(KWKIX)(I:I) = CHAR(INTA(I - KB(KWKIX)))
   40       CONTINUE
            KB(KWKIX) = KB(KWKIX) + NINTA
         ELSE
*           String too long to fit in a complete buffer - give error
            KERROR=-2004
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
