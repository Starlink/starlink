#include "config.h"

      SUBROUTINE GKIOFO(IFUNC, NINT, INTA, NLEFT)
*
* (C) COPYRIGHT SERC 1987
*
*-----------------------------------------------------------------------
*
*  RAL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             DRJF / PJWR
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Puts characters into records which are grouped together in an
*     internal buffer and output when the buffer is full.
*
*  MAINTENANCE LOG
*  ---------------
*     04/03/87  PJWR  Pyramid version 2 adapted from PR1ME version
*                     by DRJF.
*     09/07/87  PJWR  Updated error codes for GKS 7.4.
*     17/06/04  TIMJ  Autoconf version
*
*  ARGUMENTS
*  ---------
*     IFUNC   INP  Function Code:
*                    KIOIT   Initialise buffers.
*                    KIOBB   Set up start buffer (BBUFF).
*                    KIOEB   Set up end buffer (EBUFF).
*                    KIOPB   Put bytes in buffer creating a new record
*                            if required.
*                    KIOSN   Flush the buffer.
*                    KIOQS   Return amount of space left in record.
*                    KIOER   Force end of record.
*                    KIOSO   Unused (see GKIOBO() and GKIOCO()).
*     NINT    INP  Number of integers in INTA.
*     INTA    INP  Array of integers containing bytes to output.
*     NLEFT   OUT  Number of bytes available in buffer.
*
*
      INTEGER IFUNC, NINT, INTA(NINT), NLEFT
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  EXTERNAL FUNCTIONS
*  ------------------
#if HAVE_INTRINSIC_AND
*     AND     Bitwise AND.
      INTRINSIC AND
#elif HAVE_INTRINSIC_IAND
*     IAND     Bitwise AND.
      INTRINSIC IAND
#endif

#if HAVE_INTRINSIC_OR
*     OR      Bitwise OR.
      INTRINSIC OR
#elif HAVE_INTRINSIC_IOR
*     IOR     Bitwise OR.
      INTRINSIC IOR
#endif

#if HAVE_INTRINSIC_ISHFT
*     ISHFT   Shift left (>0) or right (<0)
      INTRINSIC ISHFT
#else
#if HAVE_INTRINSIC_LSHIFT
*     LSHIFT  Left shift.
      INTRINSIC LSHIFT
#elif HAVE_INTRINSIC_SHIFTL
*     SHIFTL  Left shift.
      INTRINSIC SHIFTL
#endif

#if HAVE_INTRINSIC_RSHIFT
*     RSHIFT  Right shift.
      INTRINSIC RSHIFT
#elif HAVE_INTRINSIC_SHIFTR
*     SHIFTR  Right shift.
      INTRINSIC SHIFTR
#endif
#endif
*
*     FPUTC   Character output as per C fputc().
      INTEGER FPUTC
#if HAVE_INTRINSIC_FPUTC
      INTRINSIC FPUTC
#else
      EXTERNAL FPUTC
#endif
*
*  LOCALS
*  ------
*     IBUFSP  Length of main data buffer.
*     IRBGIN  Length of start record buffer.
*     IREND   Length of end record buffer.
*     IBC     Main buffer cursor.
*     IRC     Record cursor.
*     IRL     Record length (set by caller).
*     IBRC    Start record length.
*     IERC    End record length.
*     IMBUFF  Main data buffers.
*     IBBUFF  Start record buffers.
*     IEBUFF  End record buffers.
*     I       Loop counter.
*     LUN     Local copy of output stream.
*     IOS     For return value from FPUTC().
*     ENDREC  .TRUE. if we are at the end of a record.
*
      INTEGER    IBUFSP,     IRBGIN,   IREND
      PARAMETER (IBUFSP=256, IRBGIN=6, IREND=4)

      INTEGER IBC(1:KWK),  IRC(1:KWK), IRL(1:KWK),
     :        IBRC(1:KWK), IERC(1:KWK)

      SAVE IBC, IRC, IRL, IBRC, IERC

      INTEGER*2 IMBUFF(IBUFSP,KWK)
      INTEGER*2 IBBUFF(IRBGIN,KWK)
      INTEGER*2 IEBUFF(IREND,KWK)

      SAVE IMBUFF, IEBUFF, IBBUFF

      INTEGER I, LUN, IOS
      INTEGER II
      LOGICAL ENDREC
*
*  ALGORITHM
*  ---------
*     Characters are put into records whose length is determined by  the
*     calling routine at initialisation. If there is not enough room  in
*     a record or if the calling routine explicitly ends a record, a new
*     record is started. The routine incorporates a 'begin  record'  and
*     'end record', that can be set by the calling routine.  Thereafter,
*     every time a record is started or  finished  the  same  data  will
*     appear at the  beginning  and  end  of  the  record.  Records  are
*     constructed until the buffer is full (at which point the buffer is
*     output) or the buffer is forced out.  If a record is incomplete in
*     either situation it is continued over into the  new  buffer.  When
*     there is no more data to be put in the buffer, the calling routine
*     must call the routine to end the buffer and again to force out the
*     buffer.
*
*  ERRORS
*  ------
*     303 Input/Output error has occurred while writing
*
*  COMMENTS
*  --------
*     The buffers are stored in local SAVEd variables.  As in the  PR1ME
*     version,  the function code is interpreted as an  absolute  value,
*     so this routine is dependant on the PARAMETER values in gkio.par.
*     Half words are used because the PR1ME expects this.
*
*-----------------------------------------------------------------------

      ENDREC=.FALSE.
      GOTO (10,20,30,40,50,200,70) IFUNC
      GOTO 999
*
*     Initialise
*
   10 CONTINUE
      IBC(KWKIX)=0
      IRC(KWKIX)=0
      IRL(KWKIX)=INTA(1)/2
      IBRC(KWKIX)=0
      IERC(KWKIX)=0
      GOTO 200
*
*     Set up begin record
*
   20 CONTINUE
      IBRC(KWKIX)=0
      DO 22 I=1,NINT,2
        IBRC(KWKIX)=IBRC(KWKIX)+1
        IBBUFF(IBRC(KWKIX),KWKIX)=
#if HAVE_INTRINSIC_OR || HAVE_OR
     :       OR(
#elif HAVE_INTRINSIC_IOR || HAVE_IOR
     :       IOR(
#endif
#if HAVE_INTRINSIC_LSHIFT || HAVE_LSHIFT
     :       LSHIFT(INTA(I),8),INTA(I+1))
#elif HAVE_INTRINSIC_SHIFTL || HAVE_SHIFTL
     :       SHIFTL(INTA(I),8),INTA(I+1))
#elif HAVE_INTRINSIC_ISHFT || HAVE_ISHFT
     :       ISHFT(INTA(I),8),INTA(I+1))
#endif
   22 CONTINUE
      DO 25 I=1,IBRC(KWKIX)
        IMBUFF(I,KWKIX)=IBBUFF(I,KWKIX)
   25 CONTINUE
      IBC(KWKIX)=IBRC(KWKIX)
      IRC(KWKIX)=IBRC(KWKIX)
      GOTO 200
*
*     Set up end record
*
   30 CONTINUE
      IERC(KWKIX) = 0
      DO 32 I=1,NINT,2
        IERC(KWKIX)=IERC(KWKIX)+1
        IEBUFF(IERC(KWKIX),KWKIX)=
#if HAVE_INTRINSIC_OR || HAVE_OR
     :       OR(
#elif HAVE_INTRINSIC_IOR || HAVE_IOR
     :       IOR(
#endif
#if HAVE_INTRINSIC_LSHIFT || HAVE_LSHIFT
     :       LSHIFT(INTA(I),8), INTA(I+1))
#elif HAVE_INTRINSIC_SHIFTL || HAVE_SHIFTL
     :       SHIFTL(INTA(I),8), INTA(I+1))
#elif HAVE_INTRINSIC_ISHFT || HAVE_ISHFT
     :       ISHFT(INTA(I),8), INTA(I+1))
#endif

   32 CONTINUE
      IRL(KWKIX)=IRL(KWKIX)-IERC(KWKIX)
      GOTO 200
*
*     Put bytes in record. If there isn't enough room in the record then
*     start a new one,  flushing IMBUFF if necessary.
*
   40 CONTINUE
      ENDREC=IRC(KWKIX)+NINT/2 .GT. IRL(KWKIX)
      IF ( ENDREC ) THEN
        IF (IBC(KWKIX)+NINT/2+IERC(KWKIX)+IBRC(KWKIX).GT.IBUFSP)
     :     GOTO 100
      ELSE
        IF (IBC(KWKIX)+NINT/2.GT.IBUFSP) GOTO 100
      END IF
      GOTO 175
*
*     Force the buffer
*
   50 CONTINUE
      IF (IBC(KWKIX).GT.0) GOTO 100
      GOTO 200
*
*     End record entry
*
   70 CONTINUE
      ENDREC=.TRUE.
      IF (IBC(KWKIX)+IERC(KWKIX)+IBRC(KWKIX).LE.IBUFSP) GOTO 175
*
*     Output buffer
*
  100 CONTINUE
      LUN = KWCID(KWKIX)
      DO 125, I = 1, IBC(KWKIX)
        II = IMBUFF(I,KWKIX) ! RS6000 systems want intermediate variable
#if HAVE_INTRINSIC_RSHIFT || HAVE_RSHIFT
        IOS = FPUTC(LUN, CHAR(RSHIFT(II, 8)))
#elif HAVE_INTRINSIC_SHIFTR || HAVE_SHIFTR
        IOS = FPUTC(LUN, CHAR(SHIFTR(II, 8)))
#elif HAVE_INTRINSIC_ISHFT || HAVE_ISHFT
        IOS = FPUTC(LUN, CHAR(ISHFT(II, -8)))
#endif
	IF (IOS.NE.0) THEN
	  KERROR = 303
	  GOTO 999
	ENDIF
#if HAVE_INTRINSIC_AND || HAVE_AND
	IOS = FPUTC(LUN, CHAR(AND(II, 255)))
#elif HAVE_INTRINSIC_IAND || HAVE_IAND
        IOS = FPUTC(LUN, CHAR(IAND(II, 255)))
#endif
	IF (IOS.NE.0) THEN
	  KERROR = 303
	  GOTO 999
	ENDIF
  125 CONTINUE
      IBC(KWKIX)=0
*
*     End record and start new one
*
  175 CONTINUE
      IF ( ENDREC ) THEN
*
*       Add end record if necessary
*
        IF (IERC(KWKIX).GT.0) THEN
          DO 102 I=1,IERC(KWKIX)
            IMBUFF(IBC(KWKIX)+I,KWKIX)=IEBUFF(I,KWKIX)
  102     CONTINUE
          IBC(KWKIX)=IBC(KWKIX)+IERC(KWKIX)
        ENDIF
        IRC(KWKIX)=0
*
*       Add start record if necessary
*
        IF (IBRC(KWKIX).GT.0) THEN
          DO 120 I=1,IBRC(KWKIX)
            IMBUFF(IBC(KWKIX)+I,KWKIX)=IBBUFF(I,KWKIX)
  120     CONTINUE
          IBC(KWKIX)=IBC(KWKIX)+IBRC(KWKIX)
          IRC(KWKIX)=IBRC(KWKIX)
        END IF
      END IF
*
*     Now put new bytes in cleared buffer depending on IFUNC
*
      IF (IFUNC.NE.4) GO TO 200
*
*       Pack the bytes into the buffer of 16 bit half words,  retaining
*       byte order.
*
      DO 142 I=1,NINT,2
        IBC(KWKIX)=IBC(KWKIX)+1
        IRC(KWKIX)=IRC(KWKIX)+1
        IMBUFF(IBC(KWKIX),KWKIX)=
#if HAVE_INTRINSIC_OR || HAVE_OR
     :       OR(
#elif HAVE_INTRINSIC_IOR || HAVE_IOR
     :       IOR(
#endif
#if HAVE_INTRINSIC_LSHIFT || HAVE_LSHIFT
     :       LSHIFT(INTA(I),8), INTA(I+1))
#elif HAVE_INTRINSIC_SHIFTL || HAVE_SHIFTL
     :       SHIFTR(INTA(I),8), INTA(I+1))
#elif HAVE_INTRINSIC_ISHFT || HAVE_ISHFT
     :       ISHFT(INTA(I),8), INTA(I+1))
#endif
  142 CONTINUE
*
*     Set number of bytes remaining in record
*
  200 NLEFT=(IRL(KWKIX)-IRC(KWKIX))*2
*
*     Finish
*
  999 CONTINUE
      RETURN
      END
