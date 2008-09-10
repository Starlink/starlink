#include "config.h"

      SUBROUTINE GKIOBO(IFUNC, NINT, INTA, NLEFT)
*
* Copyright (C) SERC 1986
*
*---------------------------------------------------------------------
*
* Type of routine:  SYSTEM INTERFACE
* Author:           PJWR
*
      INCLUDE '../include/check.inc'
*
* PURPOSE OF THE ROUTINE
* ----------------------
*     Handles byte output with a fixed length buffer.
*
* MAINTENANCE LOG
* ---------------
*     07/08/86  PJWR  Original UNIX version stabilised.
*     09/07/87  PJWR  Updated error numbers for GKS 7.4.
*     17/06/04  TIMJ  Updated for autoconf system
*
* ARGUMENTS
* ---------
*     IFUNC   INP  Function Code:
*                    KIOIT   Initialise buffers.
*                    KIOBB   Set up start buffer (beginning of CMBUFF).
*                    KIOEB   Set up end buffer (CEBUFF).
*                    KIOPB   Put string in CMBUFF and send if full.
*                    KIOSN   Send CMBUFF and CEBUFF.
*                    KIOQS   Return amount of space left in CMBUFF.
*                    KIOER   Unused (see GKIOFO()).
*                    KIOSO   Send CMBUFF without appending CEBUFF.
*     NINT    INP  Number of integers in INTA.
*     INTA    INP  Array of integers containing ASCII codes.
*     NLEFT   OUT  Number of bytes available in buffer.
*
      INTEGER IFUNC, NINT, INTA(NINT), NLEFT
*
* COMMON BLOCK USAGE
* ------------------
*     Read    /GKYWCA/  KWKIX
*             /GKYWKD/  KWCID
*             /GKYWCB/  KWKC
*     Modify  /GKYIO/   <All variables>
*             /GKZIO/   <All variables>
*             /GKYERR/  KERROR
*             /GKZXIO/  ISWKT
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkio.par'
      INCLUDE '../include/gkio.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkxio.cmn'
*
* LOCALS
* ------
*     FPUTC   Fortran library function [see putc(3f)].
*     I       Loop control integer.
*     IOS     Status return for FPUTC().
*     LUN     Output logical unit number.
*
      INTEGER FPUTC, I, IOS, LUN
#if HAVE_INTRINSIC_FPUTC
      INTRINSIC FPUTC
#else
      EXTERNAL FPUTC
#endif
*
* ERRORS
* ------
*     303   Input/Output error has occured while writing
*
*---------------------------------------------------------------------

* Initialise buffers.

      IF (IFUNC.EQ.KIOIT) THEN
	KB(KWKIX) = 0
	KBB(KWKIX) = 0
	KEB(KWKIX) = 0

* Set up start of CMBUFF.

      ELSE IF (IFUNC.EQ.KIOBB) THEN
	DO 10, I = 1, NINT
	  CMBUFF(KWKIX)(I:I) = CHAR(INTA(I))
10      CONTINUE
	KB(KWKIX) = NINT
	KBB(KWKIX) = NINT

* Set up CEBUFF.

      ELSE IF (IFUNC.EQ.KIOEB) THEN
	DO 20, I = 1, NINT
	  CEBUFF(KWKIX)(I:I) = CHAR(INTA(I))
20      CONTINUE
	KEB(KWKIX) = NINT

* Buffer INTA,  sending CMBUFF first if necessary.

      ELSE IF (IFUNC.EQ.KIOPB) THEN
	IF ((KB(KWKIX) + NINT).GT.KBUF) THEN
	  LUN = KWCID(KWKIX)
	  IF (ISWKT(KWKIX)) THEN
	    LUN = LUN + 1
	  ENDIF
	  DO 30, I = 1, KB(KWKIX)
	    IOS = FPUTC(LUN, CMBUFF(KWKIX)(I:I))
	    IF (IOS.NE.0) GOTO 999
30        CONTINUE
	  KB(KWKIX) = KBB(KWKIX)
	ENDIF
	DO 40, I = KB(KWKIX) + 1, KB(KWKIX) + NINT
	  CMBUFF(KWKIX)(I:I) = CHAR(INTA(I - KB(KWKIX)))
40      CONTINUE
	KB(KWKIX) = KB(KWKIX) + NINT

*     Send buffer immediately ...

      ELSE IF (IFUNC.EQ.KIOSN.OR.IFUNC.EQ.KIOSO) THEN
	LUN = KWCID(KWKIX)
	IF (ISWKT(KWKIX)) THEN
	  LUN = LUN + 1
	ENDIF
	DO 50, I = 1, KB(KWKIX)
	  IOS = FPUTC(LUN, CMBUFF(KWKIX)(I:I))
	  IF (IOS.NE.0) GOTO 999
50      CONTINUE

*       ... including end buffer.

	IF (IFUNC.EQ.KIOSN) THEN
	  DO 60, I = 1, KEB(KWKIX)
	    IOS = FPUTC(LUN, CEBUFF(KWKIX)(I:I))
	    IF (IOS.NE.0) GOTO 999
60        CONTINUE
	ENDIF
	KB(KWKIX) = KBB(KWKIX)

*       ... then flush the unit.
#if HAVE_INTRINSIC_FLUSH || HAVE_FLUSH
	CALL FLUSH(LUN)
#else
 error 'Do not know how to flush output buffer'
#endif
      ENDIF

*     Normal Return - give available buffer space.

      NLEFT = KBUF - KB(KWKIX)

      RETURN

*     Error Return

999   CONTINUE
      KERROR = 303

      RETURN

      END
