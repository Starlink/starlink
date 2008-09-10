#include "config.h"

      SUBROUTINE GKIOCO(IFUNC, STRING, NLEFT)
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
*     Handles character output with a fixed length buffer.
*
* MAINTENANCE LOG
* ---------------
*     07/08/86  PJWR  Original UNIX version stabilised.
*     09/07/87  PJWR  Updated error codes for GKS 7.4.
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
*     STRING  INP  String of characters to be output.
*     NLEFT   OUT  Number of bytes available in buffer.
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
*             /GKZXIO/  ISWKT
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkio.par'
      INCLUDE '../include/gkio.cmn'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkxio.cmn'
*
* LOCALS
* ------
*     FPUTC   Fortran library function [see putc(3f)].
*     I       Loop control integer.
*     IOS     Status return for FPUTC().
*     LENGTH  Length of STRING.
*     LUN     Output logical unit number.
*
      INTEGER FPUTC, I, IOS, LENGTH, LUN
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
	LENGTH = LEN(STRING)
	CMBUFF(KWKIX)(1:LENGTH) = STRING(1:LENGTH)
	KB(KWKIX) = LENGTH
	KBB(KWKIX) = LENGTH

* Set up CEBUFF.

      ELSE IF (IFUNC.EQ.KIOEB) THEN
	LENGTH = LEN(STRING)
	CEBUFF(KWKIX)(1:LENGTH) = STRING(1:LENGTH)
	KEB(KWKIX) = LENGTH

* Buffer STRING,  sending CMBUFF first if necessary.

      ELSE IF (IFUNC.EQ.KIOPB) THEN
	LENGTH = LEN(STRING)
	IF ((KB(KWKIX) + LENGTH).GT.KBUF) THEN
	  LUN = KWCID(KWKIX)
	  IF (ISWKT(KWKIX)) LUN = LUN + 1
	  DO 10, I = 1, KB(KWKIX)
	    IOS = FPUTC(LUN, CMBUFF(KWKIX)(I:I))
	    IF (IOS.NE.0) GOTO 999
10        CONTINUE
	  KB(KWKIX) = KBB(KWKIX)
	ENDIF
	CMBUFF(KWKIX)(KB(KWKIX)+1:KB(KWKIX)+LENGTH) = STRING(1:LENGTH)
	KB(KWKIX) = KB(KWKIX) + LENGTH

*     Send buffer immediately ...

      ELSE IF (IFUNC.EQ.KIOSN.OR.IFUNC.EQ.KIOSO) THEN
	LUN = KWCID(KWKIX)
	IF (ISWKT(KWKIX)) LUN = LUN + 1
	DO 20, I = 1, KB(KWKIX)
	  IOS = FPUTC(LUN, CMBUFF(KWKIX)(I:I))
	  IF (IOS.NE.0) GOTO 999
20      CONTINUE

*       ... including end buffer...

	IF (IFUNC.EQ.KIOSN) THEN
	  DO 30, I = 1, KEB(KWKIX)
	    IOS = FPUTC(LUN, CEBUFF(KWKIX)(I:I))
	    IF (IOS.NE.0) GOTO 999
30        CONTINUE
	ENDIF

*       ... then flush the unit.
#if HAVE_INTRINSIC_FLUSH || HAVE_FLUSH
	CALL FLUSH(LUN)
#else
 error 'Do not know how to flush output buffer'
#endif
	KB(KWKIX) = KBB(KWKIX)

      ENDIF

*     Normal Return - give available buffer space.

      NLEFT = KBUF - KB(KWKIX)

      RETURN

*     Error Return

999   CONTINUE
      KERROR = 303

      RETURN

      END
