#include "config.h"

      SUBROUTINE GKIOCI(IFUNC, NPR, SPR, STRING, NOUT)
*
* Copyright (C) SERC 1986
*
*-----------------------------------------------------------------------
*
* Type of routine:  SYSTEM INTERFACE
* Author:           PJWR
*
      INCLUDE '../include/check.inc'
*
* PURPOSE  OF THE ROUTINE
* -----------------------
*     Handles byte input from an interactive workstation.
*
* MAINTENANCE LOG
* ---------------
*     11/08/86  PJWR  Original UNIX version stabilised.
*     09/07/87  PJWR  Updated error numbers for GKS 7.4.
*     17/06/04  TIMJ  Updated for autoconf system
*
* ARGUMENTS
* ---------
*     IFUNC   INP  Function Code:
*                    KIOEN   Read with echo,  do not purge typeahead.
*                    KIOEP   Read with echo,  purge typeahead.
*                    KIONN   Read without echo,  do not purge typeahead.
*                    KIONP   Read without echo,  purge typeahead.
*     NPR     INP  Number of characters in SPR.
*     SPR     INP  Prompt string.
*     STRING  OUT  Character string input.
*     NOUT    OUT  Number of characters read.
*
      INTEGER IFUNC, NPR
      CHARACTER*(*) SPR, STRING
      INTEGER NOUT
*
* COMMON BLOCK USAGE
* ------------------
*     Read    /GKYWCA/  KWKIX
*             /GKYWKD/  KWCID
*     Modify  /GKYERR/  KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkio.par'
      INCLUDE '../include/gkmc.par'
*
* LOCALS
* ------
*     BYTE    Input byte.
*     ECHO    Echo on/off.
*     PURGE   Purge/don't purge input buffer.
*     FGETC   Fortran library function [see getc(3f)].
*     FPUTC   Fortran library function [see putc(3f)].
*     I       Loop control variable.
*     IOS     Status return for FGETC/FPUTC.
*     INPLUN  Logical unit number for FGETC.
*     OUTLUN  Logical unit number for FPUTC.
*     NL      Constant value for newline (LF).
*
      CHARACTER BYTE
      LOGICAL ECHO, PURGE
      INTEGER FGETC, FPUTC, I, IOS, INPLUN, OUTLUN, NL
      PARAMETER(NL = 10)
#if HAVE_INTRINSIC_FGETC
      INTRINSIC FGETC
#else
      EXTERNAL FGETC
#endif
#if HAVE_INTRINSIC_FPUTC
      INTRINSIC FPUTC
#else
      EXTERNAL FPUTC
#endif
*
* ERRORS
* ------
*    302   Input/Output error has occurred while reading
*    303   Input/Output error has occurred while writing
*
* COMMENTS
* --------
*    Uses the routine GKTSET(INPLUN, ECHO, PURGE) to access the ioctl(2)
*    system call.
*
*-----------------------------------------------------------------------

*     Evaluate LUN for input.

      INPLUN = KWCID(KWKIX)

*     Set up workstation according to IFUNC.

      ECHO = IFUNC.EQ.KIOEN.OR.IFUNC.EQ.KIOEP
      PURGE = IFUNC.EQ.KIOEP.OR.IFUNC.EQ.KIONP
      CALL GKTSET(INPLUN, ECHO, PURGE)

*     Output prompt if there is one.

      IF (NPR.GT.0) THEN

*       Obtain LUN for output.

	OUTLUN = INPLUN + 1

*       Send the prompt

	DO 10, I = 1, NPR
	  IOS = FPUTC(OUTLUN, SPR(I:I))
	  IF (IOS.NE.0) THEN
	    KERROR = 303
	    GOTO 30
	  ENDIF
10      CONTINUE
#if HAVE_INTRINSIC_FLUSH || HAVE_FLUSH
	CALL FLUSH(OUTLUN)
#else
  error 'Do not know how to flush output buffer'
#endif
      ENDIF

*     Collect the input,  using a WHILE loop.

*     Prime the loop ...

      NOUT = 0
      IOS = FGETC(INPLUN, BYTE)
      IF (IOS.NE.0) THEN
	KERROR = 302
	GOTO 30
      ENDIF

*     ... main loop; WHILE NOT(condition) DO ...

20    CONTINUE
      IF (BYTE.EQ.CHAR(NL)) GOTO 30
	NOUT = NOUT + 1
	STRING(NOUT:NOUT) = BYTE
	IOS = FGETC(INPLUN, BYTE)
	IF (IOS.NE.0) THEN
	  KERROR = 302
	  GOTO 30
	ENDIF
	GOTO 20

*     Reconfigure workstation (echo on,  no purge).

30    CONTINUE
      CALL GKTSET(INPLUN, .TRUE., .FALSE.)

*     Return

      RETURN

      END
