#include "config.h"

      SUBROUTINE GKIOBI(IFUNC, NPR, IPR, NINT, INTA, NOUT)
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
*     08/08/86  PJWR  Original UNIX version.
*     09/07/87  PJWR  Updated error numbers for GKS 7.4.
*     17/06/04  TIMJ  Autoconf version
*
* ARGUMENTS
* ---------
*     IFUNC   INP  Function Code:
*                    KIOEN   Read with echo,  do not purge typeahead.
*                    KIOEP   Read with echo,  purge typeahead.
*                    KIONN   Read without echo,  do not purge typeahead.
*                    KIONP   Read without echo,  purge typeahead.
*     NPR     INP  Number of elements to use from IPR.
*     IPR     INP  Prompt array.
*     NINT    INP  Maximum number of elements to use in INTA.
*     INTA    OUT  Array of bytes input, one per element.
*     NOUT    OUT  Number of characters read.
*
      INTEGER IFUNC, NPR, IPR(NPR), NINT, INTA(NINT), NOUT
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
*     PURGE   Purge/don'y purge input buffer.
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
	  IOS = FPUTC(OUTLUN, CHAR(IPR(I)))
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

*     Loop priming ...

      NOUT = 0
      IOS = FGETC(INPLUN, BYTE)
      IF (IOS.NE.0) THEN
	KERROR = 302
	GOTO 30
      ENDIF

*     ... main loop;  WHILE NOT(condition) DO...

20    CONTINUE
      IF (NOUT.EQ.NINT.OR.BYTE.EQ.CHAR(NL)) GOTO 30
	NOUT = NOUT + 1
	INTA(NOUT) = ICHAR(BYTE)
	IOS = FGETC(INPLUN, BYTE)
	IF (IOS.NE.0) THEN
	  KERROR = 302
	  GOTO 30
	ENDIF
	GOTO 20

*     Reconfigure workstation.

30    CONTINUE
      CALL GKTSET(INPLUN, .TRUE., .FALSE.)

*     Return

      RETURN

      END
