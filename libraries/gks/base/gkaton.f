      SUBROUTINE GKATON(NOCHS, IASCII, SCHARS)
*
* Copyright (C) SERC 1986
*
*-----------------------------------------------------------------------
*
* Type of Routine:  SYSTEM INTERFACE
* Author:           PJWR
*
      INCLUDE '../include/check.inc'
*
* PURPOSE OF THE ROUTINE
* ----------------------
*
*     Converts an array of ASCII codes into the corresponding character
*     string.  Codes on the range 32 to 127 (SP to DEL) are recognised.
*     In the event of an unrecognised code being returned,  KERROR is
*     set to 101 and the routine exits immediately.
*
*     This routine is suitable for use on UNIX systems which support the
*     ASCII character set.
*
* MAINTENANCE LOG
* ---------------
*
*     30/07/86  PJWR  Original UNIX version stabilised.
*
* ARGUMENTS
* ---------
*
*     NOCHS   INP  Number of ASCII codes to be converted.
*     IASCII  INP  Array of ASCII codes to be converted.
*     SCHARS  OUT  String of converted codes.
*
      INTEGER NOCHS
      INTEGER IASCII(NOCHS)
      CHARACTER*(*) SCHARS
*
* COMMON BLOCK USAGE
* ------------------
*
*     Modify  /GKYERR/KERROR
*
      INCLUDE '../include/gkerr.cmn'
*
* LOCALS
* ------
*
*     MNCODE  Lowest ASCII code recognised.
*     MXCODE  Highest ASCII code recognised.
*     CODE    Temporary for code being converted.
*     I       Loop control variable.
*
      INTEGER MNCODE
      PARAMETER(MNCODE = 32)
      INTEGER MXCODE
      PARAMETER(MXCODE = 127)
      INTEGER CODE
      INTEGER I
*
* ERRORS
* ------
*
*     101   Invalid code in string
*
*-----------------------------------------------------------------------

      DO 10, I = 1, NOCHS
	CODE = IASCII(I)
	IF (CODE.LT.MNCODE.OR.CODE.GT.MXCODE) THEN
	  KERROR = 101
	  GOTO 20
	ELSE
	  SCHARS(I:I) = CHAR(CODE)
	ENDIF
10    CONTINUE

20    CONTINUE

      RETURN

      END
