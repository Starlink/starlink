      SUBROUTINE GKNTOA(NOCHS, SCHARS, IASCII)
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
*     Converts a Fortran character string into an array of ASCII codes.
*     Characters in the range SP to DEL are recognised.  In  the  event
*     of an unrecognised code being present,  KERROR is set to 101  and
*     the routine exits immediately.
*
*     This routine is suitable for UNIX systems  which  use  the  ASCII
*     character set.
*
* MAINTENANCE LOG
* ---------------
*
*     30/07/86  PJWR  Original UNIX version stabilised.
*
* ARGUMENTS
* ---------
*
*     NOCHS   INP  Size of the output array IASCII.
*     SCHARS  INP  String to be converted.
*     IASCII  OUT  Array of ASCII codes.
*
      INTEGER NOCHS
      CHARACTER*(*) SCHARS
      INTEGER IASCII(NOCHS)
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
*     MNCODE  The ASCII code of the "smallest" character recognised.
*     MXCODE  The ASCII code of the "greatest" character recognised.
*     CODE    The ASCII code of the character being examined.
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
	CODE = ICHAR(SCHARS(I:I))
	IF (CODE.LT.MNCODE.OR.CODE.GT.MXCODE) THEN
	  KERROR = 101
	  GOTO 20
	ELSE
	  IASCII(I) = CODE
	ENDIF
10    CONTINUE

20    CONTINUE

      RETURN

      END
