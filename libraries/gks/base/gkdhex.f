      SUBROUTINE GKDHEX(IDEC,HEXSTR)
*
* Copyright (C) SERC 1987
*
*---------------------------------------------------------------------
*
* Type of routine:  UTILITY
* Author:           PLP
*
      INCLUDE '../include/check.inc'
*
* PURPOSE OF THE ROUTINE
* ----------------------
*     Converts a Decimal Integer (in the range 0 to 255) into a
*     Hexadecimal String
*
* MAINTENANCE LOG
* ---------------
*     21/04/88  PLP   Original version stabilized.
*
* ARGUMENTS
* ---------
*     INP  IDEC    An Integer containing the Decimal number (0-255)
*                  for conversion
*     OUT  HEXSTR  A two character string containing the resulting
*                  Hexadecimal number.
*
      INTEGER IDEC
      CHARACTER*2 HEXSTR
*
* COMMON BLOCK USAGE
* ------------------
*
      INCLUDE '../include/gkerr.cmn'
*
* INTRINSIC FUNCTIONS
* -------------------
*
      INTRINSIC MOD
*
* LOCALS
* ------
*
*     HEXCON Decimal digit to hexadecimal character conversion array
*
      CHARACTER HEXCON(0:15)

      DATA HEXCON/'0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
     :            'A', 'B', 'C', 'D', 'E', 'F'/
*
* ERRORS
* ------
*
*   -2004   Documented condition to be satisfied by parameter(s)
*           of internal routine is not satisfied
*
* ALGORITHM
* ---------
*
*     Conversion is done for integers from 0 to 255 ONLY. Input
*     data outside this range will trigger error message and an
*     "out of format" (**) return string.
*
*---------------------------------------------------------------------

*     Discard erroneous input first
      IF (0.LE.IDEC.AND.IDEC.LE.255) THEN
*        Do the conversion
         HEXSTR(1:1) = HEXCON(IDEC/16)
         HEXSTR(2:2) = HEXCON(MOD(IDEC,16))
      ELSE
*        Input outside the expected range - "Out Of Format" error
         KERROR = -2004
         HEXSTR = '**'
      ENDIF

      END
