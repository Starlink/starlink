      SUBROUTINE GK2MHA(INCHAR)
*---------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  Routine to convert hex written as 2 characters
*  into 1 ASCII character (machine hex)
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*        INCHAR : Used as a temporary hex variable
*
*  LOCALS
*  ------
*        I      : Loop counter
*        N      : Loop counter
*        TEMP   : Temporary variable

*        CHASET : Contains hex digits (as characters)
*        STRIN  : Input string

      INTEGER I, N, TEMP
      CHARACTER CHASET*16, INCHAR*80, STRIN*40
      DATA CHASET/'0123456789ABCDEF'/

*---------------------------------------------------------------

      N=1
      I=1
  10  CONTINUE

*   Convert characters to integer value
      TEMP=(INDEX(CHASET,INCHAR(N:N))-1)*16
      TEMP=TEMP+(INDEX(CHASET,INCHAR(N+1:N+1))-1)

*   Convert the integer value to an ASCII character
      STRIN(I:I)=CHAR(TEMP)
      N=N+2
      I=I+1

*   Repeat if not end of string (delimited by a space)
      IF (INCHAR(N:N).NE.' ') GOTO 10

*   Set last ASCII character to NUL
      STRIN(I:I)=CHAR(0)

*   Write to CGM
      CALL GK2MBU(STRIN)
      RETURN
      END
