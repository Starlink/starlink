      INTEGER FUNCTION GKNA1(SCHAR)
*
* (C) COPYRIGHT ICL & SERC  1986
*
*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    System Interface Routine
*  Author:             RMK
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Converts a single character to an ASCII integer.
*     Should be used instead of Fortran function ICHAR.
*
*  MAINTENANCE LOG
*  ---------------
*     16/04/86  RMK  Original version stabilized
*     22/04/86  RMK  Version for Prime FTN77 compiler.
*
*  ARGUMENTS
*  ---------
*     INP   SCHAR    Character to be converted
*
      CHARACTER*1 SCHAR
*
*  FUNCTION RETURN VALUE
*  ---------------------
*
*     ASCII equivalent of SCHAR
*
*  COMMENTS
*  --------
*
*     This routine is SYSTEM DEPENDENT.
*
*-----------------------------------------------------------------------

      GKNA1 = ICHAR(SCHAR)
      END
