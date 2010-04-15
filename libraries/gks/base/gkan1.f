      CHARACTER*1 FUNCTION GKAN1(IASC)
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
*     Converts a single ASCII number into a character.
*     Should be used instead of Fortran function CHAR.
*
*  MAINTENANCE LOG
*  ---------------
*     16/04/86  RMK  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP   IASC     ASCII integer to be converted
*
      INTEGER IASC
*
*  FUNCTION RETURN VALUE
*  ---------------------
*
*     Character equivalent of IASC
*
*  COMMENTS
*  --------
*
*     This routine is SYSTEM DEPENDENT.
*-----------------------------------------------------------------------

      GKAN1 = CHAR(IASC)
      END
