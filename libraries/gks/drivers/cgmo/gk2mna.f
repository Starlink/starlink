      INTEGER FUNCTION  GK2MNA (SCHAR)
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
*     Converts a single character to an ASCII integer.
*
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver
*
*
*  ARGUMENTS
*  ---------
*     INP   SCHAR    Character to be converted
*
      CHARACTER*1 SCHAR
*
*  LOCALS
*  ------
*
      INTEGER NOCHS, IASC
      PARAMETER (NOCHS = 1)
*
*  FUNCTION RETURN VALUE
*  ---------------------
*
*     ASCII equivalent of SCHAR
*
*---------------------------------------------------------------------

      CALL GKNTOA(NOCHS, SCHAR, IASC)
      GK2MNA   = IASC

      END
