C# IL>=a, OL>=0
      SUBROUTINE GKERR(IERROR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONT END
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*  A front end to the error handler. It sets the error state on, calls the
*  error handler GERHND and then sets the error state off.
*
*  MAINTENANCE LOG
*  ---------------
*     07/02/83  CJW  Original version stabilized
*     27/06/83  CJW  Implement revised error handling precedure
*                    (No change required)
*     28/07/83  CJW  Change to integer names
*
*  ARGUMENTS
*  ---------
*     INP   IERROR The GKS error number
*
      INTEGER IERROR
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read     /GKERR/    KRTNM,KERRFL
*     Modify   /GKERR/    KERRS
*
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkerr.par'
*
*---------------------------------------------------------------------


*     Set Error state on

      KERRS = KON

*     Call Error Handler

      CALL GERHND(IERROR,KRTNM,KERRFL)

*     Set Error state off

      KERRS = KOFF

      END
