
* --------------------------------------------------------------
      SUBROUTINE GK0CPB(NINTA)
* --------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             RMK
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81  Takes an ASCII value and puts it into an array element to allow
*     this to be safely passed to GK0CPA.
*
*  MAINTENANCE LOG
*  ---------------
*     09/07/84  RMK   Original version stabilized
*  ARGUMENTS
*  ---------
*
*  INP NINTA  ASCII value
*
      INTEGER NINTA
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
      INTEGER INTA(1)
*
* ----------------------------------------------------
*
      INTA(1)=NINTA
      CALL GK0CPA(KIOPB,1,INTA)
      RETURN
      END
