C# IL>=a, OL>=0
      SUBROUTINE GKPWRL(IDNR)


*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Release pick device
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilised
*     14/07/87  RMK   Changed calls of heap routines to use enumerated
*                     types in GKHP.PAR.
*
*  ARGUMENTS
*  ---------
*     INP IDNR   - Pick device number
*
      INTEGER IDNR
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwpc.par'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IDEV    Device entry integers
*     RDEV    Device entry reals
*
      INTEGER IDEV(KPCNI)
      REAL    RDEV(KPCNR)
*
*  ERRORS
*  ------
*
*---------------------------------------------------------------------


*     get pick device details
      CALL GKRQIP(GPICK,IDNR,KPCNI,KPCNR,IDEV,RDEV)
      IF(KERROR.EQ.0) THEN
        CALL GKHPDA(IDEV(KPCIWI),KINTGS)
        CALL GKHPDA(IDEV(KPCIWR),KREALS)
        IDEV(KPCIWI)=KNIL
        IDEV(KPCIWR)=KNIL
        CALL GKPWPD(IDNR,IDEV,RDEV)
      ENDIF

      END
