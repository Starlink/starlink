C# IL>=a, OL>=0
      SUBROUTINE GKPWPD(IDNR,IDEV,RDEV)


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
*     Put pick device entry.
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP IDNR        - Pick device number
*     INP IDEV(KPCNI) - Device entry integers
*     INP RDEV(KPCNR) - Device entry reals
*
      INTEGER IDNR
      INTEGER IDEV(*)
      REAL    RDEV(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwpc.par'
      INCLUDE '../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
*  ERRORS
*  ------
*
*---------------------------------------------------------------------

      CALL GKDRPU(KIPDPT(GPICK,KWKIX),IDNR,KPCNI,KPCNR,IDEV,RDEV)

      END
