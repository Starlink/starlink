C# IL>=a, OL>=0
      SUBROUTINE GKPWAL(IDNR,IDEV,RDEV)


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
*     Allocate space for pick device state data
*     and return pick device entry details.
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilised
*     14/07/87  RMK   Changed calls of heap routines to use enumerated
*                     types in GKHP.PAR.
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
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkwpc.par'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
*  ERRORS
*  ------
*
*---------------------------------------------------------------------

*     get pick device details
      CALL GKRQIP(GPICK,IDNR,KPCNI,KPCNR,IDEV,RDEV)
      IF(KERROR.EQ.0) THEN

*       allocate device state integers
        CALL GKHPAL(KPCNWI,KINTGS,IDEV(KPCIWI))
        IF(KERROR.NE.0) THEN
          IDEV(KPCIWI)=KNIL
        ELSE

          CALL GKHPAL(KPCNWR,KREALS,IDEV(KPCIWR))
          IF(KERROR.NE.0) THEN
            IDEV(KPCIWR)=KNIL
*           error so deallocate device state integers
            CALL GKHPDA(IDEV(KPCIWI),KINTGS)
            IDEV(KPCIWI)=KNIL
          ELSE
*           update pick device entry
            CALL GKPWPD(IDNR,IDEV,RDEV)
          ENDIF

        ENDIF

      ENDIF

      END
