C# IL>=a, OL>=0
      SUBROUTINE GKPWSL(IDNR,ISTATE,RSTATE)


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
*     Select pick device.
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilised
*     06/08/90  KEVP  Removed unused local variable (S342)
*
*  ARGUMENTS
*  ---------
*     INP IDNR           - Pick device number
*     INP ISTATE(KPCNWI) - Device state integers
*     INP RSTATE(KPCNWR) - Device state reals
*
      INTEGER IDNR
      INTEGER ISTATE(*)
      REAL    RSTATE(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkinp.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkwpc.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*     IDEV    Device entry integers
*     RDEV    Device entry reals
*     SEGPRI  Segment priority
*     BBOX    Bounding box
*

      INTEGER IDEV(KPCNI)
      REAL    SEGPRI,BBOX(4)
      REAL    RDEV(KPCNR)
*
*  ERRORS
*  ------
*
*---------------------------------------------------------------------

      CALL GKPWAL(IDNR,IDEV,RDEV)
      IF(KERROR.EQ.0) THEN

        IF(IDEV(KPCISG).EQ.KNIL) THEN
          ISTATE(KPWSTA)=GNPICK
        ELSEIF(IDEV(KPCINS).EQ.GOK) THEN
          CALL GKSLGE(KSSGPT(KWKIX),IDEV(KPCISG),ISTATE(KPWSG),
     :                   SEGPRI,BBOX)
          IF(IDEV(KPCISG).EQ.ISTATE(KPWSG)) THEN
            ISTATE(KPWSTA)=GOK
          ELSE
            ISTATE(KPWSTA)=GNPICK
          ENDIF
        ELSE
          ISTATE(KPWSTA)=GNPICK
        ENDIF

        ISTATE(KPWID)=IDEV(KPCINI)
        ISTATE(KPWPI)=1
        ISTATE(KPWESW)=IDEV(KIPE)
        ISTATE(KPWPET)=IDEV(KIPPRT)
        ISTATE(KPWTRG)=KPTPRV

        CALL GKHPPI(IDEV(KPCIWI),0,KPCNWI,ISTATE)
        CALL GKHPPR(IDEV(KPCIWR),0,KPCNWR,RSTATE)

      ENDIF

      END
