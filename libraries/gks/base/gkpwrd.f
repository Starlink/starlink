C# IL>=a, OL>=0
      SUBROUTINE GKPWRD(XLOCAT,IACT,ISTATE,RSTATE,IREQ)


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
*     Read pick device.
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP XLOCAT         - Locator routine
*     INP IACT           - Actioned request
*     INP ISTATE(KPCNWI) - Device state integers
*     INP RSTATE(KPCNWR) - Device state reals
*     OUT IREQ           - Next request
*
      INTEGER IACT,IREQ
      INTEGER ISTATE(*)
      REAL    RSTATE(*)
      EXTERNAL XLOCAT
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkpca.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkwpc.par'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     ECOS   Echo state (OFF,SHOW,ERASE)
*
      INTEGER   OFF,  SHOW,  ERASE
      PARAMETER(OFF=0,SHOW=1,ERASE=2)
      INTEGER ECOS
*
*  ERRORS
*  ------
*
*---------------------------------------------------------------------

      IF(ISTATE(KPWSTA).EQ.GNPICK .OR. IACT.EQ.KPECHO) THEN
        CALL XLOCAT(ISTATE(KPWTRG),RSTATE(KPWDCX),RSTATE(KPWDCY))
        IF(KERROR.NE.0) THEN
          ISTATE(KPWSTA)=GNONE
          IREQ=KNIL
          GOTO 999
        ENDIF
      ENDIF

      IF(ISTATE(KPWSTA).EQ.GOK) THEN
        IF(IACT.EQ.KPECHO) THEN
          ECOS=ERASE
        ELSEIF(IACT.EQ.KPUNEC) THEN
          ECOS=OFF
        ELSE
          ECOS=SHOW
        ENDIF

      ELSE
        ECOS=OFF
      ENDIF

      IF(ECOS.EQ.ERASE) THEN
        IREQ=KPUNEC
      ELSEIF(ECOS.EQ.SHOW) THEN
        IREQ=KPECHO

      ELSE
        IF(ISTATE(KPWTRG).EQ.KPTBRK) THEN
          ISTATE(KPWSTA)=GNONE
          IREQ=KNIL
        ELSEIF(ISTATE(KPWTRG).EQ.KPTFIN) THEN
          IREQ=KNIL
        ELSE
          IREQ=KPSCAN
        ENDIF

      ENDIF

*
 999  CONTINUE

      END
