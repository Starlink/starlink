C# IL>=a, OL>=1
      SUBROUTINE GQACWK (NTH, IER, NACWK, LACWK)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE SET MEMBER OF ACTIVE WORKSTATIONS
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Nth member of the Set of Active Workstations
*
*  MAINTENANCE LOG
*  ---------------
*     10/10/82  CJW  Original version stabilized
*     22/12/83  AS   Avoid returning KCSWIX
*     16/02/84  JRG  Return NOPWK even if NTH is invalid; also correct
*                    bug that causes error indicator to be set whenever
*                    CSS is on the active list
*     22/01/87  JCS  IS conversion. Error number changed.
*     27/09/90  KEVP Report error 2002 only if there is at least one
*                    active workstation and the list element requested
*                    is non-zero (C41). Required by GKS FORTRAN BINDING.
*
*  ARGUMENTS
*  ---------
*     IN    NTH    Set member requested
*     OUT   IER    Error indicator
*     OUT   NACWK  Number of Active workstations
*     OUT   LACWK  Nth member of set of active workstations
*
      INTEGER NTH, IER, NACWK, LACWK
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkwcb.cmn'
*
*  ERRORS
*  ------
*     2002  Set member not available
*
*---------------------------------------------------------------------


      CALL GKPRLG(KNIL,GGKOP,GSGOP)
      IER = KERROR

*   NACWK is returned whatever set member is requested; allowance must
*   be made for the possibility of CSS on the end.
*   LACWK is returned only if the requested set member is valid.
      IF (KERROR .EQ. 0) THEN
        NACWK=KNACWK
        LACWK=KNIL
        IF( KNACWK.GT.0 ) THEN
          IF( KACPT(KNACWK).EQ.KCSWIX ) NACWK=KNACWK-1
        ENDIF
        IF (NTH.GT.0.AND.NTH.LE.NACWK) THEN
          LACWK = KWKID(KACPT(NTH))
        ELSEIF ((NTH.NE.0).AND.(NACWK.NE.0)) THEN
          IER = 2002
        ENDIF
      ENDIF

      END
