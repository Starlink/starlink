C# IL>=a, OL>=0
      SUBROUTINE GQOPWK (NTH, IER, NOPWK, LOPWK )
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE SET MEMBER OF OPEN WORKSTATIONS
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Nth member of set of Open Workstations
*
*  MAINTENANCE LOG
*  ---------------
*     04/10/83  CJW   Original version stabilized
*     16/02/84  JRG   Return NOPWK even if NTH is invalid
*     22/01/87  JCS   IS conversion. Error number changed.
*     27/09/90  KEVP  Report error 2002 only if there is at least one
*                     open workstation and the list element requested
*                     is non-zero (C41). Required by GKS FORTRAN BINDING.
*
*  ARGUMENTS
*  ---------
*     IN    NTH    List element requested
*     OUT   IER    Error indicator
*     OUT   NOPWK  Number of open workstations
*     OUT   LOPWK  Nth member of set of open workstations
*
      INTEGER NTH, IER, NOPWK, LOPWK
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
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

*   NOPWK is returned whatever set member has been requested.
*   LOPWK is returned only if the set member requested is valid.
      IF (KERROR .EQ. 0) THEN
        NOPWK=KNOPWK
        LOPWK=KNIL
        IF (NTH.GT.0 .AND. NTH.LE.KNOPWK) THEN
          LOPWK = KWKID(KOPPT(NTH))
        ELSEIF((NTH.NE.0).AND.(NOPWK.NE.0))THEN
          IER = 2002
        ENDIF
      ENDIF

      END
