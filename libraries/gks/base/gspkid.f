C# IL>=a, OL>=0
      SUBROUTINE GSPKID (IPKID)
*
* (C) COPYRIGHT ICL & SERC  1986
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET PICK IDENTIFIER
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set current pick identifier
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/86  MGC   Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*     18/05/87  DCS   Correct valid states passed to GKPRLG (S263).
*
*  ARGUMENTS
*  ---------
*     INP  IPKID  Pick identifier
*
      INTEGER IPKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/   KCPCID
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*
*  ERRORS
*  ------
*     97   Pick identifier invalid
*
*---------------------------------------------------------------------

      CALL GKPRLG (ESPKID, GGKOP, GSGOP)
      IF (KERROR.NE.0 ) GOTO 999

      IF (IPKID.LT.0) THEN
        KERROR = 97
        GOTO 999
      ENDIF

* Make pick identifier current
      KCPCID=IPKID

* Send pick identifier to active workstations if segment open
      IF (KOPS.EQ.GSGOP) THEN
        KWI1=KCPCID
        CALL GKSACW(KSPKID,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KERROR.NE.0) GOTO 999
      ENDIF
      RETURN

  999 CONTINUE
      CALL GKERR(KERROR)
      END
