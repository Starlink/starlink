C# IL>=a, OL>=1
      SUBROUTINE GSDS(IWKID, IDEFER, IMRGEN)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  SET DEFERRAL STATE
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*    To set deferral mode and implicit regeneration mode on the
*    workstation. Deferred actions are output if appropriate with
*    the new setting. A regeneration takes place if appropriate.
*
*  MAINTENANCE LOG
*  ---------------
*      5/7/83   JRG  Original version stabilized
*     28/9/83   AS   Change subroutine name
*     20/01/87  PKY   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation identifier of workstation to be operated on
*     INP   IDEFER New deferral mode
*     INP   IMRGEN New implicit regeneration mode
*
      INTEGER IWKID, IDEFER, IMRGEN
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKYWCA/ Set KWI1 before call to workstation; inspect
*                     KWDONE after call
*     Read   /GKYERR/ KERROR
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*        7   Wrong GKS state
*       20   Workstation Identifier invalid
*       25   Workstation not open
*  33,35,36  Workstation Category not suitable for this routine
*     2000 Enumeration type out of range
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG (ESDS,GWSOP,GSGOP)
      IF( KERROR.GT.0 ) GOTO 990

*   Check new values for deferral state supplied in argument list
      IF(.NOT.  ((IDEFER.EQ.GASAP .OR. IDEFER.EQ.GBNIG .OR.
     :            IDEFER.EQ.GBNIL .OR. IDEFER.EQ.GASTI)
     :      .AND. (IMRGEN.EQ.GALLOW .OR. IMRGEN.EQ.GSUPPD) )) THEN
                     KERROR=2000
                     GOTO 990
      ENDIF

*   Send SET DEFERRAL STATE to workstation.
      KWI1=IDEFER
      KWI2=IMRGEN
      CALL GKSONW(IWKID,KSDS, 1,KDAT, 1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 990

*   Set output flags to be out of date
      CALL GKTOLD

*   Regenerate if necessary
      IF( KRGN ) CALL GKRGN
      IF( KERROR.EQ.0 ) GOTO 999

*   Report error
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
