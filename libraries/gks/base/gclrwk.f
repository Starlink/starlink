      SUBROUTINE GCLRWK(IWKID,ICOFL)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  CLEAR WORKSTATION
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Clear a workstation of its segments and its display surface contents.
*
*  MAINTENANCE LOG
*  ---------------
*     05/05/83   JRG  Original version stabilized
*      14/6/83   JRG  Make call to CLEAR WORKSTATION entry in workstation
*      23/6/83   JRG  Changes to use KERROR for error reporting
*     10/12/83   JRG  Changes to workstation interface as a result of
*                     segment design
*     21/12/83  JRG   KERROR check after GKRWKS
*     20/01/87  PKY   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation Identifier of workstation to be cleared
*     INP   ICOFL  Clear Control Flag: valid values are 'condit' or
*                  'always'
*
      INTEGER IWKID,ICOFL
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYWCA/ Used before call to CLEAR WORKSTATION workstation entry
*     Modify /GKYERR/ KERROR
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     INFA   Flag returned by GKDDA: actually ignored by this subroutine
*
      INTEGER INFA
*
*  ERRORS
*  ------
*        6   Wrong GKS state
*       20   Workstation Identifier invalid
*       25   Workstation not open
*    33,35   Workstation in wrong category
*     2000   Enumeration type out of range
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG(ECLRWK,GWSOP,GWSAC)
      IF( KERROR.GT.0 ) GOTO 990

*   Check Clear Control Flag
      IF(.NOT. ( ICOFL.EQ.GCONDI .OR. ICOFL.EQ.GALWAY ) ) THEN
          KERROR=2000
          GOTO 990
      ENDIF
*   End of preliminary error checking.
*   ---------------------------------

*   Send CLEAR WORKSTATION (1st entry) to the workstation
      KWI1=1
      CALL GKSONW(IWKID,KCLRWK, 1,KDAT, 1,QDAT,QDAT, 1,CH)
      IF( KERROR.NE.0 ) GOTO 990

*   Delete all segments
      CALL GKRWKS(IWKID)
      IF( KERROR.NE.0 ) GOTO 990

*   Send CLEAR WORKSTATION (2nd entry) to the workstation. If refused, then
*   split into individual actions.
      KWI1=2
      KWI2=ICOFL
      CALL GKSONW(IWKID, KCLRWK, 1,KDAT, 1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 990
      IF( KWDONE.EQ.KRFUSE ) THEN

*   Perform deferred output actions. The reply in INFA (new frame
*   necessary) is ignored here, as the succeeding steps are always
*   taken (subject of course to the Clear Control Flag).
        CALL GKDDA(IWKID,INFA)
        IF( KERROR.NE.0 )  GOTO 990

*   Clear display surface
        CALL GKCLDS(IWKID,ICOFL)
        IF( KERROR.NE.0 )  GOTO 990
*   No need to call GKTOLD here as it's already been done in GKCLDS
      ELSE
        CALL GKTOLD
      ENDIF
      GOTO 999

*   Report error
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
