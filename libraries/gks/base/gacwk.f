C# IL>=a, OL>=0
      SUBROUTINE GACWK(IWKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  ACTIVATE WORKSTATION
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*      To activate the specified workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     16/03/83  JRG   Stabilised
*     30/03/83  JRG   CHECK.INC included
*     07/04/83  JRG   EXTERNAL declaration corrected
*     11/04/83  JRG   GKWKE included; in -> input
*     23/06/83  JRG   Changes to use KERROR for error reporting
*     19/01/87  DCS   IS conversion. Remove Metafile Input Attribute List
*                     and bring workstation up to date (transformation
*                     and attributes) directly (ie remove calls to
*                     GKUPCV).
*     20/01/87  PKY   IS conversion. Comment about error 43 added.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID  Workstation identifier
*
      INTEGER IWKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYOPS/ Change GKS Operating State
*     Read   /GKYWCA/,/GKZWCA/ Use dummy arrays KDAT,QDAT,CH
*     Modify /GKYWCB/ Add to list of active workstations
*     Read   /GKYSL/  Inspect source flags
*     Modify /GKYERR/ KERROR
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     J      Loop counter
*     IX     Workstation Index
*
      INTEGER J, IX
*
*  ERRORS
*  ------
*     6      GKS in wrong state
*     20     Workstation Identifier invalid
*     25     Workstation does not appear on list of open workstations
*     29     Workstation is already active
*     33     Workstation is metafile input
*     35     Workstation is input only
*
*---------------------------------------------------------------------



*   GKS Prologue
      CALL GKPRLG(EACWK,GWSOP,GWSAC)
      IF( KERROR.GT.0 ) GOTO 990

*   Check that workstation id is valid
      IF( IWKID.LT.1 ) GOTO 960

*   Check that workstation is open
      DO 100 IX=1,KWK
        IF( KWKID(IX).EQ.IWKID ) GOTO 110
  100 CONTINUE
      KERROR=25
      GOTO 990
*   Here: IX is Workstation Index.
  110 CONTINUE

*   Check that workstation is not already active
      DO 150 J=1,KNACWK
        IF(  KACPT(J).EQ.IX  ) GOTO 970
  150 CONTINUE

*   Error 43 (Maximum number of active workstations would be exceeded)
*   is not tested since max numbers of open and active workstations are
*   the same and max number of open workstations has already been tested

*   Check that this workstation's category is suitable
      IF( KWKC(IX).EQ.GMI ) GOTO 975
      IF( KWKC(IX).EQ.GINPUT ) GOTO 980
*   End of error checking
*---------------------------

*   Now bring this workstation up to date with info on transformations
*   and attributes.
*   Depending on source flag, send current transformations and
*   clipping rectangle if necessary.
*   Then deal with attributes for each primitive in turn
      IF( KSTRWK.NE.KHANGE ) THEN
        IF( KSTRWK.EQ.KGKSFN ) CALL GKCCTG
        IF( KSTRWK.EQ.KMI) CALL GKCCTM
        CALL GKSONW(IWKID, KNT, 1,KDAT, 1,QDAT,QDAT, 1,CH)
      ENDIF
      IF( KSPLWK.EQ.KGKSFN ) THEN
        CALL GKCPLG
        CALL GKSONW(IWKID, KSPLA, 1,KDAT, 1,QDAT,QDAT, 1,CH)
      ENDIF
      IF( KSPMWK.EQ.KGKSFN ) THEN
        CALL GKCPMG
        CALL GKSONW(IWKID, KSPMA, 1,KDAT, 1,QDAT,QDAT, 1,CH)
      ENDIF
      IF( KSTXWK.EQ.KGKSFN ) THEN
        CALL GKCTXG
        CALL GKSONW(IWKID, KSTXA, 1,KDAT, 1,QDAT,QDAT, 1,CH)
      ENDIF
      IF( KSFAWK.EQ.KGKSFN ) THEN
        CALL GKCFAG
        CALL GKSONW(IWKID, KSFAA, 1,KDAT, 1,QDAT,QDAT, 1,CH)
      ENDIF

*   Update list of active workstations in W.C.B.
      KNACWK=KNACWK+1
      KACPT(KNACWK)=IX

*   GKS Operating State <- WorkstationActive
      KOPS=GWSAC
      GOTO 999
*-----------------------------


*   Error reporting.
  960 KERROR=20
      GOTO 990

  970 KERROR=29
      GOTO 990

  975 KERROR=33
      GOTO 990

  980 KERROR=35
      GOTO 990

*   Report error in KERROR
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
