C# IL>=a, OL>=0
      SUBROUTINE GDAWK(IWKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  DEACTIVATE WORKSTATION
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To remove the specified workstation from the list of active
*     workstations.
*
*  MAINTENANCE LOG
*  ---------------
*       6/7/83   JRG  Original version stabilized
*       7/7/83   JRG  Bug fix: should set operating state
*
*  ARGUMENTS
*  ---------
*     Inp  IWKID   Workstation Identifier
*
      INTEGER IWKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYOPS/ Alter Operating State, if no remaining active wkstns
*     Modify /GKYWCB/ Inspect list of open workstations; alter list of
*                     active workstations.
*     Modify /GKYERR/ Alter KERROR.
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*    IACTIV Index of this workstation in list of active workstations
*    IWKIX  Workstation index (i.e. index of this workstation in list
*           of workstation identifiers)
*    J      Loop counter
*
      INTEGER IACTIV,IWKIX,J
*
*  ERRORS
*  ------
*        3   Wrong GKS state
*       20   Invalid workstation i.d. specified
*       30   Workstation not active
*    33,35   Workstation of wrong category
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG(EDAWK,GWSAC,GWSAC)
      IF( KERROR.NE.0 ) GOTO 990

*   Check that workstation identifier is valid. For most routines,
*   check is made in GKSONW, but DEACTIVATE does not call it so we
*   have to do it here.
      IF( IWKID.LT.1 ) THEN
        KERROR=20
        GOTO 990
      ENDIF

*   Find this workstation identifier
      CALL GKFIND(KWKID,KWK,IWKID,30,IWKIX)
      IF( KERROR.NE.0 ) GOTO 990

*   Find the workstation index IWKIX in the active list
      CALL GKFIND(KACPT,KNACWK,IWKIX,30,IACTIV)
      IF( KERROR.NE.0 ) GOTO 990

*   Check the workstation category - surely this can never be wrong
*   if the workstation is in the active list ?!
      IF    ( KWKC(IWKIX).EQ.GMI    ) THEN
                KERROR=33
                GOTO 990
      ELSEIF( KWKC(IWKIX).EQ.GINPUT ) THEN
                KERROR=35
                GOTO 990
      ENDIF
*   End of error checking
*   ---------------------

*   Now remove index IWKIX from the active list (its position in
*   in the list is IACTIV) and keep the list contiguous.
      DO 100 J=IACTIV+1,KNACWK
  100   KACPT(J-1)=KACPT(J)
      KNACWK=KNACWK-1

*   Set GKS Operating State, if no remaining active workstations
      IF( KNACWK.EQ.0 ) KOPS=GWSOP
      GOTO 999

*   Report error
  990 CALL GKERR(KERROR)

*   Finish
  999 CONTINUE
      END
