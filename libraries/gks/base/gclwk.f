      SUBROUTINE GCLWK(IWKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  CLOSE WORKSTATION
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To close specified workstation
*
*  MAINTENANCE LOG
*  ---------------
*      11/7/83   JRG  Original version stabilized
*     10/12/83   JRG  Changes to workstation interface due to segment
*                     design
*      10/1/84   JRG  Send UPDATE WORKSTATION only if category is right
*      20/1/84   JRG  Prevent metafile workstations from receiving things
*                     that they shouldn't (using wkstn category)
*      24/01/85  MGC  -ditto- for WISS (strictly Level 2)
*
*  ARGUMENTS
*  ---------
*     Inp   IWKID   Workstation Identifier
*
      INTEGER IWKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYOPS/ Operating State becomes 'GKS Open' if last
*                     workstation successfully closed
*     Read   /GKYWCB/ Lists checked
*     Modify /GKYWCA/ KWI1 set before entry to workstation
*     Modify /GKYERR/ Sets KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     INFA   'New frame action necessary on update' returned from
*            workstation via GKDDA
*     IWKIX  Workstation Index .... index of this workstation in list
*            of workstation identifiers
*     J      Loop counter
*
      INTEGER INFA,IWKIX,J
*
*  ERRORS
*  ------
*        7   Wrong GKS state
*       20   Invalid workstation identifier
*       25   Specified workstation not open
*       29   Specified workstation is active
*      147   Input queue has overflowed
*
*---------------------------------------------------------------------


*   GKS Prologue
      CALL GKPRLG(ECLWK,GWSOP,GSGOP)
      IF( KERROR.NE.0 ) GOTO 990

*   Check input queue has not overflowed
C#IL>=C
C#

*   Check workstation i.d. is valid
      IF( IWKID.LT.1 ) THEN
        KERROR=20
        GOTO 990
      ENDIF

*   Find this workstation identifier in list of workstation identifiers.
      CALL GKFIND(KWKID, KWK, IWKID, 25, IWKIX)
      IF( KERROR.NE.0 ) GOTO 990

*   Check workstation is not active
      DO 100 J=1,KNACWK
        IF( KACPT(J).EQ.IWKIX ) GOTO 980
  100 CONTINUE
*   End of preliminary error checking.
*   Workstation index is in IWKIX
*   -----------------------------------

*   Send CLOSE WORKSTATION (1st entry)
      KWI1=1
      CALL GKSONW(IWKID,KCLWK, 1,KDAT,1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 990

*   Update only if the workstation category is appropriate
      IF( .NOT.  (KWKC(IWKIX).EQ.GMI  .OR.
     :            KWKC(IWKIX).EQ.GMO  .OR.
     :            KWKC(IWKIX).EQ.GINPUT .OR.
     :            KWKC(IWKIX).EQ.GWISS) ) THEN

*       Send UPDATE WORKSTATION
          KWI1=GPERFO
          CALL GKSONW(IWKID, KUWK, 1,KDAT, 1,QDAT,QDAT,1,CH)
          IF( KERROR.NE.0 ) GOTO 990

*       If UPDATE not completely performed by workstation, split up the
*       actions
          IF( KWDONE.EQ.KRFUSE ) THEN

*       Do deferred actions
            CALL GKDDA(IWKID,INFA)
            IF( KERROR.NE.0 ) GOTO 990

*       If regenerate is pending, then do it
            IF( INFA.EQ.GYES ) THEN

*       Clear display surface
              CALL GKCLDS(IWKID,GCONDI)
              IF( KERROR.NE.0 ) GOTO 990

C#OL>=1
*       Redraw all visible segments
              CALL GKRDRW(IWKID)
              IF( KERROR.NE.0 ) GOTO 990
C#
            ENDIF
          ENDIF
      ENDIF

C#IL>=C
C#OL>=1
*   Remove all segments from this workstation if the appropriate category
      IF(.NOT. (KWKC(IWKIX).EQ.GMI .OR. KWKC(IWKIX).EQ.GINPUT) )
     :      CALL GKRWKS(IWKID)
      IF( KERROR.NE.0 ) GOTO 990
C#

*   Send CLOSE WORKSTATION (2nd entry) to workstation
      KWI1=2
      CALL GKSONW(IWKID, KCLWK, 1,KDAT, 1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 990

*   Remove this workstation from Workstation Control Block
      CALL GKCWCB(IWKID)
      IF( KERROR.NE.0 ) GOTO 990

*   If no open workstations remain, set OperatingState <- GKS Open
      IF( KNOPWK.EQ.0 ) KOPS=GGKOP
      GOTO 999

*   Error report for "Workstation is active"
  980 KERROR=29

*   Error reporting
  990 CALL GKERR(KERROR)

  999 CONTINUE
      END
