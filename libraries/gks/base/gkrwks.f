C# IL>=a, OL>=1
      SUBROUTINE GKRWKS(IWKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*-----------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     To remove all segments from specified workstation and update
*     front end segment data structures accordingly.
*
*  MAINTENANCE LOG
*  ---------------
*      9/12/83  JRG   Created
*     21/12/83  JRG   Simplify the case where only one workstation is open.
*     20/02/84  JRG   Correct bug whereby KNUMSG was not correctly updated
*     19/01/87  PKY   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     INP  IWKID   Identifier of workstation on which segments are to
*                  be deleted
*
      INTEGER IWKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /SL/     Segment pointer in GKS State List
*     Read   /WCB/    Number of workstations open
*     Modify /WCA/    Sets variables in W.C.A. for entry to workstation
*     Modify /STK/    Stack used for storing some segment names
*     Modify /ERR/    KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     NSEGS  Size of piece of stack used to hold segment names
*     IHOLD  Temproary for KERROR, preserves first to occur
*     ISEG   Segment name
*     ISGRQ  Temporary for KSGRQ
*     ISTK   Pointer to stack allocated
*     J      Loop counter
*     IATTS  Segment attributes (integer)
*     RATTS  Segment attributes (real)
*
      INTEGER NSEGS
      PARAMETER (NSEGS=100)
      INTEGER IHOLD,ISEG,ISGRQ,ISTK,J, IATTS(KSGISZ)
      REAL RATTS(KSGRSZ)
*
*  ERRORS
*  ------
*   -2003 Invalid data record
*
*  STACK USAGE
*  -----------
*     NSEGS    INTEGER  Used to hold group of segment names from workstation
*
*-----------------------------------------------------------------------


*   KNOPWK is numnber of open workstations
      IF( KNOPWK.LT.1 ) THEN
        CALL GKBUG(-2003,'GKRWKS')
      ELSEIF( KNOPWK.EQ.1 ) THEN

*      If specified workstation IWKID is the only one open, then we just
*      delete all segments - no need to find out from workstation what
*      they are.
*      First, find whether workstation uses CSS (in KSGRQ)
        KWI1=KREL
        KWI2=KLOEST
        KSGRQ=0
        CALL GKSONW(IWKID,KQSGWK,1,KDAT,1,QDAT,QDAT,1,CH)

*      If there are any segments, delete them all from segment state list
*      and from CSS if appropriate.
        IF( KSGLST.NE.KNIL ) THEN
          CALL GKDRDL(KSGLST)
          KNUMSG=0
          KWI1=2
          IF( KSGRQ.GT.0 ) CALL GKCSWD(KCLRWK, 1,KDAT, 1,QDAT,QDAT,
     :                                            1,CH)
        ENDIF
      ELSE

*      Here: there are other open workstations. Therefore we can only delete
*      segments on the specified workstation. Retrieve a group of segments
*      on each iteration and deal with each.
*      First: set the 'starting segment' to be the lowest priority,
*             then allocate some stack (no need to fetch a lot).
        KWI2=KLOEST
        CALL GKSTAL(KINTGS,NSEGS,ISTK)
        IF( KERROR.NE.0 ) GOTO 999

*      Loop: while there are segments to delete, delete a group of them
  300   CONTINUE

*         Inquire about next group of NSEGS segments (the workstation
*         remembers what the next group is).
*         KWI2 already contains the specification KLOEST or KHIER
            KWI1=KREL
            KSGRQ=0
            CALL GKSONW(IWKID,KQSGWK, NSEGS,KSTACK(ISTK),
     :                                   1,QDAT,QDAT, 1,CH)
            IF( KERROR.NE.0 ) GOTO 390

*         Here: KSGRQ>0 indicates that workstation uses CSS,
*               KNIR (=<NSEGS) is the number of segments returned in
*                              this group,
*               KSTACK(ISTK) and succeeding elements contain the
*                              segment names.
*         Now deal with each segment (first save KSGRQ....could get overwrit)
            ISGRQ=KSGRQ
            DO 320 J=ISTK,ISTK+KNIR-1

*             Save segment name for this iteration
                ISEG=KSTACK(J)

*             If CSS then delete it from CSS
                KWI1=ISEG
                IF( ISGRQ.GT.0 ) CALL GKCSWD(KDSG, 1,KDAT,
     :                                                1,QDAT,QDAT,1,CH)
                IF( KERROR.NE.0 ) GOTO 390

*             Get attributes (but only interested in nmbr of workstations)
                CALL GKDRGE(KSGLST,ISEG, KSGISZ,KSGRSZ, IATTS,RATTS)
                IF( KERROR.NE.0 ) GOTO 390

*             Reduce nmbr of workstations (associated with this segment)
*             by 1.
                IATTS(KSGNWS)=IATTS(KSGNWS)-1

*             If workstations associated is now zero, delete the segment,
*             otherwise put all the attributes back.
                IF( IATTS(KSGNWS).EQ.0 ) THEN
                  CALL GKDRDE(KSGLST,ISEG)
                  KNUMSG=KNUMSG-1
                ELSE
                  CALL GKDRPU(KSGLST,ISEG,KSGISZ,KSGRSZ,IATTS,RATTS)
                ENDIF
                IF( KERROR.NE.0 ) GOTO 390
  320       CONTINUE

*         The next group of segments is of higher priority.
            KWI2=KHIER

*         If the workstation fills the space we provided, then we
*         assume it has more segments to tell us, so we repeat.
            IF( KNIR.EQ.NSEGS ) GOTO 300

*      End loop

*      Deallocate stack (first we save KERROR)
  390   IHOLD=KERROR
        CALL GKSTDA(KINTGS,ISTK)
        IF( IHOLD.NE.0 ) KERROR=IHOLD
      ENDIF

*
  999 CONTINUE
      END
