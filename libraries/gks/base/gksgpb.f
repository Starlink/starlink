C# IL>=a, OL>=1
      SUBROUTINE GKSGPB(IWKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
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
*     To playback a sequence of segments to a workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     23/11/83  JRG   Created
*     30/01/84  JRG   Output buffer when playback is all over
*     01/01/85  MGC   Additional arguments to GKSGCN for Level 2a
*     17/07/85  GSM   Move DATA statement, added by MGC for level 2a, to
*                     below EXTERNAL statement - for VME compiler.
*     20/01/87  ARG   IS conversion. Error numbers changed.
*
*  ARGUMENTS
*  ---------
*     INP  IWKID   Workstation Identifier of workstation to which segment
*                  is to be sent
*
      INTEGER IWKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /SL/     Use pointer for Segment State List
*     Read   /OPS/    GKS Operating State ... to check if segment is open.
*     Modify /WCA/    Read parameters for segment playback (KRPCC, KRPTYP,
*                     KRPSG, KINENT). Set KCVIS & KCHLT explicitly at
*                     beginning of segment; also some KWIn (n=1,2....)
*     Modify /ERR/    KERROR passed on to caller. Modified when the
*                     saved value (in IHOLD) is restored. Effect though
*                     is to preserve the value of the first error to
*                     be detected.
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IWA,RWA Local arrays saving Workstation Communication Area
*     IATTS,RATTS  Segment attributes for the segment currently being
*                  played back
*     IHOLD   Local copy of KERROR. Needed because more than error
*             condition may occur and we want the first.
*     ISTAT   Status from some routines
*     IVIS    Local copy of segment visibility
*     IDUM    Dummy argument for routine call
*     J       Loop counter
*     UFORM   Identity matrix
*
      INTEGER IWA(KNKWI), IATTS(KSGISZ), IHOLD,ISTAT,IVIS,IDUM,J
      REAL    RWA(KNQWR), RATTS(KSGRSZ)
      REAL    UFORM(6)
*
*  EXTERNALS
*  ---------
*     GKCSRD  Utility to read CSS segment on playback
*     GKSONW  Call Layer link to workstation on playback
*
      EXTERNAL GKCSRD,GKSONW
*
*  DATA
*  ----
      DATA    UFORM / 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 /
*
*  ERRORS
*  ------
*     -2007  Entry not found in list
*     -2012  Segment does not exist on CSS
*
*---------------------------------------------------------------------


*   Save Workstation Communication Area (W.C.A.) in local arrays. This is
*   because we might be called by Workstation Call Layer in the middle
*   of a list of workstations and we will certainly overwrite
*   parts of the W.C.A. needed by subsequent workstations. We need to
*   save those entries that are used by entrypoints liable to have
*   caused this playback (or regenerate) and which are also used by
*   entrypoints resulting from the playback itself. We restore at the
*   end of this routine.
      DO 100 J=1,KNKWI
  100   IWA(J)=KWIA(J)
      DO 110 J=1,KNQWR
  110   RWA(J)=QWRA(J)

*   -------------
*   Start of loop. Each iteration, we deal with one segment.
  200 CONTINUE
*       Find segment in CSS.
          CALL GKCSSL(KRPSG,ISTAT)
          IF( ISTAT.EQ.KRFUSE ) THEN
            CALL GKBUG(-2012,'GKSGPB')
            GOTO 999
          ENDIF

*       Here, segment KRPSG exists in CSS.
*       Now get the segment attributes.
          IF( KSGLST.NE.KNIL ) THEN
            CALL GKDRGE(KSGLST,KRPSG, KSGISZ,KSGRSZ, IATTS,RATTS)
          ELSE
          KERROR = -2007
          ENDIF
          IF( KERROR.NE.0 ) GOTO 980

*       Only send this segment if the request for playback (KRPCC) is
*       right (need to match with segment visibility).
          IVIS=IATTS(KSGVIS)
          IF(    KRPCC.EQ.KRPYES
     :      .OR.(KRPCC.EQ.KRPVIS .AND. IVIS.EQ.GVISI)
     :      .OR.(KRPCC.EQ.KRPINV .AND. IVIS.EQ.GINVIS) ) THEN
*          Set segment attributes variables in W.C.A.
            KCVIS=IVIS
            KCHLT=IATTS(KSGHLT)

*          Send Begin-Segment to workstation
            KWI1=KRPSG
            KWI2=KCVIS
            KWI3=KCHLT
            KWI4=IATTS(KSGDTE)
            CALL GKSONW(IWKID,KBGSG,1,KDAT,1,QDAT,QDAT,1,CH)
            IF( KERROR.NE.0 ) GOTO 800

*          Now copy the contents of the segment. Emerge on end
*          of segment (or error).
            CALL GKSGCN(IWKID,UFORM,RATTS(KSGTRN),
     :                     GKCSRD,GKSONW,.TRUE.)
            IF( KERROR.NE.0 ) THEN
*             Need to release segment.
*             KERROR saved first. This conceals any error arising from
*             GKCSRL, but there won't be any (only thing there
*             could be is a bug).
                IHOLD=KERROR
                CALL GKCSRL
                KERROR=IHOLD
                GOTO 999
            ENDIF
          ENDIF

*       Send end-of-segment to workstation. Set "request for playback"
*       first to clear the request we've just dealt with.
          KRPCC=KRPNO
          CALL GKSONW(IWKID,KENSG,1,KDAT,1,QDAT,QDAT,1,CH)
          IF( KERROR.NE.0 ) GOTO 800

*       If workstation wants another playback, then go round again
*       (alternative test KRPCC.NE.KRPNO would cause loop if there was
*        some rogue value)
          IF( KRPCC.EQ.KRPVIS .OR. KRPCC.EQ.KRPINV .OR.
     :        KRPCC.EQ.KRPYES ) GOTO 200
*   End of loop
*   -----------

*   Restore W.C.A. ... first restore general purpose variables
  800 DO 810 J=1,KNKWI
  810   KWIA(J)=IWA(J)
      DO 820 J=1,KNQWR
  820   QWRA(J)=RWA(J)

*   Output buffer
      CALL GKDDA(IWKID,IDUM)

*   Then restore current visibility and highlighting. Use standard
*   values unless a segment is open.
      IF( KOPS.EQ.GSGOP ) THEN
          CALL GKDRGE(KSGLST,KOPSG, KSGISZ,KSGRSZ, IATTS,RATTS)
          IF( KERROR.NE.0 ) GOTO 980
          KCVIS=IATTS(KSGVIS)
          KCHLT=IATTS(KSGHLT)
      ELSE
          KCVIS=GVISI
          KCHLT=GNORML
      ENDIF
      GOTO 999

*       Bug report ... workstation is asking for segment that's not in
*                      Segment State List
  980 CALL GKBUG (-2007,'GKSGPB')
*
  999 CONTINUE
      END
