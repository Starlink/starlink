C# IL>=a, OL>=1
      SUBROUTINE GCRSG(ISGNO)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  CREATE SEGMENT
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level create segment.
*
*  MAINTENANCE LOG
*  ---------------
*     18/11/83  JGW   Original version stabilised
*     18/11/83  AS    Include CHECK.INC, other minor changes
*     22/11/83  AS    Change KSGLN to KSGSLN
*     25/11/83  AS    Maintain KNUMSG, number of segments in use
*     14/12/83  JRG   Open CSS, set open segment, 917->1017
*     19/12/83  JRG   Sets KWI1,QWR1 again for creating segment in CSS
*      3/1/84   JRG   Correct GKS state checking; delete seg if error;
*                     remove regeneration check
*     16/02/84  JRG   If storage overflow, then emergency close GKS
*     16/03/84  CJW   If any error then delete the segment
*     01/03/86  MGC   Send pick id. to all active workstations
*     20/01/87  ARG   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP  ISGNO  segment name
*
      INTEGER ISGNO
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    RGN: regenerate
*     Modify /SL/     KSTRWK, KSFAWK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkerr.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkfls.cmn'
*
*  LOCALS
*  ------
*     IARRAY Integer array for build/inquire segment directory
*     RARRAY Real array for build/inquire segment directory
*     XFORM  Real array - data initialised holding default transform
*     IHOLD  Temporary error number
*
      INTEGER IARRAY(KSGISZ),I,IHOLD
      REAL    RARRAY(KSGRSZ),XFORM(6)
      DATA    XFORM / 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 /
*
*  ERRORS
*  ------
*      3    GKS in wrong state.
*    120    Invalid segment name.
*    121    Segment name already in use.
*
*----------------------------------------------------------------------
*     -- validate call & argument
      CALL GKPRLG(ECRSG,GWSAC,GWSAC)
      IF (KERROR.NE.0) GOTO 999

      IF (ISGNO.LE.0) THEN
         KERROR = 120
         GOTO 999
      ENDIF
      IF (KSGLST.EQ.KNIL) THEN
         CALL GKDRCR(KSGSLN,KSGISZ,KSGRSZ,KSGLST)
         IF (KERROR.NE.0) GO TO 999
      ELSE
         CALL GKDRGE(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
         IF (KERROR.EQ.0) THEN
            KERROR = 121
            GOTO 999
         ELSEIF (KERROR .EQ. -1017) THEN
            KERROR = 0
         ELSE
            GOTO 999
         ENDIF
      ENDIF
*
*     -- validation OK, update F/E  then send to w/s --
*     =================================================
      IARRAY(KSGVIS) = GVISI
      IARRAY(KSGHLT) = GNORML
      IARRAY(KSGDTE) = GUNDET
      IARRAY(KSGNWS) = KNACWK
      DO 100 I=0,5
         RARRAY(KSGTRN+I) = XFORM(I+1)
  100 CONTINUE
      RARRAY(KSGPRI) = 0.0
      CALL GKDRPU(KSGLST,ISGNO,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF( KERROR.NE.0 ) GOTO 999

      KCVIS = GVISI
      KCHLT = GNORML
      KWI1  = ISGNO
      QWR1  = 0.0
      KSGRQ = 0
      CALL GKSACW(KCRSG,1,KDAT,1,QDAT,QDAT,1,CH)
      IF( KERROR.NE.0 ) GOTO 980
      KNUMSG = KNUMSG + 1
      IF (KSGRQ.NE.0) THEN
*        -- if CSS not already open then open it --
         IF( KCSFLS.NE.KFLOP ) THEN
           CALL GKCSWD(KOPWK, 1,KDAT, 1,QDAT,QDAT, 1,CH)
           IF( KERROR.NE.0 ) GOTO 980
         ENDIF
*        -- activate CSS --
         KNACWK = KNACWK + 1
         KACPT(KNACWK)  = KCSWIX
*        -- create segment in CSS (remembering to set up WCA again) --
         KWI1=ISGNO
         QWR1=0.0
         CALL GKCSWD(KCRSG,1,KDAT,1,QDAT,QDAT,1,CH)
         IF (KERROR.NE.0) THEN
            KNACWK = KNACWK - 1
            GO TO 980
         ENDIF
      ENDIF
*     -- send pick identifier to active wkstns (may include CSS)
      KWI1=KCPCID
      CALL GKSACW(KSPKID,1,KDAT,1,QDAT,QDAT,1,CH)
*     -- set catch up flags --
      KSTRWK = KHANGE
      KSPLWK = KHANGE
      KSPMWK = KHANGE
      KSTXWK = KHANGE
      KSFAWK = KHANGE
*     -- update name of open segment and GKS operating state --
      KOPSG  = ISGNO
      KOPS   = GSGOP
      GOTO 999

*   Here to delete segment if we get an error after it's been created
*   GKSACW is used instead of a segment interface 'cos
*   a) segment is M/T b) CSS is not active
  980 CONTINUE
      IHOLD=KERROR
      CALL GKDRDE(KSGLST,ISGNO)
      CALL GKSACW(KCLSG,1,KDAT,1,QDAT,QDAT,1,CH)
      KWI1=ISGNO
      CALL GKSACW(KDSG,1,KDAT,1,QDAT,QDAT,1,CH)
      KERROR=IHOLD

*
  999 CONTINUE
      IF( KERROR.NE.0) CALL GKERR(KERROR)
      END
