C# IL>=a, OL>=1
      SUBROUTINE GKPKPB(IWKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Either: to select segment and return segment attributes.
*     Or: to playback all or part of segment to a workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC  Created
*     20/01/87  PKY   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     INP  IWKID   Workstation Identifier
*
      INTEGER IWKID
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /SL/     Use pointer for Segment State List
*     Modify /WCA/    Read parameters for segment playback (KRPCC, KRPTYP,
*                     KRPSG, KINENT). Set KCVIS & KCHLT explicitly at
*                     beginning of segment; also some KWIn (n=1,2....).
*                     QWRA11..16 when segment transformation requested.
*     Modify /ERR/    KERROR passed on to caller. Modified when the
*                     saved value (in IHOLD) is restored. Effect though
*                     is to preserve the value of the first error to
*                     be detected.
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkpca.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkpc.cmn'
      INCLUDE '../include/gkpca.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     I       Loop counter
*     IHOLD   Local copy of KERROR. Needed because more than error
*             condition may occur and we want the first.
*     ISTAT   Status from some routines
*     IDUM    Dummy argument for routine call
*     IATTS,RATTS  Segment attributes for the segment
*     UFORM   Identity matrix
*
      INTEGER I,IHOLD,ISTAT,IDUM
      INTEGER IATTS(KSGISZ)
      REAL    RATTS(KSGRSZ)
      REAL    UFORM(6)
*
*  EXTERNALS
*  ---------
*     GKPCRD  Utility to read CSS segment
*     GKSONW  Call Layer link to workstation
*
      EXTERNAL GKPCRD,GKSONW
*
*  DATA
*  ----
*
      DATA UFORM / 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 /
*
*  ERRORS
*  ------
*    -2007 Entry not found in list
*   -2012 Segment does not exist on CSS
*    -2015 CSS not open
*
*---------------------------------------------------------------------

* Store paramaters appropriate to action requested
      KPCRQ=KWI1
      KPCSG=KWI2
      IF(KPCRQ.EQ.KPECHO .OR. KPCRQ.EQ.KPUNEC) THEN
         KPCID=KWI3
         KPCPI=KWI4
         KPCPET=KWI5
         KPCPT=KWI6
      ENDIF

* Check CSS is open
      IF(KCSFLS.NE.KFLOP) THEN
        KERROR=-2015
        GOTO 999
      ENDIF

* Find segment in CSS
      CALL GKCSSL(KPCSG,ISTAT)
      IF(ISTAT.EQ.KRFUSE) THEN
        CALL GKBUG(-2012,'GKPKPB')
        GOTO 999
      ENDIF

* Here, segment exists in CSS. Now get the segment attributes.
      IF( KSGLST.NE.KNIL ) THEN
        CALL GKDRGE(KSGLST,KPCSG,KSGISZ,KSGRSZ,IATTS,RATTS)
      ELSE
*      (set KERROR to any non-zero value)
        KERROR=1
      ENDIF
      IF( KERROR.NE.0 ) GOTO 980

* Decide if segment attributes are required (scan) or
* if echoplay (echo or unecho) is to be performed.
      IF(KPCRQ.EQ.KPSCAN) THEN
        KWI3=KPCSG
*       segment is pickable if visible and detectable
        IF(IATTS(KSGVIS).EQ.GVISI .AND.
     :     IATTS(KSGDTE).EQ.GDETEC) THEN
*         pickable
          KWI4=GDETEC
        ELSE
*         not pickable
          KWI4=GUNDET
        ENDIF
*       transformation
        DO 10 I=0,5
          QWRA(11+I)=RATTS(KSGTRN+I)
 10     CONTINUE

*       set echoplay mode (scan)
        KPKECO=KPCRQ

      ELSEIF(KPCRQ.EQ.KPECHO .OR. KPCRQ.EQ.KPUNEC) THEN

* Initialise data for echoplay
        KPCMOR=KNIL
        KPCNID=0
        KPCNP=0

* Set echoplay mode (echo or unceho)
        KPKECO=KPCRQ

* Now copy the contents of the segment. Emerge on end
* of segment (or error).
        CALL GKSGCN(IWKID,UFORM,RATTS(KSGTRN),
     :                 GKPCRD,GKSONW,.TRUE.)
        IF( KERROR.NE.0 ) THEN
* Need to release segment.
* KERROR saved first. This conceals any error arising from
* GKCSRL, but there won't be any (only thing there
* could be is a bug).
          IHOLD=KERROR
          CALL GKCSRL
          KERROR=IHOLD
          GOTO 999
        ELSEIF( KPCRQ.EQ.KPSTOP ) THEN
* Release segment if echoplay stopped
          CALL GKCSRL
        ENDIF

* Output buffer
        CALL GKDDA(IWKID,IDUM)
      ENDIF
      GOTO 999

* Bug report ... workstation is asking for segment that's not in
*                Segment State List
 980  CALL GKBUG(-2007,'GKPKPB')
*
 999  CONTINUE
      END
