C# IL>=a, OL>=1
      SUBROUTINE GKPIKP(XPN,YPN,PKAP,XFTSUB,XPPSUB,NSEG,NPKID,NPRIM,
     :                  ITYPE)
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Given a point selected by the user (the pick point)
*     determine the Segment name and Pick ID
*     appropiate to the point
*
*     If there is no pick the segment name is returned as KNIL
*
*  MAINTENANCE LOG
*  ---------------
*     25/02/88  KEVP  Stabilised
*     07/04/88  KEVP  In transforming bounding box made routine ignore
*                     error 152, ie, bounding box may extend outside of
*                     NDC rectangle.
*     06/01/89  KEVP  Changed name from GKLCPK to GKPIKP, removed
*                     external XSRSUB segment read routine, to use GKCSRD
*                     added primitive number NPRIM to arguments
*     09/01/89  KEVP  Pick-Point Argument put into NDC (previously DC)
*                     Primitive Type added to arguments
*     16/01/89  KEVP  Changed bounding boxes from WC(=NDC) to DC
*     11/05/89  KEVP  Moved check on bounding boxes, so that it is only
*                     done for segments that are detectable and visible.
*     16/11/89  RMK   Removed unused local variables.
*
*  ARGUMENTS
*  ---------
*     INP  XPN,YPN  The Pick Point in NDC
*     INP  PKAP     The Pick apperture in DC units
*     INP  XFTSUB   Font details routine
*     INP  XPPSUB   Primitive pick routine
*     OUT  NSEG     Segment Name
*     OUT  NPKID    Pick ID
*     OUT  NPRIM    Primitive Number
*     OUT  ITYPE    Primitive Type (KPL,KPM,KTX,KFA,KCA,KGDP)
*
      REAL XPN,YPN, PKAP
      EXTERNAL XFTSUB, XPPSUB
      INTEGER NSEG, NPKID, NPRIM, ITYPE
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkwdt.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     BOXDEF  True, if bounding box needs to be defined
*     BOUND   Bounding box for segment (XMIN,XMAX,YMIN,YMAX)
*     DIRHIT  True, if direct hit in primitive from current segment
*     DIST    Distance between pick point and nearest primitive
*     IATTS,RATTS  Segment attributes for the segment currently
*                  being examined for pick
*     IBOUND  Bounding box flag
*     ICTYPE  Current primitive type
*     IDET    Local copy of segment detectability
*     IER     Local copy of error number for open segment enquiry
*     IHOLD   Local copy of KERROR. Needed because more than error
*             condition may occur and we want the first.
*     ISTAT   Status from some routines
*     IVIS    Local copy of segment visibility
*     IWA,RWA Local arrays saving Workstation Communication Area
*     NCPKID  Pick ID obtained from current segment
*     NCPRIM  Number of primitive obtained from current segment
*     NCSEG   Name of current segment being examined
*     NOPSG   Name of open segment, if one exists
*     PREHIT  True, if hit in previous segment
*     PRI     Priority of current segment
*     UFORM   Indentity matrix
*     XP,YP   Pick point in DC

      INTEGER IWA(KNKWI), IATTS(KSGISZ), IHOLD, ISTAT, NCPKID,NCPRIM
      REAL    RWA(KNQWR), RATTS(KSGRSZ), XP,YP
      INTEGER NCSEG , IVIS, IDET, J, IBOUND, IER, NOPSG, ICTYPE
      REAL    BOUND(4), PRI, UFORM(6), DIST
      LOGICAL PREHIT, BOXDEF, DIRHIT

*
*  STACK USAGE
*  -----------
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
*  COMMENTS
*  --------
*     This utility assumes that the following apply
*     to the workstation driver, whenever a
*     software segment is being played back.
*
*     - The set text attributes entry-point KSTXA
*       executes GKDTXB and does nothing else that
*       effects the text attributes in GKWKD CMN
*       except colour index.
*
*     - The set marker attributes entry-point KSPMA
*       executes GKDPMB and does nothing else that
*       effects the marker type and size in GKWKD CMN.
*
*     - The normalisation entry-point KNT
*       executes GKWKC4 and does nothing else that
*       effects QWTOTT.
*
*     If the workstation uses bounding boxes for segments,
*     they must be specified in Device Coordinates
*     with segment transformation
*     and in the form XMIN,XMAX,YMIN,YMAX.
*     Time can be saved by using bounding boxes.
*
*---------------------------------------------------------------------
*
*     Set Initial Value of Segment Name and distance
      NSEG = KNIL
      DIST = PKAP
*
*     Convert pick point to DC
      CALL GKTND (1,XPN,YPN,XP,YP)
      IF(KERROR .NE. 0)GOTO 999
*
*   Save Workstation Communication area in local arrays
      DO 100 J=1,KNKWI
  100   IWA(J)=KWIA(J)
      DO 110 J=1,KNQWR
  110   RWA(J)=QWRA(J)

*   Get bounding box flag
      CALL GKSLBF (KSSGPT(KWKIX),IBOUND)
      BOXDEF = (IBOUND .EQ. KBOX)
*   Find segment of highest priority
      CALL GKSLGE (KSSGPT(KWKIX),KHIEST,NCSEG,PRI,BOUND)

*   Loop over each segment going in order of priority downwards
*     ----------
*     Begin Loop
*     ----------
  200 CONTINUE

*     Find segment in CSS.
      CALL GKCSSL(NCSEG,ISTAT)
      IF( ISTAT.EQ.KRFUSE ) THEN
        CALL GKBUG(-2012,'GKPIKP')
        GOTO 999
      ENDIF

*   Here, segment NCSEG exists in CSS.
*   Now get the segment attributes.
      IF( KSGLST.NE.KNIL ) THEN
        CALL GKDRGE(KSGLST,NCSEG, KSGISZ,KSGRSZ, IATTS,RATTS)
      ELSE
      KERROR = -2007
      ENDIF
      IF( KERROR.NE.0 ) GOTO 980

*     Only scan this segment if it is
*     both visible and detectable
      IVIS=IATTS(KSGVIS)
      IDET=IATTS(KSGDTE)
      IF((IVIS .EQ. GVISI) .AND. (IDET .EQ. GDETEC)) THEN

*        Eliminate segment
*           if its bounding box has been defined,
*           and the pick point is outside of bounding box
*               or segment is empty.
         IF(IBOUND .EQ. KBOX)THEN
           IF(BOUND(1) .LE. BOUND(2))THEN
             IF(BOUND(3) .LE. BOUND(4))THEN
*              No need to define bounding box
               BOXDEF = .FALSE.
*              See if pick pt is outside bounding box
*              and not within the pick apperture of it.
               IF(XP .LT. BOUND(1) - PKAP) GOTO 800
               IF(XP .GT. BOUND(2) + PKAP) GOTO 800
               IF(YP .LT. BOUND(3) - PKAP) GOTO 800
               IF(YP .GT. BOUND(4) + PKAP) GOTO 800
             ELSE
*              Segment is defined but empty
               GOTO 800
             ENDIF
           ELSE
*            Bounding box is undefined and needs defining,
*            but this is not done if the segment is open
*            because further primitives could be added.
             IHOLD = KERROR
             CALL GQOPSG (IER,NOPSG)
             BOXDEF = ((IER .EQ. 4) .OR. (NOPSG .NE. NCSEG))
             KERROR = IHOLD
           ENDIF
         ENDIF

*        Here - either  Pick point is inside bounding box
*                   or  bounding box is undefined
*                   or  bounding boxes are not in use.

*        Now scan segment to determine distance of pick point
*        from primitives and pick ID
*        Emerge on end of segment (or error).
         PREHIT = (NSEG .NE. KNIL)
         CALL GKPPSG(XP,YP,UFORM,RATTS(KSGTRN),XFTSUB,XPPSUB,PREHIT,
     :               BOXDEF,DIST,NCPKID,NCPRIM,ICTYPE,DIRHIT)
         IF( KERROR.NE.0 ) THEN
*          Need to release segment.
*          KERROR saved first. This conceals any error arising from
*          GKCSRL, but there won't be any (only thing there
*          could be is a bug).
             IHOLD=KERROR
             CALL GKCSRL
             IF(KERROR .EQ. 0)KERROR=IHOLD
             GOTO 999
         ENDIF
*      If a primitive has been picked - set output arguments
         IF (NCPKID .NE. KNIL) THEN
           NSEG  = NCSEG
           NPKID = NCPKID
           NPRIM = NCPRIM
           ITYPE = ICTYPE
*          If direct hit - quit loop
           IF(DIRHIT)GOTO 900
         ENDIF
      ENDIF

  800 CONTINUE
*     Deal with next lower segment,  if it exists , else quit loop
      CALL GKSLGE (KSSGPT(KWKIX),KLOER,NCSEG,PRI,BOUND)
      IF(KERROR .NE. 0) GOTO 900
      IF(NCSEG .NE. KNIL) GOTO 200
*     -----------
*     End of Loop
*     -----------

*   Restore W.C.A. ... first restore general purpose variables
  900 CONTINUE
      DO 910 J=1,KNKWI
  910   KWIA(J)=IWA(J)
      DO 920 J=1,KNQWR
  920   QWRA(J)=RWA(J)

      GOTO 999
*       Bug report ... workstation is asking for segment that's not in
*                      Segment State List
  980 CALL GKBUG (-2007,'GKPIKP')
*
  999 CONTINUE
      RETURN
      END
