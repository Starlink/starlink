C# IL>=a, OL>=2
      SUBROUTINE GINSG (ISGNAM,TRAMAT)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INSERT SEGMENT
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Allows previously stored primitives (in segments in WISS)
*     to be transformed and again placed into the stream of
*     output primitives.
*     The transformed primitives are sent to all active workstations,
*     including (in state SGOP) those with segments open.
*     All clipping rectangles in the inserted segment are ignored.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilized
*     21/01/87  ARG   IS conversion. Language binding has transposed
*                     matrix. Error numbers changed.
*
*  ARGUMENTS
*  ---------
*     INP ISGNAM  source segment name
*     INP TRAMAT  transformation matrix
*
      INTEGER ISGNAM
      REAL    TRAMAT (2,3)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /OPS/ KOPS
*     Read   /SL/  KOPSG,KSGLST
*     Read   /WCB/ KWKC,KWKID,KOPPT
*     Modify /WCA/ KSGRQ,KWI1..KWI5,QWI1..QWI16
*     Modify /ERR/ KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkops.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
*
*  LOCALS
*  ------
*     I      : loop control etc
*     IWSSID : WISS workstation identifier
*     IARRAY : dummy segment integer array
*     RISARY : insert segment real array
*     ROSARY : open segment real array
*     TRNC2  : combined insert transformation
*     UFORM  : identity matrix
*     XFORM  : transposed matrix
*
      INTEGER I
      INTEGER IWSSID
      INTEGER IARRAY(KSGISZ)
      REAL    RISARY(KSGRSZ),ROSARY(KSGRSZ)
      REAL    TRNC2(6)
      REAL    UFORM(6)
      REAL    XFORM (6)
*
*  EXTERNALS
*  ---------
*     GKSONW : call layer interface to one open workstation
*     GKSGAC : playback interface to active workstations
*
      EXTERNAL GKSONW, GKSGAC
*
*  DATA
*  ----
*
      DATA    UFORM / 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 /
*
*  ERRORS
*  ------
*      5    GKS not in proper state: should be WSAC or SGOP
*     27    Workstation independent segment storage is not open
*    120    Specified segment name is invalid
*    124    Specified segment name does not exist on WISS
*    125    Specified segment is open
*  -2007    Bug - cannot find directory segment for open segment
*
*  COMMENTS
*  --------
*  The combined insert transformation (C2) for the inserted
*  segment is obtained from MxS,
*  where:  M - transformation supplied via matrix parameter
*          S - stored transformation of specified segment
*
*  If KERROR=123, following the inquiry on the WISS workstation,
*  then KERROR=124 is reported to indicate that the segment
*  does not exit in WISS.
*  KERROR=124 is also reported for the unlikely case where a
*  segment exits in WISS, but the segment directory is empty or
*  does not contain an entry for the segment.
*
*---------------------------------------------------------------------

*     GKS prolog
      CALL GKPRLG (EINSG,GWSAC,GSGOP)
      IF (KERROR.NE.0) GOTO 999

*     check WISS open, and obtain its identifier
      DO 200 I=1,KNOPWK
        IF( KWKC(KOPPT(I)).EQ.GWISS ) THEN
          IWSSID = KWKID(KOPPT(I))
          GOTO 210
        ENDIF
 200  CONTINUE
      KERROR=27
      GOTO 999
 210  CONTINUE

*     validate segment name
      IF(ISGNAM.LT.1) THEN
        KERROR=120
        GOTO 999
      ENDIF

*     inquire to confirm that segment exists in WISS ...
      KWI1=KREL
      KWI2=ISGNAM
      KSGRQ=0
      CALL GKSONW(IWSSID,KQSGWK,1,KDAT,1,QDAT,QDAT,1,CH)
      IF(KERROR.EQ.123) KERROR=124
      IF(KERROR.NE.0) GOTO 999

*     ... and is not the open segment
      IF(KOPS.EQ.GSGOP .AND. ISGNAM.EQ.KOPSG) THEN
        KERROR=125
        GOTO 999
      ENDIF
*
* --------------end of standard error checking---------------------

*     obtain segment attributes
      IF(KSGLST.NE.KNIL)
     :   CALL GKDRGE(KSGLST,ISGNAM,KSGISZ,KSGRSZ,IARRAY,RISARY)
      IF(KERROR.EQ.-1017 .OR. KSGLST.EQ.KNIL) KERROR=124
      IF(KERROR.NE.0) GOTO 999

*     transpose matrix
      XFORM (1) = TRAMAT (1,1)
      XFORM (2) = TRAMAT (1,2)
      XFORM (3) = TRAMAT (1,3)
      XFORM (4) = TRAMAT (2,1)
      XFORM (5) = TRAMAT (2,2)
      XFORM (6) = TRAMAT (2,3)

*     transformationC2 <- combined insert transformation (MxS)
      CALL GKMTML(RISARY(KSGTRN),XFORM,TRNC2)

*     playback segment from wiss sending to all active workstations
      IF(KOPS.EQ.GSGOP) THEN
        CALL GKDRGE(KSGLST,KOPSG,KSGISZ,KSGRSZ,IARRAY,ROSARY)
        IF (KERROR .NE. 0) CALL GKBUG (-2007,'GINSG')
*       transformationt3 is open segment transformation
        CALL GKWSPB(KNIL,ISGNAM,TRNC2,ROSARY(KSGTRN),
     :                 GKSGAC,.FALSE.)
      ELSE
*       transformationt3 is unity matrix when segment not open
        CALL GKWSPB(KNIL,ISGNAM,TRNC2,UFORM,GKSGAC,.FALSE.)
      ENDIF

 999  IF(KERROR.NE.0) CALL GKERR(KERROR)

*     set state of output attributes on workstation(s)
      CALL GKSGAT(KNIL)

      RETURN
      END
