C# IL>=a, OL>=2
      SUBROUTINE GASGWK (IWKID,ISGNAM)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  ASSOCIATE SEGMENT WITH WORKSTATION
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Copies a segment from WISS to the WDSS of the specified
*     workstation in the same way as if the workstation were
*     active when the segment was created.
*     Clipping rectangles are copied unchanged.
*     This function cannot be invoked when a segment is open.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilized
*     01/03/86  MGC   Unlock send segment detectability code
*                     and set current visibility and highlighting.
*     16/03/88  RMK   Changed error 1017 to -1017 (S316).
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP ISGNAM  segment name
*
      INTEGER IWKID, ISGNAM
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /FLS/ KCSFLS
*     Read   /SL/  KSGLST
*     Read   /WCB/ KWKC,KWKID,KOPPT
*     Modify /WCA/ KSGRQ,KWI1..KWI5,QWR1..QWR6;
*                   set KCVIS & KCHLT prior to playback.
*     Modify /ERR/ KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
*
*  LOCALS
*  ------
*     LWKCSS : .TRUE. if workstation requests CSS
*     I      : loop control etc
*     IERR   : temporary store for error code
*     IWSSID : WISS workstation identifier
*     IARRAY : segment directory integer array
*     RARRAY : segment directory real array
*
      LOGICAL LWKCSS
      INTEGER I,IERR
      INTEGER IWSSID
      INTEGER IARRAY(KSGISZ)
      REAL    RARRAY(KSGRSZ)
      REAL    UFORM(6)
*
*  EXTERNALS
*  ---------
*     GKSONW : call layer interface to one open workstation
*     GKSGOC : playback interface to workstation and CSS
*
      EXTERNAL GKSONW, GKSGOC
*
*  DATA
*  ----
*     UFORM  : identity matrix
*
      DATA    UFORM / 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 /
*
*  ERRORS
*  ------
*      6    GKS not in proper state: should be WSOP or WSAC
*     20    Specified workstation identifier is invalid
*     25    Specified workstation is not open
*     27    Workstation independent segment storage is not open
*    120    Specified segment name is invalid
*    124    Specified segment name does not exist on WISS
*
*  COMMENTS
*  --------
*   If KERROR=123, following the inquiry on the WISS workstation,
*   then KERROR=124 is reported to indicate that the segment
*   does not exit in WISS.
*   KERROR=124 is also reported for the unlikely case where the
*   segment exits in WISS, but the segment directory is empty or
*   does not contain an entry for the segment.
*   If KERROR=0, following the inquiry on the specified workstation,
*   then the segment is already associated with that workstation,
*   and no futher action is needed (consider when the workstation
*   is also WISS).
*
*---------------------------------------------------------------------

*     GKS prolog
      CALL GKPRLG (EASGWK,GWSOP,GWSAC)
      IF (KERROR.NE.0) GOTO 999

*     check specified workstation identifier is valid
      IF(IWKID.LT.1) THEN
        KERROR=20
        GOTO 999
      ENDIF

*     check specified workstation open
      DO 100 I=1,KNOPWK
        IF( KWKID(KOPPT(I)).EQ.IWKID ) GOTO 110
 100  CONTINUE
      KERROR=25
      GOTO 999
 110  CONTINUE

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

*     ... and is not already associated with specified workstation
      KWI1=KREL
      KWI2=ISGNAM
      KSGRQ=0
      CALL GKSONW(IWKID,KQSGWK,1,KDAT,1,QDAT,QDAT,1,CH)
      IF(KERROR.NE.123) GOTO 999
      KERROR=0
*
* --------------end of standard error checking---------------------

*     obtain segment attributes
      IF(KSGLST.NE.KNIL)
     :   CALL GKDRGE(KSGLST,ISGNAM,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF(KERROR.EQ.-1017 .OR. KSGLST.EQ.KNIL) KERROR=124
      IF(KERROR.NE.0) GOTO 999

*     create segment on specified workstation
      KWI1  = ISGNAM
      QWR1  = RARRAY(KSGPRI)
      KSGRQ = 0
      CALL GKSONW(IWKID,KCRSG,1,KDAT,1,QDAT,QDAT,1,CH)
      IF(KERROR.NE.0) GOTO 999
*     remember if workstation requested CSS
      LWKCSS = KSGRQ.NE.0

*     send set segment transformation to workstation
      KWI1=ISGNAM
      KWI2=KSNOPN
      KWI3=GNEMPT
      KWI4=IARRAY(KSGVIS)
      KWI5=2
      QWR1=RARRAY(KSGTRN)
      QWR2=RARRAY(KSGTRN+1)
      QWR3=RARRAY(KSGTRN+2)
      QWR4=RARRAY(KSGTRN+3)
      QWR5=RARRAY(KSGTRN+4)
      QWR6=RARRAY(KSGTRN+5)
      CALL GKSONW(IWKID,KSSGT,1,KDAT,1,QDAT,QDAT,1,CH)
      IF(KERROR.NE.0) GOTO 980

*     send segment visibilty if different from default
      IF(IARRAY(KSGVIS).NE.GVISI) THEN
        KWI1=ISGNAM
        KWI2=KSNOPN
        KWI3=GEMPTY
        KWI4=IARRAY(KSGVIS)
        CALL GKSONW(IWKID,KSVIS,1,KDAT,1,QDAT,QDAT,1,CH)
        IF(KERROR.NE.0) GOTO 980
      ENDIF

*     send segment highlighting if different from default
      IF(IARRAY(KSGHLT).NE.GNORML) THEN
        KWI1=ISGNAM
        KWI2=KSNOPN
        KWI3=GEMPTY
        KWI4=IARRAY(KSGVIS)
        KWI5=IARRAY(KSGHLT)
        IF(KERROR.NE.0) GOTO 980
      ENDIF

*     send segment detectability if different from default
      IF(IARRAY(KSGDTE).NE.GUNDET) THEN
        KWI1=ISGNAM
        KWI2=KSNOPN
        KWI3=GEMPTY
        KWI4=IARRAY(KSGVIS)
        KWI5=IARRAY(KSGDTE)
        CALL GKSONW(IWKID,KSDTEC,1,KDAT,1,QDAT,QDAT,1,CH)
        IF(KERROR.NE.0) GOTO 980
      ENDIF

*     set visibility and highlighting variables in WCA
      KCVIS=IARRAY(KSGVIS)
      KCHLT=IARRAY(KSGHLT)

*     involve CSS in segment playback if requested by workstation
      IF (LWKCSS) THEN

*        if CSS not already open then open it
         IF( KCSFLS.NE.KFLOP ) THEN
           CALL GKCSWD(KOPWK,1,KDAT,1,QDAT,QDAT,1,CH)
           IF(KERROR.NE.0) GOTO 980
         ENDIF

*        send create segment to CSS (maybe +1 to client count)
         KWI1=ISGNAM
         QWR1=RARRAY(KSGPRI)
         KSGRQ=1
         CALL GKCSWD(KCRSG,1,KDAT,1,QDAT,QDAT,1,CH)
         IF(KERROR.NE.0) GO TO 980

*        replay only to workstation if segment already in CSS
         IF(KDAT(1) .GT. 1) THEN
           CALL GKWSPB(IWKID,ISGNAM,UFORM,RARRAY(KSGTRN),
     :                    GKSONW,.TRUE.)
         ELSE
*          replay to workstation and also to CSS (because new segment)
           CALL GKWSPB(IWKID,ISGNAM,UFORM,RARRAY(KSGTRN),
     :                    GKSGOC,.TRUE.)
*          close the segment in CSS
           CALL GKCSWD(KCLSG,1,KDAT,1,QDAT,QDAT,1,CH)
         ENDIF
*        if error send delete segment to CSS (maybe -1 to client count)
         IF(KERROR.NE.0) THEN
           KWI1=ISGNAM
           KSGRQ=1
           CALL GKCSWD(KDSG,1,KDAT,1,QDAT,QDAT,1,CH)
         ENDIF
      ELSE
*       replay to workstation without assistance from CSS
        CALL GKWSPB(IWKID,ISGNAM,UFORM,RARRAY(KSGTRN),
     :                 GKSONW,.TRUE.)
      ENDIF

*     close segment on workstation (regardless of error)
  980 CONTINUE
      IERR=KERROR
      CALL GKSONW(IWKID,KCLSG,1,KDAT,1,QDAT,QDAT,1,CH)
*     delete segment if earlier error
      IF(IERR.NE.0) THEN
        KWI1=ISGNAM
        CALL GKSONW(IWKID,KDSG,1,KDAT,1,QDAT,QDAT,1,CH)
      ENDIF
      KERROR=IERR

*     update segment client count in central register unless error
 999  IF(KERROR.EQ.0) THEN
        IARRAY(KSGNWS)=IARRAY(KSGNWS)+1
        CALL GKDRPU(KSGLST,ISGNAM,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      ELSE
        CALL GKERR(KERROR)
      ENDIF

*     restore standard visibility and highlighting
      KCVIS=GVISI
      KCHLT=GNORML

*     set state of output attributes on workstation
      CALL GKSGAT(IWKID)

      RETURN
      END
