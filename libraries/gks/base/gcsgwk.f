C# IL>=a, OL>=2
      SUBROUTINE GCSGWK (IWKID,ISGNAM)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  COPY SEGMENT TO WORKSTATION
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Copies primitives from a segment in WISS to be output on
*     the specified workstation.
*     The primitives are sent to the workstation after segment
*     transformation and clipping at the clipping rectangle
*     stored with each primitive.
*     This function cannot be invoked when a segment is open.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilized
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
*     Read   /SL/  KSGLST
*     Read   /WCB/ KWKC,KWKID,KOPPT
*     Modify /WCA/ KSGRQ,KWI1..KWI5
*     Modify /ERR/ KERROR
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkssl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwcb.cmn'
*
*  LOCALS
*  ------
*     I      : loop control etc
*     IERR   : temporary store for error code
*     IWSSID : WISS workstation identifier
*     IARRAY : segment directory integer array
*     RARRAY : segment directory real array
*     UFORM  : identity matrix
*
      INTEGER I,IERR
      INTEGER IWSSID
      INTEGER IARRAY(KSGISZ)
      REAL    RARRAY(KSGRSZ)
      REAL    UFORM(6)
*
*  EXTERNALS
*  ---------
*     GKSONW : call layer interface to one open workstation
*
      EXTERNAL GKSONW
*
*  DATA
*  ----
*
      DATA    UFORM / 1.0, 0.0, 0.0, 0.0, 1.0, 0.0 /
*
*  ERRORS
*  ------
*      6    GKS not in proper state: should be WSOP or WSAC
*     20    Specified workstation identifier is invalid
*     25    Specified workstation is not open
*     27    Workstation independent segment storage is not open
*     36    Specified workstation is WISS
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
*
*---------------------------------------------------------------------

*     GKS prolog
      CALL GKPRLG (ECSGWK,GWSOP,GWSAC)
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

*     check workstation is not also WISS
      IF(IWKID.EQ.IWSSID) THEN
        KERROR=36
        GOTO 999
      ENDIF

*     validate segment name
      IF(ISGNAM.LT.1) THEN
        KERROR=120
        GOTO 999
      ENDIF

*     inquire to confirm that segment exists in WISS
      KWI1=KREL
      KWI2=ISGNAM
      KSGRQ=0
      CALL GKSONW(IWSSID,KQSGWK,1,KDAT,1,QDAT,QDAT,1,CH)
      IF(KERROR.EQ.123) KERROR=124
      IF(KERROR.NE.0) GOTO 999
*
* --------------end of standard error checking---------------------

*     obtain segment attributes
      IF(KSGLST.NE.KNIL)
     :   CALL GKDRGE(KSGLST,ISGNAM,KSGISZ,KSGRSZ,IARRAY,RARRAY)
      IF(KERROR.EQ.-1017 .OR. KSGLST.EQ.KNIL) KERROR=124
      IF(KERROR.NE.0) GOTO 999

*     send begin segment to workstation, supplying default attributes
      KWI1=ISGNAM
      KWI2=GVISI
      KWI3=GNORML
      KWI4=GUNDET
      CALL GKSONW(IWKID,KBGSG,1,KDAT,1,QDAT,QDAT,1,CH)
      IF(KERROR.NE.0) GOTO 999

*     playback segment from wiss to workstation
      CALL GKWSPB(IWKID,ISGNAM,RARRAY(KSGTRN),UFORM,
     :               GKSONW,.TRUE.)

*     send end segment to workstation (regardless of error)
      IERR=KERROR
      CALL GKSONW(IWKID,KENSG,1,KDAT,1,QDAT,QDAT,1,CH)
      KERROR=IERR

 999  IF(KERROR.NE.0) CALL GKERR(KERROR)

*     set state of output attributes on workstation
      CALL GKSGAT(IWKID)

      RETURN
      END
