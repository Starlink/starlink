C# IL>=a, OL>=0
      SUBROUTINE GKSGWK(IENT,ERSBLE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Segment workstation entrypoints
*
*  MAINTENANCE LOG
*  ---------------
*     08/12/83  AS    Original version stabilized
*      11/1/84  JRG   Correct computed GOTO expression
*     13/02/84  JGW   Check KERROR before CSS requests.
*     12/03/84  CJW   Close workstation now does nothing
*                     Various changed due to mods to SL package
*     29/03/84  JRG   Correct compilation error
*      11/4/84  JRG   For Delete Seg, put correct values in KWI2
*     17/04/84  RSK   Modified 430,440,500 to call 'SLGE'. Temporary till
*                     new SL package installed.
*     09/05/85  PGLS  Change seg pri type to real (ISGPRI -> SEGPRI)
*     20/01/87  ARG   IS conversion. Error number changed.
*     03/03/88  KEVP  Add BBOX to enable use with bounding boxes (S315).
*
*  ARGUMENTS
*  ---------
*     INP IENT   - Entrypoint code
*     INP ERSBLE - TRUE if workstation can attempt to erase a single segment
*
      INTEGER IENT
      LOGICAL ERSBLE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     BBOX    Bounding box of segment
*     ISGNO   Segment name
*     SEGPRI  Segment priority
*
      INTEGER ISGNO
      REAL    SEGPRI, BBOX(4)
*
*---------------------------------------------------------------------




* Conditional GOTO on entrypoint code

      GOTO (410,420,430,440,450,460,470,480,490,500,510) IENT-KCRSG+1

      GOTO 9999



* Create segment
  410 CONTINUE
      IF (KSSGPT(KWKIX).EQ.KNIL) CALL GKSLCR(KNOBOX,KSSGPT(KWKIX))
      IF (KERROR.EQ.0) THEN
         CALL GKSLPU(KSSGPT(KWKIX),KWI1,QWR1,QDAT)
         IF (KERROR.EQ.0) KSGRQ = KSGRQ + 1
      END IF
      GOTO 9999



* Close segment
  420 CONTINUE
      GOTO 9999



* Rename segment
  430 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SEGPRI,BBOX)
      IF (KWI1.EQ.ISGNO) THEN
        CALL GKSLNM(KSSGPT(KWKIX),KWI1,KWI2)
        IF (KERROR.EQ.-1017) THEN
           KERROR = 0
        ELSE IF (KERROR.EQ.0) THEN
           KSGRQ = KSGRQ + 1
        ENDIF
      ENDIF
      GOTO 9999



* Delete segment
  440 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SEGPRI,BBOX)
      IF (KWI1.EQ.ISGNO) THEN
* segment identified ok
        CALL GKSLDS(KSSGPT(KWKIX),KCURR)
        IF (KERROR.EQ.-1017) THEN
           KWI2 = GABSNT
           KERROR = 0
        ELSE IF (KERROR.EQ.0) THEN
*          segment identified ok
           KWI2 = GPRSNT
           KSGRQ  = KSGRQ + 1
           IF (ERSBLE) KRPCC  = KRPVIS
           KRPTYP = KRPERA
           KRPSG  = KWI1
           GOTO 8888
        ENDIF
      ELSE
        KWI2 = GABSNT
      ENDIF
      GOTO 9999



* Begin segment
  450 CONTINUE
      GOTO 9999



* End segment
  460 CONTINUE
      IF (KINENT .EQ. KRSGWK) THEN
        CALL GKSLGE(KSSGPT(KWKIX),KRPSG,ISGNO,SEGPRI,BBOX)
        CALL GKSLGE(KSSGPT(KWKIX),KHIER,KRPSG,SEGPRI,BBOX)
        IF( KERROR.EQ.0 .AND. KRPSG.NE.KNIL ) KRPCC=KRPVIS
      ENDIF
      GOTO 9999



* Set segment transformation
  470 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SEGPRI,BBOX)
      IF (KWI1.EQ.ISGNO .AND. KWI3.EQ.GNEMPT .AND. KWI4.EQ.GVISI)  THEN
        IF (ERSBLE) KRPCC  = KRPVIS
        KRPSG  = KWI1
        IF (KWI5.EQ.1) THEN
          KRPTYP = KRPERA
        ELSE
          KRPTYP = KRPDRW
          GOTO 8888
        ENDIF
      ENDIF
      GOTO 9999



* Set visibility
  480 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SEGPRI,BBOX)
      IF (KWI1.EQ.ISGNO .AND. KWI3.NE.GEMPTY) THEN
        IF (KWI4.EQ.GVISI) THEN
          KRPCC  = KRPYES
          KRPTYP = KRPDRW
          KRPSG  = KWI1
        ELSE
          IF (ERSBLE) THEN
            KRPCC  = KRPYES
            KRPTYP = KRPERA
            KRPSG  = KWI1
          ENDIF
          GOTO 8888
        ENDIF
      ENDIF
      GOTO 9999



* Set highlighting
  490 CONTINUE
      GOTO 9999



* Set segment priority
  500 CONTINUE
      CALL GKSLGE(KSSGPT(KWKIX),KWI1,ISGNO,SEGPRI,BBOX)
      IF (KWI1.EQ.ISGNO) THEN
        CALL GKSLPR(KSSGPT(KWKIX),KWI1,QWR1)
        IF (KERROR.EQ.0) THEN
          IF (KWI3.EQ.GNEMPT .AND. KWI4.EQ.GVISI) GOTO 8888
        ELSEIF (KERROR.EQ.-1017) THEN
           KERROR = 0
        ENDIF
      ENDIF
      GOTO 9999



* Set detectability
  510 CONTINUE
      GOTO 9999



 8888 CONTINUE
      IF (KIMRGM(KWKIX).EQ.GALLOW) THEN
* If regenerate immediate then suppress the playback
        KRPCC = KRPNO
        KRGN  = .TRUE.
        KWRGN(KWKIX) = .TRUE.
      ELSE
        KNFAUP(KWKIX) = GYES
      ENDIF


 9999 CONTINUE
      END
