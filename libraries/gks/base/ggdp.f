C# IL>=a, OL>=0
      SUBROUTINE GGDP(NPTS,RX,RY,ID,LDR,DATREC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  GENERALIZED DRAWING PRIMITIVE
*  Author:             JGW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Front end for GDP.
*
*  MAINTENANCE LOG
*  ---------------
*     13/12/83  JGW   Original version stabilized
*     20/12/83  AS    Rewrite (just about)
*     20/02/86  DCS   Replace call to GKSCTG by its contents.
*     19/01/87  DCS   IS conversion. Update COMMON block usage.
*     19/01/87  ARG   IS conversion. Report GKPRLG errors. Error numbers
*                     changed.
*     30/05/90  KEVP  Check GDP indentifier for validity (S228).
*
*  ARGUMENTS
*  ---------
*     INP   NPTS    Number co-ordinates.
*     INP   RX }    Co-ordinates.
*     INP   RY }
*     INP   ID      GDP identifier.
*     INP   LDR     Size of data record.
*     INP   DATREC  Data record.
*
      INTEGER NPTS, ID, LDR
      REAL    RX(*),RY(*)
      CHARACTER*(*) DATREC(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /GKYSL/  KSTRWK,KSPLWK,KSPMWK,KSTXWK,KSFAWK
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
*
*  LOCALS
*  ------
      INTEGER LENGTH
*
*  ERRORS
*  ------
*     100   Number of points invalid
*     102   Invalid GDP identifier
*    2001   Dimension of data record < 1
*    2003   Data record not CHARACTER*80
*
*---------------------------------------------------------------------

      CALL GKPRLG(EGDP,GWSAC,GSGOP)
      IF (KERROR.NE.0) GOTO 8888

      LENGTH = LEN(DATREC(1))
      IF(ID .EQ. 0)THEN
        CALL GKERR(102)
      ELSEIF (NPTS.LE.0) THEN
        CALL GKERR(100)
      ELSEIF (LDR.LE.0) THEN
        CALL GKERR(2001)
      ELSEIF (LENGTH.NE.80) THEN
        CALL GKERR(2003)
      ELSE


* catchup output attributes
        IF (KSTRWK.NE.KGKSFN) THEN
          CALL GKCCTG
          CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
          IF (KERROR.NE.0) GOTO 8888
          KSTRWK=KGKSFN
        ENDIF

        IF (KSPLWK.NE.KGKSFN) THEN
          CALL GKCPLG
          CALL GKSACW(KSPLA,1,KDAT,1,QDAT,QDAT,1,CH)
          IF (KERROR.NE.0) GOTO 8888
          KSPLWK = KGKSFN
        ENDIF

        IF (KSPMWK.NE.KGKSFN) THEN
          CALL GKCPMG
          CALL GKSACW(KSPMA,1,KDAT,1,QDAT,QDAT,1,CH)
          IF (KERROR.NE.0) GOTO 8888
          KSPMWK = KGKSFN
        ENDIF

        IF (KSTXWK.NE.KGKSFN) THEN
          CALL GKCTXG
          CALL GKSACW(KSTXA,1,KDAT,1,QDAT,QDAT,1,CH)
          IF (KERROR.NE.0) GOTO 8888
          KSTXWK = KGKSFN
        ENDIF

        IF (KSFAWK.NE.KGKSFN) THEN
          CALL GKCFAG
          CALL GKSACW(KSFAA,1,KDAT,1,QDAT,QDAT,1,CH)
          IF (KERROR .NE. 0) GOTO 8888
          KSFAWK=KGKSFN
        ENDIF

* send GDP request to w/s
        KWI1 = ID
        QWR1 = 1.0
        QWR2 = 0.0
        QWR3 = 1.0
        QWR4 = 0.0
        QWR5 = 1.0
        QWR6 = 1.0
        CALL GKSACW(KGDP,1,KDAT,NPTS,RX,RY,LDR,DATREC)
        IF (KERROR.NE.0) GOTO 8888
        GOTO 9999
      ENDIF

 8888 CONTINUE
      CALL GKERR(KERROR)

 9999 CONTINUE
      END
