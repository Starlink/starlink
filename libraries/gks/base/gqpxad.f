C# IL>=a, OL>=0
      SUBROUTINE GQPXAD(IWKID,PX,PY,QX,QY,IER,IDX,IDY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PIXEL ARRAY DIMENSIONS
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level Inquire Pixel Array Dimensions routine
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83  NGB   Original version stabilized
*     17/02/83  PGLS  Call to PRLG inserted
*     10/01/83  AS    Should make sure transformation is up to date
*     20/02/86  DCS   Send transformation data to this workstation only
*                     and restore values afterwards (cf GQTXX).
*     22/01/87  DCS   IS conversion. Remove metafile index.
*
*  ARGUMENTS
*  ---------
*     INP IWKID   Workstation Identifier
*     INP PX,PY   World Coordinates of point P
*     INP QX,QY   World Coordinates of point Q
*     OUT IER     Error Indicator
*     OUT IDX,IDY Columns,Rows of Pixels
*
      INTEGER IWKID, IER, IDX, IDY
      REAL PX, PY, QX, QY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------



      CALL GKPRLG (KNIL,GWSOP,GSGOP)
      IER = KERROR
      IF (IER .NE. 0) RETURN

* Send transformation data

      CALL GKCCTG
      CALL GKSONW(IWKID,KNT,1,KDAT,1,QDAT,QDAT,1,CH)
      IER = KERROR
      IF (IER.NE.0) RETURN

      QWR1 = PX
      QWR2 = PY
      QWR3 = QX
      QWR4 = QY
      CALL GKSONW(IWKID,KQPXAD,1,KDAT,1,QDAT,QDAT,1,CH)
      IER = KERROR
      IF (IER.EQ.0) THEN
       IDX = KWI1
       IDY = KWI2
      ENDIF

* Send transformation data to correspond to 'last source' flag if
* necessary. (Correspond to KGKSFN at present and so only send for value
* of KMI).

      IF (KSTRWK .EQ. KMI) THEN
        CALL GKCCTM
        CALL GKSONW(IWKID,KNT,1,KDAT,1,QDAT,QDAT,1,CH)
      ENDIF

      END
