C# IL>=a, OL>=0
      SUBROUTINE GQPXA(IWKID,PX,PY,IDIMX,IDIMY,ISC,ISR,IDX,IDY,
     :                 IER,INVVAL,ICOLIA)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PIXEL ARRAY
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User Level Inquire Pixel Array routine
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/83 NGB  Original version stabilized
*     17/02/83 PGLS Insert call to PRLG
*     13/06/83 AS   Miscellaneous
*     24/11/83 AS   Add extra argument IDIMX
*     10/01/83 AS   Should make sure transformation is up to date
*     20/02/86 DCS  Send transformation data to this workstation only
*                   and restore values afterwards (cf GQTXX).
*     21/01/87  KWB   IS conversion. Revised language binding and error
*                     trapping. Extended workstation interface.
*     22/01/87  DCS   IS conversion. Remove metafile index.
*
*  ARGUMENTS
*  ---------
*     INP   IWKID   Workstation Identifier
*     INP   PX,PY   Origin World Coordinates
*     INP   IDIMX   First dimension of colour index array
*     INP   IDIMY   Second dimension of colour index array
*     INP   ISC     Index of start column
*     INP   ISR     Index of start row
*     INP   IDX,IDY Size of requested pixel array
*     OUT   IER     Error Indicator
*     OUT   INVVAL  Invalid Values Present
*     OUT   ICOLIA  Pixel Array
*
      INTEGER IWKID,IDIMX,IDIMY,ISC,ISR,IDX,IDY,IER,INVVAL,
     :        ICOLIA(IDIMX,IDIMY)
      REAL PX,PY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     91   Dimensions of colour array are invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)
      IER = KERROR
      IF (IER .NE. 0) RETURN

      IF (IDIMX .LT. 1 .OR. IDIMY .LT. 1 .OR.
     :    ISC .LT. 1 .OR. ISR .LT. 1 .OR.
     :    IDX .LT. 1 .OR. IDY .LT. 1 .OR.
     :    ISC+IDX-1 .GT. IDIMX .OR. ISR+IDY-1 .GT. IDIMY) THEN
        IER = 91
      ELSE

* Send transformation data

        CALL GKCCTG
        CALL GKSONW(IWKID,KNT,1,KDAT,1,QDAT,QDAT,1,CH)
        IER = KERROR
        IF (IER.NE.0) RETURN

        QWR1 = PX
        QWR2 = PY
        KWI1 = IDIMX
        KWI2 = IDIMY
        KWI3 = ISC
        KWI4 = ISR
        KWI5 = IDX
        KWI6 = IDY
        CALL GKSONW(IWKID,KQPXA,IDIMX*IDIMY,ICOLIA,1,QDAT,QDAT,1,CH)
        IER = KERROR
        IF (IER .EQ. 0) INVVAL = KWI7
      ENDIF

* Send transformation data to correspond to 'last source' flag if
* necessary. (Correspond to KGKSFN at present and so only send for value
* of KMI).

      IF (KSTRWK .EQ. KMI) THEN
        CALL GKCCTM
        CALL GKSONW(IWKID,KNT,1,KDAT,1,QDAT,QDAT,1,CH)
      ENDIF

      END
