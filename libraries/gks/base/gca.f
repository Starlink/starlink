C# IL>=a, OL>=0
      SUBROUTINE GCA(PX,PY,QX,QY,IDIMX,IDIMY,ISC,ISR,IDX,IDY,ICOLIA)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  CELL ARRAY
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     User level Cell Array primitive routine
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83  NGB   Original version stabilized
*     17/02/83  PGLS  Call to PRLG updated.
*     25/04/83  PGLS  Add transformation catchup.
*     30/06/83  NGB   Add KERROR Changes.
*     04/07/83  PGLS  More KERROR changes.
*     15/09/83  NGB   Introduce three-point w/s interface
*     24/11/83  AS    Add IDIMX to argument list
*     20/02/86  DCS   Replace call to GKSCTG by its contents .
*     20/01/87  KWB   IS conversion. Revised language binding and error
*                     trapping. Changed cell array third point to accord
*                     with that stored on metafile. Extended workstation
*                     interface.
*
*  ARGUMENTS
*  ---------
*     INP   PX,PY   world coordinates of point P
*     INP   QX,QY   world coordinates of point Q
*     INP   IDIMX   first dimension of colour index array
*     INP   IDIMY   second dimension of colour index array
*     INP   ISC     index of start column
*     INP   ISR     index of start row
*     INP   IDX     number of cell columns
*     INP   IDY     number of cell rows
*     INP   ICOLIA  Cell Array
*
      REAL    PX, PY, QX, QY
      INTEGER  IDIMX,IDIMY,ISC,ISR,IDX,IDY,ICOLIA(IDIMX,IDIMY)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /WCA/    Various
*     Modify /SL/     KSTRWK
*     Read   /ERR/    KERROR
*
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     91   Dimensions of colour array are invalid
*
*  ALGORITHM
*  ---------
*     The Cell Array is output between P and Q on all active W/S's
*  Note:
*     Because the W/S entry point must accomodate Cell Array primitives
*  being replayed from a Metafile, which (at level 2) may have been
*  transformed to be no longer axiparallel, the interface requires
*  three points to be passed.  The third point passed is the point
*  [QX,PY] corresponding to the (DX,1) element of the colour index
*  array. Note that P corresponds to the (1,1) element and Q to the
*  (DX,DY) element.
*
*---------------------------------------------------------------------

      CALL GKPRLG(ECA,GWSAC,GSGOP)
      IF (KERROR .NE. 0) GOTO 888
      IF (IDIMX .LT. 1 .OR. IDIMY .LT. 1 .OR.
     :    ISC .LT. 1 .OR. ISR .LT. 1 .OR.
     :    IDX .LT. 1 .OR. IDY .LT. 1 .OR.
     :    ISC+IDX-1 .GT. IDIMX .OR. ISR+IDY-1 .GT. IDIMY)
     :THEN
        CALL GKERR(91)
      ELSE
        IF (KSTRWK.NE.KGKSFN) THEN
          CALL GKCCTG
          CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
          IF (KERROR.NE.0) GOTO 888
          KSTRWK=KGKSFN
        ENDIF

        QWR1=PX
        QWR2=PY

        QWR3=QX
        QWR4=QY

        QWR5=QX
        QWR6=PY

        KWI1=IDIMX
        KWI2=IDIMY

        KWI3=ISC
        KWI4=ISR

        KWI5=IDX
        KWI6=IDY

        CALL GKSACW(KCA,IDIMX*IDIMY,ICOLIA,1,QDAT,QDAT,1,CH)
        IF (KERROR.NE.0) GOTO 888

        IF (KRGN) THEN
          CALL GKRGN
          IF (KERROR.NE.0) GOTO 888
        ENDIF
      ENDIF
      RETURN

  888 CONTINUE
      CALL GKERR(KERROR)

      END
