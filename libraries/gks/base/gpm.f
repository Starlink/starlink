C# IL>=a, OL>=0
      SUBROUTINE GPM(N,PX,PY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  POLYMARKER
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     A sequence of markers is generated to identify all the given
*     positions. The current values of the polymarker attributes are
*     bound to the primitive. A marker is visible if and only if the
*     marker position is within the clipping rectangle.
*
*  MAINTENANCE LOG
*  ---------------
*     18/02/83  AS    Original version stabilized
*     18/03/83  AS    Put in aspect stuff
*     26/04/83  AS    Change error check for 100 from N.LT.2 to LT.1
*     20/02/86  DCS   Replace call to GKSCTG by its contents.
*     19/01/87  DCS   IS conversion. Update COMMON block usage.
*
*  ARGUMENTS
*  ---------
*     INP N  - number of polymarker points
*     INP PX - x-coordinates of points
*     INP PY - y-coordinates of points
*
      INTEGER N
      REAL PX(*),PY(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKS/    wsac,sgop
*     Read   /GKWKE/  KPM
*     Read   /GKWCA/  KDAT,KRGN
*     Modify /GKYSL/  KSTRWK,KSPMWK
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     100   number of points invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (EPM,GWSAC,GSGOP)
      IF (KERROR.NE.0) GOTO 888
      IF ( N.LT.1 ) THEN
        CALL GKERR(100)
      ELSE
        IF (KSTRWK.NE.KGKSFN) THEN
          CALL GKCCTG
          CALL GKSACW(KNT, 1,KDAT, 1,QDAT,QDAT,1,CH)
          IF (KERROR.NE.0) GOTO 888
          KSTRWK=KGKSFN
        ENDIF
        IF (KSPMWK.NE.KGKSFN) THEN
          CALL GKCPMG
          CALL GKSACW(KSPMA,1,KDAT,1,QDAT,QDAT,1,CH)
          IF (KERROR.NE.0) GOTO 888
          KSPMWK = KGKSFN
        ENDIF
        CALL GKSACW(KPM,1,KDAT,N,PX,PY,1,CH)
        IF (KERROR.NE.0) GOTO 888
        IF (KRGN) THEN
          CALL GKRGN
          IF (KERROR.NE.0) GOTO 888
        ENDIF
      ENDIF
      GOTO 999

  888 CONTINUE
      CALL GKERR(KERROR)
  999 CONTINUE
      END
