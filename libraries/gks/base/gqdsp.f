C# IL>=a, OL>=0
      SUBROUTINE GQDSP(IWTYPE,IER,IDCUNI,CX,CY,LX,LY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE MAXIMUM DISPLAY SURFACE SIZE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns maximum display surface size
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     09/03/87 JCS    IS Conversion. Routine name changed.
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     OUT IER    - error indicator
*     OUT IDCUNI - device coordinate units
*     OUT CX,CY  - max display surface size in DC
*     OUT LX,LY  - max display surface size in raster units
*
      INTEGER IWTYPE, IER, IDCUNI, LX, LY
      REAL CX,CY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        CALL GKSONW(IWTYPE,KQMDS,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KERROR.EQ.0) THEN
          IDCUNI = KWI1
          CX = QWR1
          CY = QWR2
          LX = KWI2
          LY = KWI3
        ENDIF
      ENDIF

      IER = KERROR

      END
