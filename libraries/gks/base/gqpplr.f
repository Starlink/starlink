C# IL>=a, OL>=0
      SUBROUTINE GQPPLR (IWTYPE,IPL,IER,LNTYPE,WIDTH,IC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PREDEFINED POLYLINE REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns predefined polyline representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     INP IPL    - polyline index
*     OUT IER    - error indicator
*     OUT LNTYPE - linetype
*     OUT WIDTH  - linewidth scale factor
*     OUT IC     - polyline colour index
*
      INTEGER IWTYPE, IPL, IER, LNTYPE, IC
      REAL WIDTH
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     60   Polyline index is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (IPL.LE.0) THEN
          IER = 60
        ELSE
          KWI1 = IPL
          CALL GKSONW(IWTYPE,KQPPLR,1,KDAT,1,QDAT,QDAT,1,CH)
          IER = KERROR
          IF (KERROR.EQ.0) THEN
            LNTYPE = KWI1
            IC = KWI2
            WIDTH = QWR1
          ENDIF

        ENDIF

      ELSE
        IER = KERROR
      ENDIF

      END
