C# IL>=a, OL>=1
      SUBROUTINE GQPLR (IWKID,IPL,ITYPE,IER,LNTYPE,WIDTH,IC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE POLYLINE REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*    Returns polyline representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKID  - workstation identifier
*     INP IPL    - polyline index
*     INP ITYPE  - type of returned values
*     OUT IER    - error indicator
*     OUT LNTYPE - linetype
*     OUT WIDTH  - linewidth scale factor
*     OUT IC     - polyline colour index

      INTEGER IWKID, IPL, ITYPE, IER, LNTYPE, IC
      REAL WIDTH
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
*  ERRORS
*  ------
*     60   Polyline index is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (IPL.LE.0) THEN
          IER = 60
        ELSE
          KWI1 = IPL
          KWI2 = ITYPE
          CALL GKSONW(IWKID,KQPLR,1,KDAT,1,QDAT,QDAT,1,CH)
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
