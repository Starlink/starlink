C# IL>=b, OL>=0
      SUBROUTINE GRQCH (IWKID,ICHDNR,ISTAT,ICHNR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  REQUEST CHOICE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Performs a REQUEST on the specified CHOICE device.
*
*  MAINTENANCE LOG
*  ---------------
*      4/05/83  AS   Original version stabilized
*      7/12/83  AS   Check for device number < 1
*     30/05/90  KEVP Set status to KNIL for input device error (S271)
*
*  ARGUMENTS
*  ---------
*     INP IWKID  workstation identifier
*     INP ICHDNR choice device number
*     OUT ISTAT  status
*     OUT ICHNR  choice number
*
      INTEGER IWKID, ICHDNR, ISTAT, ICHNR
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/GKSE_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     140  Input device number is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (ERQCH,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        ISTAT = KNIL
        IF (ICHDNR.GE.1) THEN
          KWI1 = ICHDNR
          CALL GKSONW(IWKID,KRQCH,1,KDAT,1,QDAT,QDAT,1,CH)
          IF (KERROR.EQ.0) THEN
            ISTAT = KWI1
            ICHNR = KWI2
          ELSE
            CALL GKERR(KERROR)
          ENDIF
        ELSE
          CALL GKERR(140)
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF

      END
