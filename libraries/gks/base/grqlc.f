C# IL>=b, OL>=0
      SUBROUTINE GRQLC (IWKID,LCDNR,ISTAT,ITNR,PX,PY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  REQUEST LOCATOR
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Performs a REQUEST on the specified LOCATOR device.
*
*  MAINTENANCE LOG
*  ---------------
*     04/05/83  AS    Original version stabilized
*     05/08/83  AS    Check initial locator position
*     07/12/83  AS    Check for device number < 1
*     13/07/88  RMK   Set transformation number to KNIL if have
*                     break (S328).
*     30/05/90  KEVP  Set status to KNIL for input device error (S271)
*     22/07/90  PLP   Removed unused locals I, IT.
*
*  ARGUMENTS
*  ---------
*     INP IWKID  workstation identifier
*     INP LCDNR  locator device number
*     OUT ISTAT  status
*     OUT ITNR   normalization transformation number
*     OUT PX,PY  locator position
*
      INTEGER IWKID, LCDNR, ISTAT, ITNR
      REAL PX, PY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkse.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwke.par'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
      REAL XWC(1), YWC(1), XNDC(1), YNDC(1)
*
*  ERRORS
*  ------
*     140  Input device number is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (ERQLC,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN
* Set status in case of error
        ISTAT = KNIL
        IF (LCDNR.GE.1) THEN

* Call inquire locator device to get initial position and transformation
* number so that initial position can be checked.
          KWI1 = LCDNR
          KWI2 = GSET
          CALL GKSONW(IWKID,KQLCS,1,KDAT,1,XWC,YWC,1,CH)
* Transform locator position to NDC
          CALL GKTWN(KWI4,1,XWC,YWC,XNDC,YNDC)
          IF (KERROR.NE.0) THEN
            XNDC(1) = 0.5
            YNDC(1) = 0.5
            KERROR = 0
          ENDIF
          KWI1 = LCDNR
          CALL GKSONW(IWKID,KRQLC,1,KDAT,1,XNDC,YNDC,1,CH)

          IF (KERROR.EQ.0) THEN
            ISTAT = KWI1
            IF (ISTAT.EQ.GNONE) THEN
* Have break action
              ITNR = KNIL
              PX = XNDC(1)
              PY = YNDC(1)
            ELSE
* Transform NDC to WC
              CALL GKTNW(1,XNDC,YNDC,XWC,YWC,ITNR)
              PX = XWC(1)
              PY = YWC(1)
            ENDIF
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
