C# IL>=b, OL>=0
      SUBROUTINE GRQSK (IWKID,ISKDNR,N,ISTAT,ITNR,NP,PX,PY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  REQUEST STROKE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Performs a REQUEST on the specified STROKE device.
*
*  MAINTENANCE LOG
*  ---------------
*     04/05/83  AS    Original version stabilized
*     24/08/83  AS    Check initial stroke
*     07/12/83  AS    Check for device number < 1
*     04/12/83  AS    Put in check for initial stroke = 0
*     21/01/87  PKY   IS conversion. Error number changes.
*     30/05/90  KEVP  Set status to KNIL for input device error (S271)
*     22/07/90  PLP   Removed unused locals I and J.
*
*  ARGUMENTS
*  ---------
*     INP IWKID  workstation identifier
*     INP ISKDNR stroke device number
*     INP N      maximum number of points
*     OUT ISTAT  status
*     OUT ITNR   normalization transformation number
*     OUT NP     number of points
*     OUT PX,PY  locator position
*
      INTEGER IWKID, ISKDNR, N, ISTAT, ITNR, NP
      REAL PX(*), PY(*)
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

*
*  ERRORS
*  ------
*    2001  Array dimension insufficient
*     140  Input device number is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG(ERQSK,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN
* Set status in case of error
        ISTAT = KNIL
        IF (ISKDNR.GE.1) THEN

* Check number of points is valid
          IF (N.GT.0) THEN
* Call inquire stroke device to get initial stroke and transformation
* number so that initial stroke can be checked.
            KWI1 = ISKDNR
            KWI2 = GSET
            CALL GKSONW(IWKID,KQSKS,1,KDAT,N,PX,PY,1,CH)
* Transform initial stroke to NDC
            IF (KNRR.GT.0) CALL GKTWN(KWI4,KNRR,PX,PY,PX,PY)
            IF (KERROR.NE.0) THEN
              KNRR = 0
              KERROR = 0
            ENDIF
            KWI1 = ISKDNR
            CALL GKSONW(IWKID,KRQSK,1,KDAT,N,PX,PY,1,CH)
            IF (KERROR.EQ.0) THEN
              ISTAT = KWI1
              NP = KNRR
* Transform NDC to WC
              IF (NP.GT.0) CALL GKTNW(NP,PX,PY,PX,PY,ITNR)
            ELSE
              CALL GKERR(KERROR)
            ENDIF
          ELSE
            CALL GKERR(2001)
          ENDIF
        ELSE
          CALL GKERR(140)
        ENDIF
      ELSE
        CALL GKERR(KERROR)
      ENDIF

      END
