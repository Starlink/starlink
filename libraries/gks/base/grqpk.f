C# IL>=b, OL>=0
      SUBROUTINE GRQPK (IWKID,IPKDN,ISTAT,ISGNAM,IPKID)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  REQUEST PICK
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Performs a REQUEST on the specified PICK device.
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC  Created
*     30/05/90  KEVP Set status to KNIL for input device error (S271)
*
*  ARGUMENTS
*  ---------
*     INP IWKID   workstation identifier
*     INP IPKDN   locator device number
*     OUT ISTAT   status
*     OUT ISGNAM  segment name
*     OUT IPKID   pick identifier
*
      INTEGER IWKID, IPKDN, ISTAT, ISGNAM, IPKID
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
*     None
*
*  ERRORS
*  ------
*     140  Input device number is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (ERQPK,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        ISTAT = KNIL
        IF (IPKDN.GE.1) THEN

          CALL GKPKRQ(IWKID,IPKDN,ISTAT,ISGNAM,IPKID)

          IF (KERROR.NE.0) THEN
            CALL GKERR(KERROR)
          ENDIF

        ELSE
          CALL GKERR(140)
        ENDIF

      ELSE
        CALL GKERR(KERROR)
      ENDIF

      END
