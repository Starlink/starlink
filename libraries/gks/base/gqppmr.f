C# IL>=a, OL>=0
      SUBROUTINE GQPPMR (IWTYPE,IPM,IER,MKTYPE,SF,IC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PREDEFINED POLYMARKER REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns predefined polymarker representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     22/01/87  JCS   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     INP IPM    - polymarker index
*     OUT IER    - error indicator
*     OUT MKTYPE - marker type
*     OUT SF     - marker size scale factor
*     OUT IC     - polymarker colour index
*
      INTEGER IWTYPE, IPM, IER, MKTYPE, IC
      REAL SF
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
*     66  Polymarker index is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (IPM.LE.0) THEN
          IER = 66
        ELSE
          KWI1 = IPM
          CALL GKSONW(IWTYPE,KQPPMR,1,KDAT,1,QDAT,QDAT,1,CH)
          IER = KERROR
          IF (KERROR.EQ.0) THEN
            MKTYPE = KWI1
            IC = KWI2
            SF = QWR1
          ENDIF

        ENDIF

      ELSE
        IER = KERROR
      ENDIF

      END
