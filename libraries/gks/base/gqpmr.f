C# IL>=a, OL>=1
      SUBROUTINE GQPMR (IWKID,IPM,ITYPE,IER,MKTYPE,SF,IC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE POLYMARKER REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns polymarker representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     22/01/87  JCS   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP IWKID  - workstation identifier
*     INP IPM    - polymarker index
*     INP ITYPE  - type of returned values
*     OUT IER    - error indicator
*     OUT MKTYPE - marker type
*     OUT SF     - marker size scale factor
*     OUT IC     - polymarker colour index
*
      INTEGER IWKID, IPM, ITYPE, IER, MKTYPE, IC
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


      CALL GKPRLG (KNIL,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (IPM.LE.0) THEN
            IER = 66
        ELSE
          KWI1 = IPM
          KWI2 = ITYPE
          CALL GKSONW(IWKID,KQPMR,1,KDAT,1,QDAT,QDAT,1,CH)
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
