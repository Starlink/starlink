C# IL>=a, OL>=1
      SUBROUTINE GQTXR (IWKID,ITX,ITYPE,IER,IFONT,IPREC,XP,SP,IC)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE TEXT REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns text representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     22/01/87  JCS   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP IWKID  - workstation identifier
*     INP ITX    - text index
*     INP ITYPE  - type of returned values
*     OUT IER    - error indicator
*     OUT IFONT  - text font
*     OUT IPREC  - text precision
*     OUT XP     - character expansion factor
*     OUT SP     - character spacing
*     OUT IC     - text colour index
*
      INTEGER IWKID, ITX, ITYPE, IER, IFONT, IPREC, IC
      REAL XP, SP
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
*     72  Text index is invalid
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (ITX.LE.0) THEN
          IER = 72
        ELSE
          KWI1 = ITX
          KWI2 = ITYPE
          CALL GKSONW(IWKID,KQTXR,1,KDAT,1,QDAT,QDAT,1,CH)
          IER = KERROR
          IF (KERROR.EQ.0) THEN
            IFONT = KWI1
            IPREC = KWI2
            IC = KWI3
            XP = QWR1
            SP = QWR2
          ENDIF
        ENDIF

      ELSE
        IER = KERROR
      ENDIF

      END
