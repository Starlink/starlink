C# IL>=a, OL>=0
      SUBROUTINE GKQINT (IWTYPE,IENT,IER,I1,I2,I3,I4,I5,I6,I7)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    FRONTEND
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Calls workstation to get lists of single integers
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     INP IENT   - workstation entrypoint
*     OUT IER    - error indicator
*     OUT I*     - single integers
*
      INTEGER IWTYPE, IENT, IER, I1, I2, I3, I4, I5, I6, I7
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /GKS/    gkop,sgop
*     Modify /GKWCA/  KERROR,KDAT,QDAT,KWI1-7
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        CALL GKSONW(IWTYPE,IENT,1,KDAT,1,QDAT,QDAT,1,CH)
        IER = KERROR
        IF (KERROR.EQ.0) THEN
          I1 = KWI1
          I2 = KWI2
          I3 = KWI3
          I4 = KWI4
          I5 = KWI5
          I6 = KWI6
          I7 = KWI7
        ENDIF

      ELSE
        IER = KERROR
      ENDIF

      END
