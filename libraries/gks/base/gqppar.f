C# IL>=a, OL>=0
      SUBROUTINE GQPPAR (IWTYPE,IPA,NMX,MMX,IER,N,M,IPARR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PREDEFINED PATTERN REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns predefined pattern representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     22/01/87  JCS   IS conversion. Error number changes.
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE - workstation type
*     INP IPA    - pattern index
*     INP NMX,MMX- maximum pattern array dimensions
*     OUT IER    - error indicator
*     OUT N,M    - pattern array dimensions
*     OUT IPARR  - pattern array
*
      INTEGER IWTYPE, IPA, NMX, MMX, IER, N, M, IPARR(*)
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
*  LOCALS
*  ------
*
      INTEGER IL
*
*  ERRORS
*  ------
*     85  Specified pattern index is invalid.
*  2001  Specified array size is too small
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (NMX.LE.0 .OR. MMX.LE.0) THEN
          IER = 2001

        ELSEIF (IPA.LE.0) THEN
          IER = 85

        ELSE

          KWI1 = IPA
          KWI2 = NMX
          KWI3 = MMX
          IL = NMX*MMX
          CALL GKSONW(IWTYPE,KQPPAR,IL,IPARR,1,QDAT,QDAT,1,CH)
          IER = KERROR
          IF (KERROR.EQ.0) THEN
            N  = KWI1
            M = KWI2
          IF (N .GT. NMX .OR. M .GT. MMX) IER = 2001
          ENDIF

        ENDIF

      ELSE

        IER = KERROR

      ENDIF

      END
