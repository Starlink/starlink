C# IL>=a, OL>=1
      SUBROUTINE GQPAR (IWKID,IPA,ITYPE,NMX,MMX,IER,N,M,IPARR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE PATTERN REPRESENTATION
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns pattern representation.
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     22/01/87  JCS   IS conversion. Error numbers changed.
*
*  ARGUMENTS
*  ---------
*     INP IWKID  - workstation identifier
*     INP IPA    - pattern index
*     INP ITYPE  - type of returned values
*     INP NMX,MMX- maximum pattern array dimensions
*     OUT IER    - error indicator
*     OUT N,M    - pattern array dimensions
*     OUT IPARR  - pattern array
*
      INTEGER IWKID, IPA, ITYPE, NMX, MMX, IER, N, M, IPARR(*)
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
*  LOCALS
*  ------
*
      INTEGER IL
*
*  ERRORS
*  ------
*     85  Specified pattern index is invalid.
*   2001  Specified array size is too small
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GWSOP,GSGOP)

      IF (KERROR.EQ.0) THEN

        IF (NMX.LE.0 .OR. MMX.LE.0) THEN
          IER = 2001

        ELSEIF (IPA.LE.0) THEN
          IER = 85

        ELSE

          KWI1 = IPA
          KWI2 = ITYPE
          KWI3 = NMX
          KWI4 = MMX
          IL = NMX*MMX
          CALL GKSONW(IWKID,KQPAR,IL,IPARR,1,QDAT,QDAT,1,CH)
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
