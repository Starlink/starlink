C# IL>=a, OL>=0
      SUBROUTINE GQPLF (IWTYPE,NTH,IER,NLT,LT,NLW,RNOMLW,RLWMIN,
     :                     RLWMAX,NPPLI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE POLYLINE FACILITIES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns polyline facilities
*
*  MAINTENANCE LOG
*  ---------------
*     15/02/83  AS    Original version stabilized
*     30/09/83  AS    FORTRAN binding changes
*     24/10/90  KEVP  Put in correct handling of error 2002 (C55)
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE  workstation type
*     INP NTH     list element requested
*     OUT IER     error indicator
*     OUT NLT     number of available linetypes
*     OUT LT      Nth element of list of available linetypes
*     OUT NLW     number of available linewidths
*     OUT RNOMLW  nominal linewidth
*     OUT RLWMIN  minimum linewidth
*     OUT RLWMAX  maximum linewidth
*     OUT NPPLI   number of predefined polyline indices
*
      INTEGER IWTYPE, NTH, IER, NLT, LT, NLW, NPPLI
      REAL RNOMLW, RLWMIN, RLWMAX
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
*     2002  List element requested for line type out of range
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IF (KERROR.EQ.0) THEN
        KWI1 = NTH
        CALL GKSONW(IWTYPE,KQPLF,1,KDAT,1,QDAT,QDAT,1,CH)
        IER = KERROR
        IF ((KERROR.EQ.0) .OR. (KERROR.EQ.2002)) THEN
          NLT = KWI1
          NLW = KWI3
          RNOMLW = QWR1
          RLWMIN = QWR2
          RLWMAX = QWR3
          NPPLI = KWI4
*         Make sure reporting of error 2002 is correct and
*         output line type.
          CALL GKLERR (NTH,NLT,KWI2,LT,IER)
        ENDIF
      ELSE
        IER = KERROR
      ENDIF

      END
