C# IL>=a, OL>=0
      SUBROUTINE GQPMF (IWTYPE,NTH,IER,NMT,MT,NMS,RNOMMS,RMSMIN,
     :                     RMSMAX,NPPMI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE POLYMARKER FACILITIES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns polymarker facilities
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
*     OUT NMT     number of available marker types
*     OUT MT      Nth element of list of available marker types
*     OUT NMS     number of available marker sizes
*     OUT RNOMMS  nominal marker size
*     OUT RMSMIN  minimum marker size
*     OUT RMSMAX  maximum marker size
*     OUT NPPMI   number of predefined polymarker indices
*
      INTEGER IWTYPE, NTH, IER, NMT, MT, NMS, NPPMI
      REAL RNOMMS, RMSMIN, RMSMAX
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
*     2002  List element requested for marker type out of range
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        KWI1 = NTH
        CALL GKSONW(IWTYPE,KQPMF,1,KDAT,1,QDAT,QDAT,1,CH)
        IER = KERROR
        IF ((KERROR.EQ.0).OR.(KERROR.EQ.2002)) THEN
          NMT = KWI1
          NMS = KWI3
          RNOMMS = QWR1
          RMSMIN = QWR2
          RMSMAX = QWR3
          NPPMI = KWI4
*         Make sure reporting of error 2002 is correct and
*         output marker type.
          CALL GKLERR (NTH,NMT,KWI2,MT,IER)
        ENDIF
      ELSE
        IER = KERROR
      ENDIF

      END
