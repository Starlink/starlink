C# IL>=a, OL>=0
      SUBROUTINE GQTXF (IWTYPE,NTH,IER,NFPP,IFONT,IPREC,NCHH,
     :                     RMINCH,RMAXCH,NCHF,RMINF,RMAXF,NPTXI)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE TEXT FACILITIES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns text facilities
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
*     OUT NFPP    number of text font and precision pairs
*     OUT IFONT   Nth element of list of text fonts
*     OUT IPREC   Nth element of list of text precisions
*     OUT NCHH    number of available character heights
*     OUT RMINCH  minimum character height
*     OUT RMAXCH  maximum character height
*     OUT NCHF    number of available character expansion factors
*     OUT RMINF   minimum character expansion factor
*     OUT RMAXF   maximum character expansion factor
*     OUT NPTXI   number of predefined text indices
*
      INTEGER IWTYPE, NTH, IER, NFPP, IFONT, IPREC, NCHH, NCHF, NPTXI
      REAL RMINCH, RMAXCH, RMINF, RMAXF
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
*     2002  List element requested for font-precision pair out of range
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

      IF (KERROR.EQ.0) THEN
        KWI1 = NTH
        CALL GKSONW(IWTYPE,KQTXF,1,KDAT,1,QDAT,QDAT,1,CH)
        IER = KERROR
        IF ((KERROR.EQ.0).OR.(KERROR.EQ.2002)) THEN
          NFPP   = KWI1
          NCHH   = KWI4
          RMINCH = QWR1
          RMAXCH = QWR2
          NCHF   = KWI5
          RMINF  = QWR3
          RMAXF  = QWR4
          NPTXI  = KWI6
*         Make sure reporting of error 2002 is correct and
*         output font-precision pair
          CALL GKLERR (NTH,NFPP,KWI3,IPREC,IER)
          IF(IPREC .NE. KNIL)THEN
             IFONT = KWI2
          ELSE
             IFONT = 0
          ENDIF
        ENDIF
      ELSE
        IER = KERROR
      ENDIF

      END
