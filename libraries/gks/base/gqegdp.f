C# IL>=a, OL>=0
      SUBROUTINE GQEGDP (IWTYPE,NTH,IER,NGDP,LGDP)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE LIST ELEMENT OF AVAILABLE GENERALIZED
*                      DRAWING PRIMITIVES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns Nth element of list of available GDP's.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS    Original version stabilized
*     27/09/90  KEVP  Changed error checking to conform to GKS FORTRAN
*                     BINDING standard (C41). See comments in GKQXXI.
*
*  ARGUMENTS
*  ---------
*     INP IWTYPE  workstation type
*     INP NTH     list element requested
*     OUT IER     error indicator
*     OUT NGDP    no of available GDP's
*     OUT LGDP    nth element of list of available GDP's
*
      INTEGER IWTYPE, NTH, IER, NGDP, LGDP
*
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
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)

*  GKQXXI can not be called, 'cos it calls GKPRLG (KNIL,GWSOP,GSGOP).
      IF (KERROR.EQ.0) THEN
        KWI1 = NTH
        CALL GKSONW(IWTYPE,KQEGDP,1,KDAT,1,QDAT,QDAT,1,CH)
        IF (KERROR .EQ. 0 .OR. KERROR .EQ. 2002) THEN
            NGDP = KWI1
            LGDP = KNIL
            IF((NTH.EQ.0).OR.(NGDP.EQ.0))THEN
               KERROR = 0
            ELSEIF((NTH.LT.0).OR.(NGDP.LT.NTH))THEN
               KERROR = 2002
            ELSEIF(KERROR.EQ.0)THEN
               LGDP = KWI2
            ENDIF
        ENDIF
      ENDIF

        IER = KERROR

      END
