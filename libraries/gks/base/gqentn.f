C# IL>=a, OL>=0
      SUBROUTINE GQENTN (NTH, IER, LEN, NELMNT)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE LIST ELEMENT OF NORMALIZATION
*                      TRANSFORMATION NUMBERS
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Nth element of list of normalization transformation numbers
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/83  AS   Original version stabilized
*     10/06/84  CJW  Correct use of NTH
*     22/01/87  JCS   IS conversion. Error number changed.
*     27/09/90  KEVP  Changed error checking to conform to GKS FORTRAN
*                     BINDING standard (C41).
*
*  ARGUMENTS
*  ---------
*     IN    NTH    list element requested
*     OUT   IER    Error indicator
*     OUT   LEN    Number of Normalization Transformation Numbers
*     OUT   NELMNT Nth element of list of normalization transformation numbers
*
      INTEGER NTH, IER, LEN, NELMNT
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gksl.cmn'
*
*  ERRORS
*  ------
*     2002  List element not available
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IER = KERROR
      IF (KERROR .EQ. 0) THEN
        LEN = KT+1
        NELMNT = KNIL
        IF (NTH.GE.1 .AND. NTH.LE.KT+1) THEN
          NELMNT = KTNOVP(NTH-1)
        ELSEIF(NTH.EQ.0) THEN
          IER = 0
        ELSE
          IER = 2002
        ENDIF
      ENDIF

      END
