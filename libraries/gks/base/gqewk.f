C# IL>=a, OL>=0
      SUBROUTINE GQEWK (NTH, IER, NUMBER, IWKTYP)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE LIST ELEMENT OF AVAILABLE WORKSTATION TYPES
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns Nth element of list of available workstation types.
*
*  MAINTENANCE LOG
*  ---------------
*     29/09/82  AS   Original version stabilized
*     22/01/87  JCS   IS conversion. Error number changed.
*     27/09/90  KEVP  Changed error checking to conform to GKS FORTRAN
*                     BINDING standard (C41).
*
*  ARGUMENTS
*  ---------
*     INP NTH    list element requested
*     OUT IER    Error indicator
*     OUT NUMBER Length of list
*     OUT IWKTYP Nth element of list of available workstation types
*
      INTEGER NTH, IER, NUMBER, IWKTYP
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkdt.cmn'
      INCLUDE '../include/gkops.cmn'
*
*  ERRORS
*  ------
*     2002  List element not available
*
*---------------------------------------------------------------------


      CALL GKPRLG (KNIL,GGKOP,GSGOP)
      IER = KERROR
      IF (KERROR .EQ. 0) THEN
         NUMBER = KAWKT
         IWKTYP = KNIL
        IF (NTH.GT.0 .AND. NTH.LE.KAWKT) THEN
          IWKTYP = KLAWKT(NTH)
        ELSEIF(NTH.EQ.0) THEN
          IER = 0
        ELSE
          IER = 2002
        ENDIF
      ENDIF

      END
