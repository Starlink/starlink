C# IL>=a, OL>=1
      SUBROUTINE GQSGUS ( NTH, IER, NSGNAM, ISGNAM)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  GKS Function name:  INQUIRE SET MEMBER OF SEGMENT NAMES IN USE
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Returns the Nth member of set of segment names in use.
*
*  MAINTENANCE LOG
*  ---------------
*     25/11/83  AS    Original version stabilized
*     20/04/84  JRG   Fix case of zero segments
*     22/01/87  JCS   IS conversion. Error number changed.
*     27/09/90  KEVP  Report error 2002 only if there is at least one
*                     segment and the list element requested is
*                     non-zero (C41). Required by GKS FORTRAN BINDING.
*
*  ARGUMENTS
*  ---------
*     IN    NTH    Set member requested
*     OUT   IER    Error indicator
*     OUT   NSGNAM number of segment names
*     OUT   ISGNAM Nth member of set of segment names in use
*
      INTEGER NTH, IER, NSGNAM, ISGNAM
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gksl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  ERRORS
*  ------
*     2002  Set member not available
*
*---------------------------------------------------------------------

      CALL GKPRLG (KNIL, GWSOP, GSGOP)

      IER = KERROR
      IF (KERROR .EQ. 0) THEN
* Get total number of segment names in use
        NSGNAM = KNUMSG
* Get Nth segment name
        ISGNAM=KNIL
        IF (KSGLST.NE.KNIL) CALL GKDRQN(KSGLST,NTH,ISGNAM)
        IF (ISGNAM .EQ. KNIL) THEN
           IF((NTH.NE.0).AND.(NSGNAM.NE.0)) IER = 2002
        ENDIF
      ENDIF

      END
