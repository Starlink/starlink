C# IL>=a, OL>=0
      SUBROUTINE GKCSSL(NAME, IDONE)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             CJW
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CSS : Select segment for reading
*
*  MAINTENANCE LOG
*  ---------------
*     17/11/83  CJW   Original version stabilized
*     19/01/87  ARG   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP NAME  - Segment name
*     OUT IDONE - Status
*
      INTEGER NAME, IDONE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkfls.par'
      INCLUDE '../include/gkcss.par'
      INCLUDE '../include/gkcss.cmn'
      INCLUDE '../include/gkfls.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*
*
*  ERRORS
*  ------
*     -2015  CSS is not open
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------


*     check CSS open
      IF (KCSFLS .NE. KFLOP) THEN
         CALL GKBUG (-2015,'GKCSSL')
      ELSE
*        locate segment
         CALL GKDRGE(KCSDIR, NAME, KCSIWK, KCSRWK, KCSWRK, QCSWRK)
      END IF
      IF ( KERROR.NE.0 ) THEN
         IDONE = KRFUSE
         KERROR = 0
      ELSE
         KCSRDN = NAME
         KCSRDX = KNIL
         IDONE = KACEPT
      END IF
      END
