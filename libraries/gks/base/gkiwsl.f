C# IL>=a, OL>=0
      SUBROUTINE GKIWSL(IWKIX,IWKTY)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             AS
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Initialise workstation state list.
*
*  MAINTENANCE LOG
*  ---------------
*     22/02/83  AS    Original version stabilized
*      7/07/83  AS    Change error handling
*     12/07/83  AS    Add NIPDEV for GKIWSI
*     09/11/83  AS    NIPDEV goes into common
*
*  ARGUMENTS
*  ---------
*     INP IWKIX  - workstation index
*     INP IWKTY  - workstation type
*
      INTEGER IWKIX,IWKTY
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkerr.cmn'
*
*---------------------------------------------------------------------


      CALL GKIWSO(IWKIX,IWKTY)
      IF (KERROR.NE.0) GOTO 999
      CALL GKIWSB(IWKIX,IWKTY)
      IF (KERROR.NE.0) GOTO 999
      CALL GKIWSI(IWKIX,IWKTY)

  999 CONTINUE
      END
