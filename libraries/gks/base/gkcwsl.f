C# IL>=a, OL>=0
      SUBROUTINE GKCWSL(IWKIX)
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
*     Deallocate workstation state list heaps.
*
*  MAINTENANCE LOG
*  ---------------
*     18/07/83  AS    Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IWKIX - workstation index
*
      INTEGER IWKIX
*
*---------------------------------------------------------------------


      CALL GKCWSB(IWKIX)
      CALL GKCWSI(IWKIX)

      END
