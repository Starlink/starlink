C# IL>=a, OL>=0
      SUBROUTINE GKPWSB(SEGBOX)
*
* (C) COPYRIGHT ICL & SERC  1984
*
*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Store bounding box in current segment entry.
*
*  MAINTENANCE LOG
*  ---------------
*     01/08/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP SEGBOX   - Segment extent
*
      REAL    SEGBOX(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*
*   Read   /WSL/   Segment pointers
*   Read   /WCA/   Workstation index
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gwksgl.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwsl.cmn'
*
*  LOCALS
*  ------
*
*  ERRORS
*  ------
*
*---------------------------------------------------------------------

      CALL GKSLBB(KSSGPT(KWKIX),KCURR,SEGBOX)
      END
