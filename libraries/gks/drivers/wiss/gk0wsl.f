      SUBROUTINE GK0WSL(NAME, IDONE)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             MGC
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*      WISS - Select specified segment
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
*  EXTERNALS
*  ---------
*
*  LOCALS
*  ------
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*     Incapable wiss needs CSS
*
*---------------------------------------------------------------------

      CALL GKCSSL(NAME, IDONE)
      RETURN
      END
