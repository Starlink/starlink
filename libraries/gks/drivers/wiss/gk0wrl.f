      SUBROUTINE GK0WRL
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
*     WISS - Release current segment
*
*  COMMON BLOCK USAGE
*  ------------------
*
*  EXTERNALS
*  ---------
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*     Incapable wiss needs CSS
*
*---------------------------------------------------------------------

      CALL GKCSRL
      RETURN
      END
