C# IL>=a, OL>=0
      SUBROUTINE GKCLKS
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Front End
*  Author:             JRG
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Internal routine to close GKS. Everything except calling
*     GKS Prologue is performed here. Separate routine exists so that
*     both Close GKS functions (Emergency and normal) can use it.
*
*  MAINTENANCE LOG
*  ---------------
*      2/11/83  JRG   Created
*
*  ARGUMENTS
*  ---------
*     None
*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify /OPS/    Change GKS operating state.
*
      INCLUDE '../include/gks.par'
      INCLUDE '../include/gkops.cmn'
*
*  ERRORS
*  ------
*    None ... any problems closing files and data structures are not
*    reported.
*
*---------------------------------------------------------------------


*   Close internal files
      CALL GKIOEN

*   Close various data structures
      CALL GKSTCL
      CALL GKHPCL
      CALL GKXOF

*   Set operating state to be "GKS closed"
      KOPS=GGKCL
*

*
      END
