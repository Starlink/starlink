C# IL>=a, OL>=1
      SUBROUTINE GKPWCG(DIST,BOX)
*
* (C) COPYRIGHT ICL & SERC  1985
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Pick utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Set or update extent and give hit distance.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP DIST   - Current hit distance
*     INP BOX    - Current extent
*     OUT DIST   - New hit distance
*     OUT BOX    - New extent
*
      REAL DIST,BOX(4)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Ignore   /CCA/    -  Pick point (QSS1..2)
*     Ignore   /CCA/    -  Tolerances (QSS3..4)
*     Modify   /CCA/    -  Extent     (QSS5..8)
*     Modify   /CCA/    -  Distance   (QSS9)
*
      INCLUDE '../include/gkcca.cmn'
*
*  LOCALS
*  ------
*
*  EXTERNALS
*  ---------
*
*  STACK USAGE
*  -----------
*
*  ERRORS
*  ------
*
*  COMMENTS
*  --------
*
*---------------------------------------------------------------------

* Set or update extent only when defined
      IF(QSS5.GE.0.0) THEN
        IF(BOX(1).LT.0.0) THEN
*         set initial extent
          BOX(1)=QSS5
          BOX(2)=QSS6
          BOX(3)=QSS7
          BOX(4)=QSS8
        ELSE
*         update extent
          IF(QSS5.LT.BOX(1)) BOX(1)=QSS5
          IF(QSS6.GT.BOX(2)) BOX(2)=QSS6
          IF(QSS7.LT.BOX(3)) BOX(3)=QSS7
          IF(QSS8.GT.BOX(4)) BOX(4)=QSS8
        ENDIF
      ENDIF

*     give distance
      DIST=QSS9

      END
