C# IL>=a, OL>=1
      SUBROUTINE GKPWCD(DIST)
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
*     Record hit distance.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP DIST  - Hit distance
*
      REAL    DIST
*
*  COMMON BLOCK USAGE
*  ------------------
*     Ignore   /CCA/    -  Pick point (QSS1..2)
*     Ignore   /CCA/    -  Tolerances (QSS3..4)
*     Ignore   /CCA/    -  Extent     (QSS5..8)
*     Modify   /CCA/    -  Distance   (QSS9)
*
      INCLUDE '../include/gkcca.cmn'
*
*  LOCALS
*  ------
*
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

      QSS9=DIST

      END
