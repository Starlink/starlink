C# IL>=a, OL>=1
      SUBROUTINE GKPWCR(X,Y,TMIN,TMAX)
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
*     Reset scan data.
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised
*
*  ARGUMENTS
*  ---------
*     INP X,Y   - Pick point
*     INP TMIN,TMAX - Hit tolerances
*
      REAL X,Y,TMIN,TMAX

*
*  COMMON BLOCK USAGE
*  ------------------
*     Modify   /CCA/    -  Pick point (QSS1..2)
*     Modify   /CCA/    -  Tolerances (QSS3..4)
*     Modify   /CCA/    -  Extent     (QSS5..8)
*     Modify   /CCA/    -  Distance   (QSS9)
*
      INCLUDE '../include/gkcca.cmn'
*
*  LOCALS
*  ------
*     TSMALL    Smallest acceptable tolerance
*
      REAL       TSMALL
      PARAMETER (TSMALL=0.001)
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

* Store pick point
      QSS1=X
      QSS2=Y

* Store tolerances
      IF(TMIN.GT.TSMALL) THEN
        QSS3=TMIN
      ELSE
        QSS3=TSMALL
      ENDIF
      IF(TMAX.GT.TSMALL) THEN
        QSS4=TMAX
      ELSE
        QSS4=TSMALL
      ENDIF
      IF(QSS4-QSS3.LT.TSMALL) THEN
        QSS4=QSS4+TSMALL
      ENDIF

* Initialise extent
      QSS5=-1.0
      QSS6=-1.0
      QSS7=-1.0
      QSS8=-1.0

* Initialise distance
      QSS9=1.0

      END
