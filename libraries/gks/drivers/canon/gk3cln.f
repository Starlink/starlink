      SUBROUTINE GK3CLN(N,X,Y)
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Plot polyline
*
*  ARGUMENTS
*  ---------
*     INP N    - Number of points
*     INP X,Y  - Coordinates of points
*
      INTEGER N
      REAL X(N),Y(N)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.par'
      INCLUDE '../../include/gkwdt.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*--------------------------------------------------------------------
      INTEGER NLEFT, I, NC
      CHARACTER*10 C
      REAL FX, FY
      PARAMETER (FX=8.46668)
      CHARACTER*1 IS2
*      PARAMETER (IS2=CHAR(30))
      IS2=CHAR(30)

      FY = - FX

      CALL GKFOCO(KIOPB,'1',NLEFT)
      CALL GK3CEI( NINT(X(1)*FX), C, NC)
      CALL GKFOCO(KIOPB,C(:NC),NLEFT)
      CALL GK3CEI( NINT(Y(1)*FY), C, NC)
      CALL GKFOCO(KIOPB,C(:NC),NLEFT)

      DO 100 I = 2,N
        CALL GK3CEI( NINT(X(I)*FX) - NINT(X(I-1)*FX), C, NC)
        CALL GKFOCO(KIOPB,C(:NC),NLEFT)
        CALL GK3CEI( NINT(Y(I)*FY) - NINT(Y(I-1)*FY), C, NC)
        CALL GKFOCO(KIOPB,C(:NC),NLEFT)
  100 CONTINUE

      CALL GKFOCO(KIOPB,IS2,NLEFT)

      END
