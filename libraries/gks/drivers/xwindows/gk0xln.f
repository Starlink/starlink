*-----------------------------------------------------------------------


      SUBROUTINE GK0XLN(N,X,Y)

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Outputs polyline to buffer
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP N   - number of points
*     INP X,Y - coordinates of points
*
      INTEGER N
      REAL X(N),Y(N)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkerr.cmn'
*
*  LOCALS
*  ------
      INTEGER GK0XPL
*
*---------------------------------------------------------------------
      KERROR = GK0XPL(KWKIX,N,X,Y)
      END
