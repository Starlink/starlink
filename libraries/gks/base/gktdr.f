      SUBROUTINE GKTDR (NPTS,XD,YD,XR,YR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Workstation Utility
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Converts an array of device coordinates to raster coordinates
*     No rounding is done.
*
*  MAINTENANCE LOG
*  ---------------
*     07/04/88   KEVP Stabilised
*
*  ARGUMENTS
*  ---------
*    INP   NPTS         the number of coordinate pairs
*    INP   XD (NPTS)    the real array of device x-coordinates
*    INP   YD (NPTS)     .   .    .     .   .    y-coordinates
*    OUT   XR (NPTS)    the real array of raster x-coordinates
*    OUT   YR (NPTS)     .    .    .    .    .   y-coordinates
*
      INTEGER NPTS
      REAL  XD(NPTS), YD(NPTS), XR(NPTS), YR(NPTS)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read   /WCA/    Workstation index
*     Read   /WDT/    Maximum display size in device and raster coords
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwdt.cmn'
*
*  LOCALS
*  ------
*     I        Loop variable
*     XCONV    Conversion factor for X-coords
*     YCONV    Conversion factor for Y-coords
*     TINY     Minimum allowed display size in DC
*
      INTEGER I
      REAL    XCONV, YCONV, TINY
      PARAMETER (TINY=0.000001)
*
*  ALGORITHM
*  ---------
*     The maximum display surface sizes are obtained in both
*     device coordinates and raster coordinates.
*     A pair of conversion factors are derived for X and Y coords
*     by dividing the maximum display size in raster coords by that
*     in device coords. Each coordinate is then multiplied by its
*     conversion factor.
*
*-------------------------------------------------------------------
*
*     Check maximum display sizes
      IF(((KDSRX(KWKIX) .LT. 1) .OR. (KDSRY(KWKIX) .LT. 1)) .OR.
     :   ((QDSDX(KWKIX) .LT. TINY).OR.(QDSDY(KWKIX) .LT. TINY)))THEN
         CALL GKBUG (-2006,'GKTDR')
         GOTO 999
      ENDIF

*     Derive conversion factors
      XCONV =  FLOAT(KDSRX(KWKIX))/QDSDX(KWKIX)
      YCONV =  FLOAT(KDSRY(KWKIX))/QDSDY(KWKIX)

*     Convert Coordinates
      DO 10 I=1,NPTS
         XR(I) = XCONV * XD(I)
         YR(I) = YCONV * YD(I)
   10 CONTINUE

  999 CONTINUE
      END
