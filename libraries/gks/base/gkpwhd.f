C# IL>=a, OL>=1
      SUBROUTINE GKPWHD(X,Y,WIDTH,HEIGHT,COLORS)
*
* (C) CORYRIGHT ICL & SERC  1985
*
*  Type of routine:    Pick utility
*  Author:             MGC
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Raster output (stub)
*
*  MAINTENANCE LOG
*  ---------------
*     01/01/85  MGC   Original version stabilised.
*     21/01/87  ARG   IS conversion. Error number changed.
*
*  ARGUMENTS
*  ---------
*     INP X,Y    - Upper left corner position for raster
*     INP WIDTH  - Scanline length (DC)
*     INP HEIGHT - No of scanlines
*     INP COLORS - Array of colour indices for raster
*
      REAL X,Y
      INTEGER WIDTH,HEIGHT,COLORS(WIDTH,HEIGHT)
*
*  ERRORS
*  ------
*     -9999  Bug - this routine should never be entered
*
* --------------------------------------------------------------

      CALL GKBUG (-9999,'GKPWHD')
      END
