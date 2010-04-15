      SUBROUTINE GK9SXF(IFID,RHT,RMAXWD,RBOT,RTOP,RWD)

*  Copyright (C) SERC 1987
*
*-----------------------------------------------------------------------
*
*  Type of Routine:  Part of WORKSTATION DRIVER
*  Author:           PJWR
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Supply raster font details for Sun workstation.
*
*  MAINTENANCE LOG
*  ---------------
*     10/09/87  PJWR  Created.
*     26/01/88  TAW   Changed ICHHT, ICHWD, ICHCT, ICHBB to be
*                     The same as in gk9swd.
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     OUT RHT      Height from base to cap line
*     OUT RMAXWD   Width of widest character
*     OUT RBOT     Distance from base to bottom line
*     OUT RTOP     Distance from cap to top line
*     OUT RWD      Character widths array
*
      INTEGER IFID
      REAL RHT,RMAXWD,RBOT,RTOP,RWD(*)
*
*  COMMON BLOCK USAGE
*  ------------------
*     Read    /GKYWKD/  QWKDAT
*             /GKYWCA/  KWKIX
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
*
*  LOCALS
*  ------
*     I       Loop variable
*     ICHHT   Index in QWKDAT for char. height of raster font loaded.
*     ICHWD   Index in QWKDAT for char. width of raster font loaded.
*     ICHCT   Index in QWKDAT for cap to top line distance of raster
*             font loaded.
*     ICHBB   Index in QWKDAT for base to bottom line distance of
*             raster font loaded.
*
      INTEGER    ICHHT,     ICHWD,     ICHCT,     ICHBB
      PARAMETER (ICHHT = 3, ICHWD = 4, ICHCT = 5, ICHBB = 6)
      INTEGER I
*
*  COMMENTS
*  --------
*       The font information is set up by GK9SSF.  The PARAMETERs ICH??
*       above must match those of the same name in GK9SWD().
*
*-----------------------------------------------------------------------

      RHT = QWKDAT(ICHHT,KWKIX)
      RMAXWD = QWKDAT(ICHWD,KWKIX)
      RBOT = QWKDAT(ICHBB,KWKIX)
      RTOP = QWKDAT(ICHCT,KWKIX)

*     Set widths for ASCII characters 32 to 96.

      DO 10, I = 1, 95
        RWD(I) = RMAXWD
 10   CONTINUE

      END
