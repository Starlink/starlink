*---------------------------------------------------------------------


      SUBROUTINE GK0XRO(X,Y,NXPIX,NYPIX,NXDIM,ICOLAR)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    W/S
*  Author:             DLT
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*   Outputs a NXPIX x NYPIX colour array.
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP X,Y    - coordinates of raster origin
*     INP NXPIX  - No of pixels per scanline
*     INP NYPIX  - No of scanlines in raster
*     INP NXDIM  - First dimension of colour array
*     INP ICOLAR - Integer colour array to output
*
      REAL    X, Y
      INTEGER NXPIX, NYPIX, NXDIM, ICOLAR(NXDIM,NYPIX)
      INTEGER GK0XDI
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
*
*  LOCALS
*  ------
*
*
*---------------------------------------------------------------------
      KERROR = GK0XDI(KWKIX, X, Y, NXPIX, NYPIX, NXDIM, ICOLAR)
      END
