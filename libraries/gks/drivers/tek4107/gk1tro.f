

      SUBROUTINE GK1TRO(X,Y,WIDTH,HEIGHT,NDIM,COLORS)
*---------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     TEK 4107 Raster Out
*
*  MAINTENANCE LOG
*  ---------------
*     00/99/83  MGC  Original version stabilised(CC81)
*     04/02/85  GGT  Adapt for use with Tekronix 4107
*
*  ARGUMENTS
*  ---------
*     INP X,Y    Upper left corner position for raster
*     INP WIDTH  Scanline length (DC)
*     INP HEIGHT No of scanlines
*     INP NDIM   First dimension of colour array
*     INP COLORS Array of colour indices for raster
*
      REAL X,Y
      INTEGER WIDTH,HEIGHT,NDIM,COLORS(NDIM,HEIGHT)
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
*
*  LOCALS
*  ------
*
      INTEGER IX,IY,IROW,ICOL,ICUR,INEXT,MHIGH
      REAL    RASX(2),RASY(2)
*
*  ALGORITHM
*  ---------
*     Output width by height colour array
*
* --------------------------------------------------------------

* Set up start point
      IX=INT(X+0.5)
      IY=INT(Y+0.5)
* Loop through each row of the rectangle

      MHIGH=MAX(1,HEIGHT)
      DO 20 IROW=1,MHIGH
        RASX(1)=FLOAT(IX)
        RASY(1)=FLOAT(IY)
        RASY(2)=FLOAT(IY)
*
* Scan through this row looking for colour changes, making sure that
* indices are within the workstation colour table.
*
        ICUR=MOD(COLORS(1,IROW), KPCI(KWKIX))

        DO 10 ICOL=2,WIDTH
          INEXT=MOD(COLORS(ICOL,IROW), KPCI(KWKIX))
          IF(ICUR .NE. INEXT) THEN
            RASX(2)=FLOAT(IX + (ICOL-2))
            CALL GK1TSP(ICUR)
            CALL GK1TLN(2,RASX,RASY)
            ICUR=INEXT
            RASX(1)=FLOAT(IX + ICOL - 1)
          ENDIF
   10   CONTINUE

*
* Finish off this row
*
        RASX(2)=FLOAT(IX + WIDTH - 1)
        CALL GK1TSP(ICUR)
        CALL GK1TLN(2,RASX,RASY)

        IY=IY-1
   20 CONTINUE
*
      RETURN
      END
