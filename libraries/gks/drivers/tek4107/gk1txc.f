

      SUBROUTINE GK1TXC(IFID,ICHAR,X,Y)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Tek 4107 Single character output
*
*  MAINTENANCE LOG
*  ---------------
*     25/01/85 GGT  Original version stabilized
*
*  ARGUMENTS
*  ---------
*     INP IFID     Font identifier (ignored)
*     INP ICHAR    Character value (ASCII)
*     INP X,Y      Centre position for cell
*
      INTEGER IFID,ICHAR
      REAL X,Y
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkio.par'
*
*  LOCALS
*  ------
*
*     ICHAHI  Offset in QWKDAT for hardware character height
*     ICHAWI  Offset in QWKDAT for hardware character width
*
      INTEGER    ICHAHI,   ICHAWI
      PARAMETER (ICHAHI=1, ICHAWI=2)
      INTEGER IBUFF(5),IX,IY,NLEFT
      REAL RCHH,RCHW,RCHSIN,RCHCOS
      REAL RCENX,RCENY,RCHRX,RCHRY,RCHRL

      DATA IBUFF /27,76,84,49,0/
*  COMMENTS
*  --------
*     Pen position is unknown after text output.
*
* --------------------------------------------------------------------

      RCHH=QWKDAT(ICHAHI,KWKIX)
      RCHW=QWKDAT(ICHAWI,KWKIX)
      RCENX=RCHW/2.0
      RCENY=RCHH/2.0
      RCHRX=QWCHRX(KWKIX)
      RCHRY=QWCHRY(KWKIX)
      RCHRL=SQRT(RCHRX*RCHRX+RCHRY*RCHRY)
      RCHSIN=RCHRY/RCHRL
      RCHCOS=RCHRX/RCHRL
      IX=INT(X-(RCENX*RCHCOS-RCENY*RCHSIN)+0.5)
      IY=INT(Y-(RCENX*RCHSIN+RCENY*RCHCOS)+0.5)
      CALL GK1TMV(IX,IY)
      IBUFF(5)=ICHAR
      CALL GKIOBO(KIOPB,5,IBUFF,NLEFT)
      RETURN
      END
