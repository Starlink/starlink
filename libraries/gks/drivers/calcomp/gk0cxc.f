
* --------------------------------------------------------------
      SUBROUTINE GK0CXC(IFID,ICHAR,X,Y)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     CC81 Single character output
*
*  MAINTENANCE LOG
*  ---------------
*     07/03/84 MGC  Original version stabilized
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
      INCLUDE '../../include/gkio.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwkd.cmn'
*
*  LOCALS
*  ------
*
      INTEGER IXPEN, IYPEN
      PARAMETER (IXPEN=2, IYPEN=3)
      INTEGER    ILXC
      PARAMETER (ILXC=3)
      INTEGER ICXC(ILXC)
      INTEGER IX,IY
      REAL RCHH,RCHW,RCHSIN,RCHCOS
      REAL RCENX,RCENY,RCHRX,RCHRY,RCHRL

*                 B    CR
      DATA ICXC /66, 0,13/
*
*  ALGORITHM
*  ---------
*     .PEN UP           : H
*     .MOVE TO (IX,IY)  : <IX>/<IY>K
*     .CHARPLOT ENABLE  : B
*     .PLOT CHAR(S)     : <ICHAR> ...
*     .CHARPLOT DISABLE : <CR>
*
*  COMMENTS
*  --------
*     Pen position is unknown after text output.
*
* --------------------------------------------------------------------

      RCHH=FLOAT(KWCHHT(KWKIX))
      RCHW=FLOAT(KWCHWD(KWKIX))
      RCENX=RCHW/2.0
      RCENY=RCHH/2.0
      RCHRX=QWCHRX(KWKIX)
      RCHRY=QWCHRY(KWKIX)
      RCHRL=SQRT(RCHRX*RCHRX+RCHRY*RCHRY)
      RCHSIN=RCHRY/RCHRL
      RCHCOS=RCHRX/RCHRL
      IX=IFIX(X-(RCENX*RCHCOS-RCENY*RCHSIN))
      IY=IFIX(Y-(RCENX*RCHSIN+RCENY*RCHCOS))
      ICXC(2)=ICHAR
      CALL GK0CPB(72)
      CALL GK0CPP(IX,IY)
      CALL GK0CPA(KIOPB,ILXC,ICXC)
      KWKDAT(IXPEN,KWKIX)=IXPEN+INT(RCHRX)
      KWKDAT(IXPEN,KWKIX)=KNIL
      KWKDAT(IYPEN,KWKIX)=KNIL
      RETURN
      END
