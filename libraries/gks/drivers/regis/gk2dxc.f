      SUBROUTINE GK2DXC(IFID,ICHAR,X,Y)
* --------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     ACW Single character output
*
*  MAINTENANCE LOG
*  ---------------
*     08/01/85 GGT  Original version stabilized
*     18/02/87 GGT  Adapted for DEC VT24x
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
*       IX,IY           Position of character (D.C)
*       NLEFT           Amount of space left in output buffer
*       RCHH            Character height in D.C.
*       RCHW            Character width in D.C.
*       RCHSIN          Sin component of text angle
*       RCHCOS          Cos component of text angle
*       RCENX           Half character width
*       RCENY           Half character height
*       RCHRX           Character up x-vector
*       RCHRY           Character up y_vector
*       RCHRL           Character up vector length

*
*  Offsets in KWKDAT workspace
*
      INTEGER MODE, CHARHI,CHARWI,ICX,ICY, ICPEN
      PARAMETER (MODE = 1, CHARHI = 2, CHARWI = 3, ICX = 4, ICY = 5,
     :           ICPEN = 6)
      INTEGER ICHFLG, ICHSTA, ICHNUM
      PARAMETER (ICHFLG=KMXWKI, ICHSTA=KMXWKI-1, ICHNUM=KMXWKI-2)
      INTEGER ILCFLG, ILCSTA, ILCX, ILCY
      PARAMETER (ILCFLG=KMXWKI-3, ILCSTA=KMXWKI-4)
      PARAMETER (ILCX=KMXWKR, ILCY=KMXWKR-1)
*
      INTEGER IX,IY,NLEFT
      REAL RCHH,RCHW,RCHSIN,RCHCOS
      REAL RCENX,RCENY,RCHRX,RCHRY,RCHRL
      CHARACTER*5 BUFFER
*
*  COMMENTS
*  --------
*
*     Pen position is unknown after text output.
*
* --------------------------------------------------------------------

      RCHH=FLOAT(KWKDAT(CHARHI,KWKIX))
      RCHW=FLOAT(KWKDAT(CHARWI,KWKIX))
      RCENX=RCHW/2.0
      RCENY=RCHH/2.0
      RCHRX=QWCHRX(KWKIX)
      RCHRY=QWCHRY(KWKIX)
      RCHRL=SQRT(RCHRX*RCHRX+RCHRY*RCHRY)
      RCHSIN=RCHRY/RCHRL
      RCHCOS=RCHRX/RCHRL
*
*       Calculate text position according to size
*       and rotation.
*
      IX=INT(X-(RCENX*RCHCOS-RCENY*RCHSIN)+0.5)
      IY=INT(Y-(RCENX*RCHSIN+RCENY*RCHCOS)+0.5) + KWKDAT(CHARHI,KWKIX)
     :                                                              - 1
*
*        Move to required position - this move must be forced
*        out in full by first destroying the current position
*
      KWKDAT(ICX,KWKIX)=-99
      KWKDAT(ICY,KWKIX)=-99
      CALL GK2DDR(IX,IY,0)
*
*        Output the character
*        Watch out for single quote (ASCII 39)
      IF (ICHAR.NE.39) THEN
         WRITE(BUFFER,100) CHAR(ICHAR)
  100    FORMAT('T''',A,'''')
         CALL GKIOCO(KIOPB,BUFFER(1:4),NLEFT)
      ELSE
         WRITE(BUFFER,101) CHAR(ICHAR),CHAR(ICHAR)
  101    FORMAT('T''',A,A,'''')
         CALL GKIOCO(KIOPB,BUFFER,NLEFT)
      ENDIF
*
      RETURN
      END
