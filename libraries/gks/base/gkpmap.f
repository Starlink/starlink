C# IL>=a, OL>=0
      SUBROUTINE GKPMAP(AX,AY,BX,BY,ROSUB,XMIN,XMAX,YMIN,YMAX)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    Utility
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Patterning routine for Fill Area
*
*  MAINTENANCE LOG
*  ---------------
*     01/02/83  NGB   Original version stabilized
*     29/03/83  NGB   Coordinate Truncation included
*     29/11/83  NGB   Redundant Arguments IX,IY,IA removed
*     06/12/83  AS    Change KPATXR,YR to QWPAX,Y
*     11/01/84  MGC   ROSUB colour index array first dimension
*     20/01/84  MGC   Reorder to ensure stack space deallocated
*     14/02/84  MGC   Account for Pattern Ref Point signed coordinates
*     02/03/84  MGC   Correct width (IDX) for consistency with solid
*     03/07/87  RMK   Added GKMC.PAR - now needed by GKHP.CMN.
*     22/07/88  KEVP  Replaced IFIX with machine independent INT
*
*  ARGUMENTS
*  ---------
*     INP AX,AY  End A Coords
*     INP BX,BY  End B Coords ( BX>AX, BY=AY )
*     INP ROSUB  W/S Raster Output Routine
*     INP XMIN,XMAX,YMIN,YMAX  Clipping Rectangle
*
      REAL AX, AY, BX, BY, XMIN, XMAX, YMIN, YMAX
      EXTERNAL ROSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkmc.par'
      INCLUDE '../include/gkhp.cmn'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkstk.cmn'
      INCLUDE '../include/gkwsl.cmn'
      INCLUDE '../include/gkerr.cmn'
*
*  LOCALS
*  ------
*     IAX    Truncated DC of Span
*     IBX    Truncated DC of Span
*     IAY    Truncated DC of Span
*     IDX    Stack Space details
*     IRAST  Stack Space details
*     IPX    Pattern Coordinates and Colour Index
*     IPY    Pattern Coordinates and Colour Index
*
      INTEGER IAX, IBX, IAY, IDX, IRAST, IPX, IPY, IX, INTA(3)
      REAL REALA(1)
*
*  STACK USAGE
*  -----------
*     Workspace acquired in which to construct the 1-D raster
*
*  ALGORITHM
*  ---------
*     -maps the current pattern onto the given span,
*     -constructs a 1-D Raster by invoking GKPATP for each pixel
*     -and outputs it to the WorkStation using ROSUB
*
*  COMMENTS
*  --------
*     For consistency, the policy for all Pixel operations is to
*     truncate coordinates to the next lower pixel.
*
*---------------------------------------------------------------------



* First check that scanline is not clipped
      IF ( (AY.GE.YMIN) .AND. (AY.LE.YMAX) ) THEN
* Clip span
         IF(AX.LT.XMIN)  AX=XMIN
         IF(BX.GT.XMAX)  BX=XMAX
* Truncate (clipped) coords
         IAX=INT(AX)
         IBX=INT(BX)
         IAY=INT(AY)
* we know AX <= BX initially
         IF(IAX.LE.IBX) THEN
* (not completely clipped)
* how many pixels?
            IDX=IBX-IAX+1
* Get pattern dimensions and heap pointer (could set KERROR)
            CALL GKDRGE(KPABPT(KWKIX),KWFASI(KWKIX),3,0,INTA,REALA)
* acquire Workspace for the 1-D raster
            CALL GKSTAL(KINTGS,IDX,IRAST)
            IF (KERROR.EQ.0) THEN

* determine which pattern row to use
               IPY=MOD(INTA(2)+
     :                 MOD(IAY-INT(QWPAY(KWKIX)),INTA(2)),INTA(2))
               DO 100  IX=0,IDX-1
* extract appropriate pixels from pattern array
                  IPX=MOD(INTA(1)+
     :                MOD(IAX+IX-INT(QWPAX(KWKIX)),INTA(1)),INTA(1))
                  KSTACK(IRAST+IX) = KHP(KHPXI(INTA(3))+
     :                   IPX+INTA(1)*(INTA(2)-IPY-1))

  100          CONTINUE
* (ROSUB takes real Coords)
               CALL ROSUB(FLOAT(IAX),FLOAT(IAY),IDX,1,IDX,KSTACK(IRAST))
            ENDIF
*           .. deallocate any workspace
            CALL GKSTDA(KINTGS,IRAST)
         ENDIF
*        .. some span unclipped
      ENDIF
*     .. scanline not clipped
      END
