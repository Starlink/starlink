C# IL>=a, OL>=0
      SUBROUTINE GKCELI(RCX,RCY,ITYPE,IREFL,
     :                  NCEC,NCER,NRAC,NRAR,ROX,ROY)
*
* (C) COPYRIGHT ICL & SERC  1988
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Examine the cell array box, and return
*     information useful for cell array routine choice.
*
*  MAINTENANCE LOG
*  ---------------
*     09/05/88  KEVP  Created
*     27/07/88  KEVP  Use NINT to make sure the number of rows is
*                     the number of rows of pixel centres in box
*                     likewise with columns.
*     08/09/88  KEVP  Made it count Cell-Array as parallel to axes,
*                     if and only if it would conform to GKS standard,
*                     when drawn as such.
*     16/11/90  KEVP  Changed INCLUDE statements to lower-case for
*                     GKS 1.3.
*                     Reject cell arrays with area less than QTOL pixels
*                     rather than half a pixel.
*
*  ARGUMENTS
*  ---------
*     INP RCX,RCY    Cell array box in Raster Coordinates
*     OUT ITYPE      Transformation Type
*                     1 = One to one mapping of cells to pixels
*                     2 = Cell array rectangular and parallel to axes
*                     3 = General case
*                    (KNIL = Zero area Cell array, don't backtransform!)
*     OUT IREFL      Reflection Indicator (0 to 7) %
*                     1,3,5 or 7 (bit 0) Swap X and Y
*                     2,3,6 or 7 (bit 1) Reflect on X axis
*                     4,5,6 or 7 (bit 2) Reflect on Y axis
*                    Swapping is done before reflection
*     OUT NCEC,NCER  Number of cell columns and rows %
*     OUT NRAC,NRAR  Number of raster columns and rows %
*     OUT ROX,ROY    Raster origin of cell array to cope with
*                    reflection in RC (upper-left corner)

*     %  IREFL, NCEC,NCER and NRAC,NRAR are not applicable
*        to the general case ITYPE=3.
*
      REAL     RCX(4),RCY(4), ROX,ROY
      INTEGER  ITYPE, IREFL, NCEC,NCER, NRAC,NRAR
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../include/gkdt.par'
*                      for KNIL
      INCLUDE '../include/gkmc.par'
*                      for QTOL
      INCLUDE '../include/gkwca.cmn'
*                      for cell array dimensions (KWI5,KWI6)
*  LOCALS
*  ------
*  Values of ITYPE
*     JUNIT  One-to-one correspondence between pixels and cells
*     JPAR   Cell array Parallel to axes
*     JGEN   General case
*
      INTEGER    JUNIT,  JPAR,  JGEN, I
      PARAMETER (JUNIT=1,JPAR=2,JGEN=3)
*
*     RRX,RRY      Top-Right of cell array
*     RBX,RBY      Bottom-Left of cell array
*     HCAX,HCAY    Height vector of cell array (only to check area)
*     WCAX,WCAY    Width  vector of cell array (only to check area)
*     WCELL,HCELL  Cell dimensions
*     I            Do Loop Index

      REAL      RRX,RRY, RBX,RBY , HCELL,WCELL
      REAL      HCAX,HCAY, WCAX,WCAY
*
*  ALGORITHM
*  ---------
*     The Cell array box is examined for special cases:
*     1) If there is a one-to-one correspondence between cells and pixels,
*     2) If the transformed Cell Array
*          a) is rectangular;
*          b) has its edges parallel to the screen axes;
*
*     3) Otherwise
*
*     For cases (1) and (2), the cell array transformation
*     is examined for any reflection(s) and the appropiate
*     parameters are output.
*     In case (3) only (ROX,ROY) is applicable and is output
*     as (RCX(1),RCY(1)).
*
*  COMMENTS
*  --------
*     Cell array Box RCX,RCY:
*
*     The corners are of cells (1,1), (KWI5,1), (KWI5,KWI6) and (1,KWI6)
*     in that order, as given by GKCELT (but GKCELT gives the box in DC).
*
*---------------------------------------------------------------------

*     Raster origin - moved, if reflection required
      ROX   = RCX(1)
      ROY   = RCY(1)
*     Two other raster corners - moved if axes swapped
      RRX   = RCX(2)
      RRY   = RCY(2)
      RBX   = RCX(4)
      RBY   = RCY(4)

* First reject cell array with zero area (ie, with collinear corners).
      WCAX = RRX - ROX
      WCAY = RRY - ROY
      HCAX = ROX - RBX
      HCAY = ROY - RBY
      IF(ABS(WCAX*HCAY - WCAY*HCAX) .LT. QTOL) THEN
           ITYPE = KNIL
           GOTO 900

* First test left and top to see if cell array is definitely sloping
      ELSEIF((NINT(RCY(2)) .EQ. NINT(RCY(1))) .AND.
     :       (NINT(RCX(1)) .EQ. NINT(RCX(4))))THEN
           NCEC = KWI5
           NCER = KWI6
           IREFL = 0
*     -check for 90 degree rotation
      ELSEIF((NINT(RCX(2)) .EQ. NINT(RCX(1))) .AND.
     :       (NINT(RCY(1)) .EQ. NINT(RCY(4))))THEN
*   All references to the source Cell Array must swap X and Y
*  (note: calculations on destination Raster unaffected)
          IREFL = 1
*     new Width is original Depth
          NCEC = KWI6
*     new Depth is original Width
          NCER = KWI5
*     new corners
          RRX = RCX(4)
          RRY = RCY(4)
          RBX = RCX(2)
          RBY = RCY(2)
      ELSE
*       Sloping cell-array
        GOTO 500
      ENDIF
*
*  Cell array almost parallel to axes, but may slope slightly
*
*    check for reflections and set up raster dimensions & limits
*    (on the assumption of no slope)
      NRAC = NINT(RCX(3)) - NINT(RCX(1))
      NRAR = NINT(RCY(1)) - NINT(RCY(3))
*
*    if NRAC was negative, set reflection (Left-Right)
      IF (NRAC.LT.0) THEN
         IREFL = IREFL+2
         NRAC=-NRAC
      ENDIF

*    if NRAR was negative, set reflection (Top-Bottom)
      IF (NRAR.LT.0) THEN
         IREFL = IREFL+4
         NRAR = -NRAR
      ENDIF

*     Cell array is almost parallel to the axes but may still slope
*     slightly. Each row/column of cells must be tested to see
*     if no parallel row/column of pixel-centres enters of leaves
*     the cell row/column by the side. This could happen with an
*     arbitrarily small slope, if a pixel-centre occurs on the side
*     of a cell row/column.
*
*     If there are less pixel row/columns than cell row columns,
*     each pixel row/column is tested.


*     - raster rows  cell rows (after rotatation)
      HCELL = (ROY-RBY)/NCER
      IF(NRAR .GE. NCER)THEN
*        test cell rows
         DO 400 I=1,NCER
            IF(NINT(ROY-I*HCELL) .NE. NINT(RRY-I*HCELL))GOTO 500
  400    CONTINUE
      ELSE
*        test raster rows
         DO 410 I = NINT(RBY),NINT(ROY)-1
            IF((INT((FLOAT(I)-ROY)/HCELL)) .NE.
     :         (INT((FLOAT(I)-RRY)/HCELL))) GOTO 500
  410    CONTINUE
      ENDIF
*
*     - raster columns cell columns (after rotation)
      WCELL = (RRX-ROX)/NCEC
      IF(NRAC .GE. NCEC)THEN
*       test cell columns
        DO 420 I=1,NCEC
           IF(NINT(ROX+I*WCELL) .NE. NINT(RBX+I*WCELL))GOTO 500
  420   CONTINUE
      ELSE
*        test raster columns
         DO 430 I = NINT(ROX)+1,NINT(RRX)
            IF((INT((FLOAT(I)-ROX)/WCELL)) .NE.
     :         (INT((FLOAT(I)-RBX)/WCELL))) GOTO 500
  430    CONTINUE
      ENDIF

*  Cell Array Parallel to axes
*     Check for one-to-one cell-pixel correspondence
      IF((NCEC .EQ. NRAC) .AND. (NCER .EQ. NRAR))THEN
          ITYPE = JUNIT
      ELSE
          ITYPE = JPAR
      ENDIF

*     Move Raster Origin to take account of reflections
*     so as to make sure its on the top-left.
      IF(IREFL/2 .EQ. 1)THEN
*       Left-Right Reflection
         ROX = RRX
         ROY = RRY
      ELSEIF(IREFL/2 .EQ. 2)THEN
*       Top-Bottom Relection
         ROX = RBX
         ROY = RBY
      ELSEIF(IREFL/2 .EQ. 3)THEN
*       Inversion
         ROX = RCX(3)
         ROY = RCX(3)
      ENDIF
      GOTO 999
*
  500 CONTINUE
*     General type cell array not parallel to axes at all
*     Set general type
      ITYPE = JGEN
*     Set inapplicable output to KNIL
  900 IREFL = KNIL
      NCEC  = KNIL
      NCER  = KNIL
      NRAC  = KNIL
      NRAR  = KNIL

  999 CONTINUE
*
      END
