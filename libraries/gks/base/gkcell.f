C# IL>=a, OL>=0
      SUBROUTINE GKCELL(ISIZ,ICA,ROSUB)
*
* (C) COPYRIGHT ICL & SERC  1984
*

*---------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:    UTILITY
*  Author:             NGB
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     A Cell Array of N by M Cells is mapped onto a WC rectangle
*     delimited by its origin, and the two opposite corners, P and Q.
*
*  MAINTENANCE LOG
*  ---------------
*     07/03/83  NGB   Original version stabilized
*     25/04/83  NGB   Error on Stack Fail provided
*     29/04/83  AS    Change name from GKCELL
*     03/06/83  AS    Remove diagnostics
*     04/07/83  PGLS  More KERROR changes
*     04/08/83  AS    Move subroutines to correct files
*     15/09/83  NGB   Introduce three-point interface
*     19/09/83  NGB   Re-code general mapping to avoid using GKSCAN
*                     restructure the rest, removing GKCCOP & GKCMAP
*     08/11/83  NGB   Correct mapping bug that sheared the image
*     11/01/84  MGC   ROSUB cell array first dimension parameter
*     01/05/84  MGC   Corrections for transformed cell array (Case 3)
*     02/10/85  JRG   Removed blank line at end of file (bug S96)
*     25/06/86  DCS   Correction from DLT - Set ISCAN when cell array is
*                     rectangular but parallel to opposite axes (x and y
*                     swapped) (bug S191)
*     03/07/86  DCS   (ICL fix MGC 11/12/84) Correct case 2 to avoid line
*                     down right side (I221,S121)
*     21/01/87  KWB   IS conversion. Major revision of code to
*                     incorporate new CELL ARRAY definition concerning
*                     orientation of array with respect to P and Q,
*                     and new language binding which allows submatrix.
*                     Some errors also corrected:
*                     - clipping included for all cases
*                     - check for degenerate cell array made only in NDC,
*                     not WC
*                     - case of 90 degree rotation of non-square array
*                     corrected
*     23/04/87  KWB   IS conversion. Further error correction: check added
*                     in case 3 to exclude output of null portion. SMALL
*                     parameter increased to 1.0E-04 (S245).
*     28/04/87  RMK   Added declaration of local variable RY.
*     17/12/87  PJWR  Corrected generation of array indices in the case
*                     of a 1 - 1 correspondence between cells and pixels
*                     (S307). Also removed unused local variable N.
*     06/04/88  KEVP  Shortened code by using utilities GKCELT and GKCELI
*                     which deal with the cell array transformation.
*                     Rationalised names of some variables.
*                     Invert transformation with GKMTIV
*     27/07/88  KEVP  Made sure that centre of pixel is sampled to give
*                     colour as defined in GKS standard (S375 - case 3).
*     28/07/88  KEVP  Made CASE 3 more efficient by backtransforming
*                     only the upper-left pixel into cell-space and then
*                     incrementing by the backtransformation vectors.
*                     Also the X do-loop is quit, if the cell array
*                     box has been crossed (it's convex).
*     13/09/88  KEVP  Made changes to algorithm to make it comply with
*                     the GKS Standard for all 3 cases (S375).
*                     Machine dependent IFIX has been replaced by NINT
*                     or INT as appropriate.
*     23/08/90  KEVP  Converted arguments of INCLUDE statements into
*                     lower-case for GKS1.3.
*     29/10/90  PLP   Removed unused local ISIDE.
*     16/07/92  DLT   Fix erroneous computation of inverse
*                     transformation.
*
*  ARGUMENTS
*  ---------
*     INP ISIZ  Cell Array Dimensions
*     INP ICA   Cell Array
*     INP ROSUB Device Driver RasterOut routine
*
      INTEGER ISIZ, ICA(ISIZ)
      EXTERNAL ROSUB
*
*  COMMON BLOCK USAGE
*  ------------------
*    W/S Comms Area:
*     KWKIX     : Workstation Identifier
*
*     KWI1,KWI2 : dimensions of colour array
*     KWI3,KWI4 : start column, start row
*     KWI5,KWI6 : number of columns, number of rows
*
      INCLUDE '../include/gkhp.par'
      INCLUDE '../include/gkdt.par'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkstk.cmn'
*
*  LOCALS
*  ------
*     XMIN,XMAX  Clipping rectangle (xmin,xmax,ymin,ymax)
*     YMIN,YMAX
*     NCEC   Cell columns
*     NCER   Cell rows
*     NRAC   Pixel Cols
*     NRAR   Pixel Rows
*     LSCAN  Pixel Array, Cell Array first dimension
*     DCX    DC Coord arrays for transformation
*     DCY
*     TRNCEL Cell array transformation
*     TRCINV Inverse Cell array transformation
*     INCRA  Increment for pixel columns
*     INCRB  Increment for pixel rows
*     ICASE  Case of cell array
*     ROX    DC Cell Array Origin Mapping
*     ROY
*     IREFL  Reflection indicator (used to indicate 90 degree rotation)
*     IA,IB  Swapable indices
*     LCB    Cell Coord of Last Pixel Row
*     IRR    StackBase for Pixel buffer of Raster Row
*     ZLX,ZRX,ZBY,ZTY  Intersection of cell array bounding rectangle
*                      and clip rectangle in DC (xmin,xmax,ymin,ymax)
*     ILX,IRX,IBY,ITY  Intersection in integer coordinates
*                      rounded to include pixel centres (ie with NINT)
*     ICL,ICR,ICT,ICB, Region of Cell Array index space to be shown
*     ICC     Cell coordinates of back-transformed pixel
*     IYC     Cell coordinates of back-transformed pixel row start
*     MWID    Minimum width of cell array (in cells or pixels) to be
*             tested for slipingness
*     ILEL,IRIL   Left and Right limits of back-transformed pixel row
*     PYC         Back-transformation of pixel row start (real)
*     PCC         Back-transformation of pixel in real Cell Space
*     RX,RY  Centre of first pixel in cell array bounding box
*     IX,IY  Local Do Loop Variables (pixel)


      INTEGER NRAR, NRAC, NCEC, NCER, ICL,ICR,ICT,ICB,
     :        IRR, ILEL,IRIL, IX,IY, IA,IB, LCB, INCRA,INCRB,
     :        ILX,IRX,ITY,IBY,IYC(2),ICC(2),LSCAN,ICASE,IREFL,MWID
      REAL XMIN,XMAX,YMIN,YMAX, ZLX,ZRX,ZBY,ZTY, DCX(4),DCY(4),
     :      ROX,ROY, RX,RY, TRNCEL(3,2),TRCINV(3,2), PYC(2), PCC(2)
*
      PARAMETER (MWID=4)
*
*  STACK USAGE
*  -----------
*     Integer workspace at IRR, size IRX-ILX+1, to hold a raster row
*
*  ERRORS
*  ------
*    300 : Stack Full  -occurs if unable to allocate sufficient stack.
*
*  ALGORITHM
*  ---------
*     A Cell Array is DX by DY Cells mapped onto a rectangle
*     delimited by two opposite corners, P and Q.  The orientation of
*     the array is such that the (1,1) element of the array is at
*     P [PX,PY], the (DX,DY) element at Q [QX,QY] and the (DX,1)
*     element at R [QX,PY].
*     For internal computation two coordinate systems are used
*     DC which are assumed to be raster coords when rounded down
*     and CIC which are cell indices minus one when rounded down.
*     P is (0,0) and R is (DX-1,0) in CIC for example.
*     The transformation from CIC to DC is called the cell array
*     transformation and is got from the utility GKCELT.
*     The sub-array offset (KWI3,KWI4) then added to the CIC before
*     any pixel rows are calculated and output.

*     The GKS standard demands that each pixel is coloured in the
*     colour of the cell, which contains the centre of the pixel.
*     To do this it is necessary to find which cell the centre of
*     each pixel belongs.
*        However there are certain cases, where short cuts can be
*     taken.
*
*     The Cell array transformation is examined for special cases:
*     1) If there is a one-to-one correspondence between cells and pixels,
*     2) If the transformed Cell Array
*          a) is rectangular;
*          b) has its edges parallel to the screen axes;
*
*     3) The general case
*
*     In each case the pixel-centre of the upper-left hand corner
*     of the cell array bounding box (just the cell array in cases
*     1) and 2) ) is converted to CIC and the cell sub array offset
*     is added.
*        The array is then scanned pixel by pixel using the appropiate
*     components of the inverse Cell-array transformation to move to
*     to the next pixel-centre. The beginning of the current pixel-row
*     in CIC is stored to enable the next row to be started at the
*     right place.
*          Note: with large cell-arrays it may be necessary for the
*       cell array transformation and pixel-centre CIC coords to be
*       double-precision to avoid cumulative floating point inaccuracies.
*       This is not done here.
*
*     Each the colour for each pixel in the pixel-row (and cell-array)
*     is put into the stack and then sent to the Device Raster Output
*     routine.
*
*     In case 1) and 2) swappable indices IA and IB are used.
*     They are equal to 1 and 2 respectively if X and Y coords
*     of the cell-array are NOT swapped by the cell array transformation.
*     Otherwise they are 2 and 1 respectively.
*        This enables only the non-zero elements of the cell-array
*     transformation to be used and simplifies the code.
*
*     Only in case 3) is it necessary to test whether the centre of
*     each pixel is inside the cell-array
*
*     In case 1) the inverse of the Cell Array Transformation is
*     easy to calculate as the non-zero elements of the main body of the
*     matrix (excluding the translation vector) have an absolute value
*     of 1 and the inverse is just the transpose.
*     This is converted to integers staight away to allow the rows
*     to be built up (in the stack) and output one by one
*     moving in steps of + or - 1.
*
*     In case 2) if when moving to the next pixel row, there is no
*     change in the integer CIC of the row start, then the stack
*     is simply sent again to the Raster Output routine with the
*     new pixel row start, because it is identical to the previous
*     pixel row.
*
*  COMMENTS
*  --------
*   Much debate has centred round the issue of Interpreting a CellArray
*      primitive read from a Metafile. The concern is that, in storing it in
*      the Metafile, a segment transformation may have been applied that
*      distorted it to a non-orthogonal NDC mapping. This cannot occur before
*      Level 2, when InsertSegment happens.
*
*   Ideally, such a transform should have been stored in the Metafile, and
*      set up by the Metafile Interpreter before sending the (untransformed)
*      Cell Array primitive. (This would have worked for other primitives
*      with implied geometry, such as circle GPSs and text).
*
*   However, this approach has not been adopted.
*      Instead, the transformation is recorded in the Metafile by storing the
*     transformed NDC of the three corners, [PX,PY], [QX,QY], [QX,PY].
*      the Workstation entry-point (which does not know the origin of a Cell
*      Array primitive) must accomodate these three points, even though (below
*      level 2) the third one is unnecessary.
*
*   CELL SAMPLING according to GKS Standard
*      The GKS standard demands that each pixel should be coloured,
*      the same colour as the cell that its centre occurs in.
*      This requires that the rounding of co-ordinates to integers is
*      deferred, so requiring work to be done in real numbers. This
*      therefore slows down the performance.
*
*      This also demands that GKCELI checks every row and column,
*      to see if the array is sloping or not, so that a faster algorithm
*      is selected, only if it complies with the GKS standard.
*      Hence it is considered not worth executing GKCELI, if the cell
*      array is very narrow. Then it is treated as though it is sloping,
*      using the slow general case algorithm.
*
*---------------------------------------------------------------------

* Extract total clip rectangle using W/S ID from Comms Area:
      XMIN=QWCLXL(KWKIX)
      XMAX=QWCLXR(KWKIX)
      YMIN=QWCLYB(KWKIX)
      YMAX=QWCLYT(KWKIX)

* Get Cell array rectangle in DC and cell array transformation
      CALL GKCELT(DCX,DCY,TRNCEL)

* (point 1 is P; point 2 is R; point 3 is Q; point 4 is P-R+Q)

* Find intersection of bounding box and clip rectangle
*     Left
      ZLX = DCX(1)
      IF (DCX(2) .LT. ZLX) ZLX = DCX(2)
      IF (DCX(3) .LT. ZLX) ZLX = DCX(3)
      IF (DCX(4) .LT. ZLX) ZLX = DCX(4)
      IF (XMIN .GT. ZLX) ZLX = XMIN
*     Right
      ZRX = DCX(1)
      IF (DCX(2) .GT. ZRX) ZRX = DCX(2)
      IF (DCX(3) .GT. ZRX) ZRX = DCX(3)
      IF (DCX(4) .GT. ZRX) ZRX = DCX(4)
      IF (XMAX .LT. ZRX) ZRX = XMAX
*     Bottom
      ZBY = DCY(1)
      IF (DCY(2) .LT. ZBY) ZBY = DCY(2)
      IF (DCY(3) .LT. ZBY) ZBY = DCY(3)
      IF (DCY(4) .LT. ZBY) ZBY = DCY(4)
      IF (YMIN .GT. ZBY) ZBY = YMIN
*     Top
      ZTY = DCY(1)
      IF (DCY(2) .GT. ZTY) ZTY = DCY(2)
      IF (DCY(3) .GT. ZTY) ZTY = DCY(3)
      IF (DCY(4) .GT. ZTY) ZTY = DCY(4)
      IF (YMAX .LT. ZTY) ZTY = YMAX

*     round the box to pixels, so that a pixel is included
*     if and only if, its centre is inside (hence use NINT & NINT-1).
      ILX = NINT(ZLX)
      IRX = NINT(ZRX) - 1
      IBY = NINT(ZBY)
      ITY = NINT(ZTY) - 1

* Check for zero intersection

      IF (ILX .GE. XMAX .OR. IRX+1 .LE. XMIN .OR.
     :    IBY .GE. YMAX .OR. ITY+1 .LE. YMIN) GOTO 9999

* If cell-array is narrow in either rasters or cells
* consider to be of general case, because testing for
* other cases is not economic.
      IF((KWI5 .LT. MWID) .OR. (KWI6 .LT. MWID) .OR.
     :(IRX-ILX .LT. MWID) .OR. (ITY-IBY .LT. MWID))THEN
         ICASE = 3
         ROX = DCX(1)
         ROY = DCY(1)
      ELSE
* Get data concerning cell array transformation
* for selection and execution for appropiate case
        CALL GKCELI(DCX,DCY,ICASE,IREFL,NCEC,NCER,NRAC,NRAR,ROX,ROY)
      ENDIF

      IF(ICASE .EQ. KNIL)GOTO 9999
*
*     Initialise Scan length for mapping 2D Cell Array to 1D Stack
      LSCAN = KWI1

      IF(ICASE .LE. 2)THEN
*     Rectangular Cell array Parallel to axes
*        Set swappable indices
         IF(IREFL .NE. 2*(IREFL/2))THEN
*        Swap X-Y
            IA = 2
            IB = 1
         ELSE
*        No Swap
            IA = 1
            IB = 2
         ENDIF
      ENDIF

*    get coords of pixel centre of (ILX,ITY)
      RX = FLOAT(ILX) + 0.5
      RY = FLOAT(ITY) + 0.5
*
* get enough workspace to hold a raster row:
      CALL GKSTAL(KINTGS,IRX-ILX+1,IRR)
      IF (KERROR.NE.0) GOTO 8888


*     Select case
      GOTO(1000,2000,3000)ICASE

********************
*                  *
*     CASE 1       *
*                  *
********************

*** One-to-one mapping between cells and pixels ***
 1000 CONTINUE

* compute inverse transformation
* (NB for columns 1 & 2:
* the non-zero elements are +-1 and the inverse is just the transpose)
      CALL GKMTIV(TRNCEL,TRCINV)
      INCRA = NINT(TRNCEL(IA,1))
C      TRCINV(3,IA) = -TRNCEL(3,1)*INCRA
      INCRB = NINT(TRNCEL(IB,2))
C      TRCINV(3,IB) = -TRNCEL(3,2)*INCRB

* get centre of pixel (ILX,ITY) in integer cell space
      IYC(IA) = INT(INCRA*RX + TRCINV(3,IA))
      IYC(IB) = INT(INCRB*RY + TRCINV(3,IB))
*     put in cell-array offset
      IYC(1) = IYC(1) + KWI3
      IYC(2) = IYC(2) + KWI4

* for each scanline ...
      DO 1200  IY=ITY,IBY,-1

* begin scanline in integer cell space
        ICC(1) = IYC(1)
        ICC(2) = IYC(2)

* for each pixel ...
        DO 1100  IX=ILX,IRX

* copy colour index
           KSTACK(IRR+IX-ILX) = ICA(LSCAN*(ICC(2)-1) + ICC(1))

* move to next pixel in real cell-space
           ICC(IA) = ICC(IA) + INCRA
 1100   CONTINUE

* scanline complete: output it
        CALL ROSUB(FLOAT(ILX),FLOAT(IY),
     :        IRX-ILX+1,1,IRX-ILX+1,KSTACK(IRR))

* move to next scanline in cell space (NB: decreasing Y)
        IYC(IB) = IYC(IB) - INCRB
 1200 CONTINUE
      GOTO 8888


********************
*                  *
*     CASE 2       *
*                  *
********************

*** Cell Array parellel to axes, but no one-to-one cell/pixel ***
*** correspondence.                                           ***
 2000 CONTINUE

* compute inverse transformation
      CALL GKMTIV(TRNCEL,TRCINV)
C      TRCINV(1,IA) = 1.0/TRNCEL(IA,1)
C      TRCINV(3,IA) = -TRNCEL(3,1)*TRCINV(IA,1)
C      TRCINV(2,IB) = 1.0/TRNCEL(IB,2)
C      TRCINV(3,IB) = -TRNCEL(3,2)*TRCINV(IB,2)

* get centre of pixel (ILX,ITY) in real cell space
      PYC(IA) = TRCINV(1,IA)*RX + TRCINV(3,IA)
      PYC(IB) = TRCINV(2,IB)*RY + TRCINV(3,IB)
*     put in cell-array offset
      PYC(1) = PYC(1) + KWI3
      PYC(2) = PYC(2) + KWI4

* set previous row indicator to something different from first row
      LCB = KNIL

* for each scanline ...
      DO 2200  IY=ITY,IBY,-1

* fill in raster row, only if different from previous row (or is first)
         IF(INT(PYC(IB)) .NE. LCB)THEN

* begin scanline in real cell space
           PCC(1) = PYC(1)
           PCC(2) = PYC(2)

* for each pixel ...
           DO 2100  IX=ILX,IRX

* round position of pixel in real cell space to integer cell
              ICC(1)=INT(PCC(1))
              ICC(2)=INT(PCC(2))

* copy colour index
              KSTACK(IRR+IX-ILX) = ICA(LSCAN*(ICC(2)-1) + ICC(1))

* move to next pixel in real cell-space
              PCC(IA) = PCC(IA) + TRCINV(1,IA)
 2100      CONTINUE
           LCB = INT(PYC(IB))
         ENDIF

* scanline complete: output it
         CALL ROSUB(FLOAT(ILX),FLOAT(IY),
     :        IRX-ILX+1,1,IRX-ILX+1,KSTACK(IRR))

* move to next scanline in cell space (NB: decreasing Y)
         PYC(IB) = PYC(IB) - TRCINV(2,IB)
 2200 CONTINUE
      GOTO 8888


********************
*                  *
*     CASE 3       *
*                  *
********************

*** General Case
 3000 CONTINUE

* As a result of segment/insert transformations, the cell axes are no
* longer parallel to device axes: the cellarray has a rotation component

* The DC transformed positions of the four corners of the CellArray
* are in the arrays DCX and DCY, in the order P,R,Q and P-R+Q.


* compute inverse transformation
      CALL GKMTIV(TRNCEL,TRCINV)

* get centre of pixel (ILX,ITY) in real cell space (including offset)
      PYC(1) = TRCINV(1,1)*RX +TRCINV(2,1)*RY +TRCINV(3,1) +KWI3
      PYC(2) = TRCINV(1,2)*RX +TRCINV(2,2)*RY +TRCINV(3,2) +KWI4

* Initialise cell space limits
      ICL = KWI3
      ICR = KWI3 + KWI5
      ICT = KWI4 + KWI6
      ICB = KWI4

* for each scanline ...
      DO 3200  IY=ITY,IBY,-1

* initialise left and right limits
         ILEL=IRX
         IRIL=ILX

* begin scanline in real cell space
         PCC(1) = PYC(1)
         PCC(2) = PYC(2)

* for each pixel ...
         DO 3100  IX=ILX,IRX

* backtransform pixel X-coord, add to backtransformed Y-coord
* round position of pixel in real cell space to integer cell
            ICC(1)=INT(PCC(1))
            ICC(2)=INT(PCC(2))

* only use pixels that map inside cell array
            IF (((ICC(1) .GE. ICL) .AND. (ICC(1) .LT. ICR))
     :      .AND. ((ICC(2) .GE. ICB) .AND. (ICC(2) .LT. ICT)))
     :      THEN

* copy colour index
               KSTACK(IRR+IX-ILX) = ICA(LSCAN*(ICC(2)-1) + ICC(1))

* update left and right limits
               IF (IX.LT.ILEL) ILEL=IX
               IF (IX.GT.IRIL) IRIL=IX
            ELSE
* if cell array has been crossed by this row, the scanline is complete
               IF(IRIL .GE. ILEL)GOTO 3110
            ENDIF
* move to next pixel in real cell-space
            PCC(1) = PCC(1) + TRCINV(1,1)
            PCC(2) = PCC(2) + TRCINV(1,2)
 3100    CONTINUE

* scanline complete: output relevant portion
 3110    IF (IRIL.GE.ILEL)
     :     CALL ROSUB(FLOAT(ILEL),FLOAT(IY),
     :          IRIL-ILEL+1,1,IRIL-ILEL+1,KSTACK(IRR+ILEL-ILX))

* move to next scanline in cell space (NB: decreasing Y)
         PYC(1) = PYC(1) - TRCINV(2,1)
         PYC(2) = PYC(2) - TRCINV(2,2)
 3200 CONTINUE


*   end of CASE 3  *
*                  *
********************

 8888 CONTINUE
* Deallocate workspace
      CALL GKSTDA(KINTGS,IRR)


 9999 CONTINUE

      END
