C# IL>=a, OL>=0
      SUBROUTINE GKCELA(ISIZ,ICA,FILSUB,COLSUB,DCX,DCY,TRNCEL,CLIP)
*
* (C) COPYRIGHT ICL & SERC  1988
*
*---------------------------------------------------------------------
*  Type of routine:    UTILITY
*  Author:             NGB/KEVP
*
      INCLUDE '../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Cell array utility using hardware fill area
*
*     A Cell Array of N by M Cells is mapped onto a WC rectangle
*     delimited by its origin, and the two opposite corners, P and Q.
*
*     Derived from GKCELL

*  MAINTENANCE LOG
*  ---------------
*     16/03/88  KEVP  Created
*     31/07/90  PLP   Added include/check.inc, brought commenting in
*                     line with the standard format.
*     27/03/91  KEVP  Corrected calculation of cell position, when start
*                     of cell row is clipped (C48).
*
*  ARGUMENTS
*  ---------
*     INP ISIZ    Cell Array Dimensions
*     INP ICA     Cell Array
*     INP FILSUB  Device Fill Area output routine
*     INP COLSUB  Device Fill Area colour setting routine
*     INP DCX,DCY Cell array box in DC obtainable from GKCELT
*     INP TRNCEL  Cell array transformation as obtained by GKCELT
*     INP CLIP    True, if clipping is required
*
      INTEGER ISIZ, ICA(ISIZ)
      REAL    DCX(4),DCY(4), TRNCEL(3,2)
      LOGICAL CLIP
      EXTERNAL FILSUB, COLSUB
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
      INCLUDE '../include/gkio.par'
      INCLUDE '../include/GKS_PAR'
      INCLUDE '../include/gkwca.cmn'
      INCLUDE '../include/gkwkd.cmn'
      INCLUDE '../include/gkerr.cmn'
      INCLUDE '../include/gkstk.cmn'
*
*  LOCALS
*  ------
*     CLIPAR True, if clipping is to be done on cell array box
*     CLIPBE True, if end of box is to be clipped
*     CLIPBS True, if start of box is to be clipped
*     CLIPBX True, if box is to be clipped
*     CLIPRE True, if end of row is to be clipped
*     CLIPRS True, if start of row is to be clipped
*     CLIPRW True, if row is to be clipped
*     IC     Corner index
*     ICA    Index for full cell array ICA
*     ICOL   Current cell colour
*     ICOLM  Cell Columns (do loop index)
*     IPOLY  Number of polygons in clipped box (always 1)
*     IPLIST Indicates number of vertices in clipped box
*     IROW   Cell Rows (do loop index)
*     IROW1  First row after outer clipping
*     IRWEND End of row after outer clipping
*     IRWST  Start of row after outer clipping
*     ISCAN  Cell Array first dimension (total length of row)
*     ISX    Offset for submatrix of colour array
*     ISY
*     LROW   Last row after outer clipping
*     MAXCLM Maximum column after outer clipping
*     MAXROW Maximum row after outer clipping
*     MINCLM Minimum column after outer clipping
*     MINROW Minimum row after outer clipping
*     NCOLM  Number of columns in cell array
*     NROW   Number of rows in cell array
*     RCOLM  Clip rectangle in terms of cell columns
*     RROW   Clip rectangle in terms of cell rows
*     TRCINV Inverse cell array transformation
*     XCBOX  Box in cell array (DC)
*     YCBOX
*     XFIL   Clipped box (up to 8 vertices)
*     YFIL
*     XM,YM  Clipping rectangle as 4 corners (DC)
*     XMIN   Clipping rectangle as max-min (DC)
*     XMAX
*     YMIN
*     YMAX
*     XRWST  Start of row in DC
*     YRWST


      LOGICAL CLIPAR, CLIPBS,CLIPBE,CLIPBX, CLIPRS,CLIPRE,CLIPRW
      INTEGER    ISCAN,ISX,ISY,NCOLM,NROW
      INTEGER    IRWST,IRWEND,IROW1,LROW,ICOLM
      INTEGER    MINCLM,MAXCLM,MINROW,MAXROW
      INTEGER    IROW,ICAI,ICOL,IC
      INTEGER    IPOLY,IPLIST(2)
      REAL XMIN,YMIN,XMAX,YMAX, XRWST,YRWST
      REAL RCOLM(4),RROW(4), XCBOX(4),YCBOX(4), XFIL(8),YFIL(8)
      REAL TRCINV(3,2), XM(4),YM(4)

*  ERRORS
*  ------
*
*  ALGORITHM
*  ---------
*     The open segment transformation is reversed on the corner pt
*     cell vectors.
*
*     A Cell Array is NCOLM by NROW Cells mapped onto a DC rectangle
*     with a corner P=(PX,PY) at the cell (1,1).
*     If (X,Y) in DC is a point in cell (I,J) of the cell array, then
*     (X+BASEX,Y) is the corrosponding point in cell (I+1,J) and
*     (X,Y+DOWNY) is the corrosponding point in cell (I,J+1).
*
*     The cell array with boxes using FILSUB.
*     There is one box per cell ,unless the next cell in the row
*     is the same colour in which case the box is extended to
*     include it.
*
*     The colour of the cells are changed with COLSUB.
*
*     If clipping is required,
*       the cell array box is tested to see if it needs clipping.
*       If not,
*            no clipping is done.
*       Otherwise,
*            all rows and columns entirely outside the
*            clipping rectange are eliminated (outer clipping).
*            Each row is then tested for clipping.
*            If not,
*               no clipping is done for that row.
*            Otherwise,
*               each box is tested for clipping.
*               If not,
*                  FILSUB is used straight away.
*               Otherwise,
*                  the box is clipped by GKPCLP beforehand
*                  using FILSUB.
*
*     When a row box is tested for clipping the start and end are
*     tested separately, so that they can contribute to the boxes
*     at the start and end of the row.
*     When a box of cells of same colour is tested for clipping,
*     only the end is tested. The start is either the end of the
*     previous box or the start of the row (already tested).
*     The end of the last box in a row is not tested because it's
*     the end of the row box (already tested).
*
*
*  COMMENTS
*  --------
*     This utility is not efficient if the cells are very small.
*     Hence, it should be used in preference to GKCELL only if,
*          (1) the cell array is oblique
*       or (2) the cell array is parallel to the X and Y axes and
*              the cells are several pixels tall in the Y direction.
*
*     Such data is available from TRNCEL.
*          (1) The cell array is oblique, if and only if
*                   TRNCEL(1,1) & TRNCEL(2,2) are not both zero
*              and  TRNCEL(2,1) & TRNCEL(1,2) are not both zero.
*          (2) The cell height in DC is the maximum absolute value
*              of TRNCEL(1,2) and TRNCEL(2,2) (the other is zero),
*              when the cell array is not oblique.
*
*   Unlike GKCELL, this utility does not assume DC to be Raster coords.
*
*---------------------------------------------------------------------

* Extract cell array dimensions and associated integers
      ISCAN = KWI1
      ISX   = KWI3
      ISY   = KWI4
      NCOLM = KWI5
      NROW  = KWI6

* Reject Cell array of non-positive dimensions
      IF ((NCOLM .LE. 0) .OR. (NROW .LE. 0)) GOTO 100

* Initialise cell array limits and clipping flags
      IRWST  = 1
      IRWEND = NCOLM
      IROW1  = 0
      LROW   = NROW -1
      CLIPAR = CLIP
      CLIPRS = .FALSE.
      CLIPRE = .FALSE.
      CLIPRW = .FALSE.
      CLIPBS = .FALSE.
      CLIPBE = .FALSE.
      CLIPBX = .FALSE.

* If clipping,
      IF (CLIP) THEN

* Extract total clip rectangle using W/S ID from Comms Area:
        XMIN = QWCLXL(KWKIX)
        XMAX = QWCLXR(KWKIX)
        YMIN = QWCLYB(KWKIX)
        YMAX = QWCLYT(KWKIX)

* Test whether cell array box is clipped
        DO 10 IC=1,4
           IF(DCX(IC) .LT. XMIN)GOTO 11
           IF(XMAX .LT. DCX(IC))GOTO 11
           IF(DCY(IC) .LT. YMIN)GOTO 11
           IF(YMAX .LT. DCY(IC))GOTO 11
   10   CONTINUE
        CLIPAR = .FALSE.
*       Here, cell array rectangle is not clipped.
*       Hence from now on, we can forget about clipping.
   11   CONTINUE
      ENDIF

      IF(CLIPAR)THEN
* Set up clip box for transformation to cell-space
        XM(1) = XMIN
        YM(1) = YMIN
        XM(2) = XMAX
        YM(2) = YMIN
        XM(3) = XMAX
        YM(3) = YMAX
        XM(4) = XMIN
        YM(4) = YMAX
*  Invert cell array transformation to eliminate
*  any rows or columns entirely outside the clipping rectangle.
        CALL GKMTIV (TRNCEL,TRCINV)
        CALL GKMTXF (TRCINV,4,XM,YM,RCOLM,RROW)
        MAXCLM = 0
        MAXROW = 0
        MINCLM = NCOLM+1
        MINROW = NROW+1
        DO 15 IC=1,4
           IF(RCOLM(IC) .GT. MAXCLM) MAXCLM=INT(RCOLM(IC))
           IF(RCOLM(IC) .LT. MINCLM) MINCLM=INT(RCOLM(IC))
           IF(RROW(IC)  .GT. MAXROW) MAXROW=INT(RROW(IC))
           IF(RROW(IC)  .LT. MINROW) MINROW=INT(RROW(IC))
   15   CONTINUE
        MAXCLM = MAXCLM+1
        MAXROW = MAXROW+1

        IF(IRWST  .LT. MINCLM)IRWST = MINCLM
        IF(IRWEND  .GT. MAXCLM)IRWEND = MAXCLM
        IF(IROW1  .LT. MINROW-1)IROW1 = MINROW - 1
        IF(LROW  .GT. MAXROW-1)LROW = MAXROW - 1
      ENDIF

* Produce the cells using boxes
      DO 22 IROW=IROW1,LROW
*        Start of row
         XRWST = TRNCEL(3,1) + TRNCEL(2,1)*IROW
         YRWST = TRNCEL(3,2) + TRNCEL(2,2)*IROW
*        Start of box (and row)
         XCBOX(1) = XRWST
         XCBOX(2) = XRWST + TRNCEL(2,1)
         YCBOX(1) = YRWST
         YCBOX(2) = YRWST + TRNCEL(2,2)

*        Test row start for clipping
         IF(CLIPAR)THEN
           CLIPRS = .TRUE.
           IF((XMIN.LE.XCBOX(1)).AND.(XCBOX(1).LE.XMAX))THEN
            IF((XMIN.LE.XCBOX(2)).AND.(XCBOX(2).LE.XMAX))THEN
             IF((YMIN.LE.YCBOX(1)).AND.(YCBOX(1).LE.YMAX))THEN
              CLIPRS = ((YCBOX(2).LT.YMIN).OR.(YMAX.LT.YCBOX(2)))
             ENDIF
            ENDIF
           ENDIF
*          and row end
           CLIPRE = .TRUE.
           XCBOX(4) = XRWST + TRNCEL(1,1)*IRWEND
           YCBOX(4) = YRWST + TRNCEL(1,2)*IRWEND
           XCBOX(3) = XCBOX(4) + TRNCEL(2,1)
           YCBOX(3) = YCBOX(4) + TRNCEL(2,2)
           IF((XMIN.LE.XCBOX(3)).AND.(XCBOX(3).LE.XMAX))THEN
            IF((XMIN.LE.XCBOX(4)).AND.(XCBOX(4).LE.XMAX))THEN
             IF((YMIN.LE.YCBOX(3)).AND.(YCBOX(3).LE.YMAX))THEN
              CLIPRE = ((YCBOX(4).LT.YMIN).OR.(YMAX.LT.YCBOX(4)))
             ENDIF
            ENDIF
           ENDIF
           CLIPRW = CLIPRS .OR. CLIPRE
           CLIPBS = CLIPRS
           CLIPBE = .FALSE.
         ENDIF
*
*        Select colour
         ICAI = ISCAN*(IROW+ISY-1) + IRWST+ISX-1
         ICOL = ICA (ICAI)
*
*        Scan row for colour change ,if so then complete box
         DO 21 ICOLM=IRWST+1,IRWEND
            ICAI = ISCAN*(IROW+ISY-1) + ICOLM+ISX-1
            IF(ICOL .NE. ICA(ICAI))THEN

*             Complete box definition
              XCBOX(4) = XRWST + TRNCEL(1,1)*(ICOLM-1)
              YCBOX(4) = YRWST + TRNCEL(1,2)*(ICOLM-1)
              XCBOX(3) = XCBOX(4) + TRNCEL(2,1)
              YCBOX(3) = YCBOX(4) + TRNCEL(2,2)

*             Test end of box for clipping
              IF(CLIPRW)THEN
                 CLIPBE = .TRUE.
        IF((XMIN.LE.XCBOX(3)).AND.(XCBOX(3).LE.XMAX))THEN
         IF((XMIN.LE.XCBOX(4)).AND.(XCBOX(4).LE.XMAX))THEN
          IF((YMIN.LE.YCBOX(3)).AND.(YCBOX(3).LE.YMAX))THEN
           CLIPBE = ((YCBOX(4).LT.YMIN).OR.(YMAX.LT.YCBOX(4)))
          ENDIF
         ENDIF
        ENDIF
                 CLIPBX = CLIPBS .OR. CLIPBE
              ENDIF

*             Set Colour and draw box clipping if reqired
              CALL COLSUB (ICOL)
              IF(CLIPBX)THEN
                IPOLY = 1
                IPLIST(1) = 4
                CALL GKPCLP (IPOLY,4,IPLIST,XCBOX,YCBOX,XMIN,XMAX,
     :                       YMIN,YMAX,XFIL,YFIL)
                IF(IPOLY .EQ. 1) CALL FILSUB (IPLIST(1),XFIL,YFIL)
              ELSE
                CALL FILSUB (4,XCBOX,YCBOX)
              ENDIF

*             Select colour for next box
              ICOL = ICA (ICAI)
*
*             Define start of next box
              XCBOX(1) = XCBOX(4)
              XCBOX(2) = XCBOX(3)
              YCBOX(1) = YCBOX(4)
              YCBOX(2) = YCBOX(3)
              CLIPBS   = CLIPBE
           ENDIF
   21    CONTINUE
*
*        Complete last box in row
*
         XCBOX(4) = XRWST + TRNCEL(1,1)*IRWEND
         YCBOX(4) = YRWST + TRNCEL(1,2)*IRWEND
         XCBOX(3) = XCBOX(4) + TRNCEL(2,1)
         YCBOX(3) = YCBOX(4) + TRNCEL(2,2)
*
*        Set colour and draw box clipping if required
         CALL COLSUB(ICOL)
         IF(CLIPBS .OR. CLIPRE)THEN
            IPOLY = 1
            IPLIST(1) = 4
            CALL GKPCLP (IPOLY,4,IPLIST,XCBOX,YCBOX,XMIN,XMAX,
     :                   YMIN,YMAX,XFIL,YFIL)
            IF(IPOLY .EQ. 1) CALL FILSUB (IPLIST(1),XFIL,YFIL)
         ELSE
           CALL FILSUB(4,XCBOX,YCBOX)
         ENDIF
   22 CONTINUE

  100 CONTINUE
      RETURN

      END
