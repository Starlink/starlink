*---------------------------------------------------------------------


      SUBROUTINE GK0XSS(IXCOL, IXDYN, IXXSP, IXYSP, NCOLS, IXSIZE,
     :  IYSIZE)

*---------------------------------------------------------------------
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Selects a suitable size and depth for the window
*
*  MAINTENANCE LOG
*  ---------------
*     Maintenance log is incorporated in main driver routine
*
*  ARGUMENTS
*  ---------
*     INP IXCOL - Maximum number of colours available
*     INP IXDYN - Colour table is dynamic (GYES) or not (GNO)
*     INP IXXSP, IXYSP - Screen size in pixels
*     OUTP NCOLS - Number of colours chosen
*     OUTP IXSIZE, IYSIZE - Window size chosen
*
      INTEGER IXCOL, IXDYN, IXXSP, IXYSP, NCOLS, IXSIZE, IYSIZE
*
*  COMMON BLOCK USAGE
*  ------------------
*
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwdt.cmn'
*
*  LOCALS
*  ------
*     IUCOLS   User prefered number of colours
*     IUXS       "      "    x size
*     IUYS       "      "    y  "
*
      INTEGER IUCOLS, IUXS, IUYS

      INTEGER DEFRX, DEFRY
      PARAMETER ( DEFRX = 780, DEFRY = 576)
*---------------------------------------------------------------------
*
*  Choose the number of colours.
*     If the colour table is not dynamic then use the maximum possible
*     since all windows can share the same table without interference.
*     If it is dynamic then the colour table has to be shared out
*     between all the windows on the display.

      CALL GK0XTL( 'COLOURS', IUCOLS )
      IF (IXDYN.EQ.GNO) THEN
         IF ( IUCOLS.EQ.0) THEN
            NCOLS = IXCOL
         ELSE
            NCOLS = MIN( IXCOL, IUCOLS)
         END IF
      ELSE
         IF (IUCOLS.EQ.0) IUCOLS = IXCOL/4
         NCOLS = MIN( IXCOL, IUCOLS)
      END IF

*  Choose the window size (assuming a border width of 10 pixels)
      CALL GK0XTL( 'XSIZE', IUXS )
      IF (IUXS.GT.0) THEN
         IXSIZE = MIN( IUXS, IXXSP - 40)
      ELSE
         IXSIZE = DEFRX
      END IF
      CALL GK0XTL( 'YSIZE', IUYS )
      IF (IUYS.GT.0) THEN
         IYSIZE = MIN( IUYS, IXYSP - 30)
      ELSE
         IYSIZE = DEFRY
      END IF
      END
