      SUBROUTINE GRAREA(ID,X0,Y0,XSIZE,YSIZE)
*+
*
*     - - - - - - - -
*       G R A R E A    (GKS emulation of GRPCKG)
*     - - - - - - - -
*
*   Selects the specified device and defines the plotting sub area. If
*   either the width or height are negative the full plotting area is
*   selected.
*
*   Given
*      ID       i     Device identifier (IGNORED)
*      X0       r     Absolute X coordinate of lower left corner
*      Y0       r         "    Y     "       "   "     "    "
*      XSIZE    r     X size in absolute coordinates
*      YSIZE    r     Y   "  "      "        "
*
*   Read from COMMON
*      GRCIDE   i     Device identifier
*      GRXMAX   r()   Workstation size (x)
*      GRYMAX   r()   Workstation size (y)
*      GRXMIN   r()   Workstation origin (x)
*      GRYMIN   r()   Workstation origin (y)
*      GRVPVI   l()   Viewport visible
*
*   Constants from GRECOM.INC
*      TRN      i     Transformation number
*
*   D.L.Terrett  Starlink  Aug 1987
*+
      IMPLICIT NONE

      INTEGER ID
      REAL X0, Y0, XSIZE, YSIZE

      INCLUDE 'grecom.inc'

      INCLUDE 'PGP_ERR'


      REAL X1, X2, Y1, Y2, WINDO(4), VIEWP(4), XSCALE, YSCALE
      INTEGER IERR

      IF (GRCIDE.LE.0) THEN
         CALL ERR_REP('GRNODO', 'GRAREA - No PGPLOT device open',
     :   GRNODO)
      ELSE

*    Update workstation
         CALL GRTERM

*     Get current WC/NDC scaling
         CALL GQNT(TRN,IERR,WINDO,VIEWP)
         IF (IERR.NE.0) THEN
            CALL GRQREP('GRAREA', 'GQNT', IERR)
            GO TO 9999
         ENDIF

*    Assume new viewport is on the display surface
         GRVPVI(GRCIDE) = .TRUE.

*    Constrain the plotting sub area to lie within the display surface
         IF (XSIZE.GT.0.0 .AND. YSIZE.GT.0.0) THEN
            X1 = MAX(X0,0.0)
            Y1 = MAX(Y0,0.0)
            X2 = MIN(X0+XSIZE,GRXMAX(GRCIDE)-GRXMIN(GRCIDE))
            Y2 = MIN(Y0+YSIZE,GRYMAX(GRCIDE)-GRYMIN(GRCIDE))

*        If the limits are now reversed then the viewport lies outside
*        the workstation.
            IF (X1.GE.X2 .OR.Y1.GE.Y2) THEN

*           Set the "viewport off screen flag" and set the viewport to
*           the full display surface.
               GRVPVI(GRCIDE) = .FALSE.
               X1 = 0.0
               Y1 = 0.0
               X2 = GRXMAX(GRCIDE)-GRXMIN(GRCIDE)
               Y2 = GRYMAX(GRCIDE)-GRYMIN(GRCIDE)
            END IF
         ELSE
            X1 = 0.0
            Y1 = 0.0
            X2 = GRXMAX(GRCIDE)-GRXMIN(GRCIDE)
            Y2 = GRYMAX(GRCIDE)-GRYMIN(GRCIDE)
         ENDIF

*     Set new world coordinates
         CALL GSWN(TRN,X1,X2,Y1,Y2)

*     Set new viewport guarding against rounding errors
         XSCALE = (VIEWP(2) - VIEWP(1)) / (WINDO(2) - WINDO(1))
         YSCALE = (VIEWP(4) - VIEWP(3)) / (WINDO(4) - WINDO(3))
         X1 = MAX(0.0,(X1+GRXMIN(GRCIDE))*XSCALE)
         X2 = MIN(1.0,(X2+GRXMIN(GRCIDE))*XSCALE)
         Y1 = MAX(0.0,(Y1+GRYMIN(GRCIDE))*YSCALE)
         Y2 = MIN(1.0,(Y2+GRYMIN(GRCIDE))*YSCALE)
         CALL GSVP(TRN,X1,X2,Y1,Y2)
      END IF

 9999 CONTINUE
      END
