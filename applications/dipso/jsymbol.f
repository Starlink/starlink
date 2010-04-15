      SUBROUTINE JSYMBOL(X,Y,NPTS,N,STYLE,IPAL,COLOUR,
     :                   FILL,EXPAND,ANGLE)
*+
*
*  - - - - - -
*   X S Y M B O L
*  - - - - - -
*
*  Plots input array (X,Y) using one of a range of symbols
*  A symbol's co-ords are generated, and then drawn by GKS/SGS and
*  optionally filled.
*
*  Given:
*    X,Y       r      x,y co-ords of array to be plotted
*    NPTS      i      No. of points in array
*    N         i      Symbol sides (see below)
*    STYLE     i      Symbol style (see below)
*    FILL      l      True if symbols are required to be filled.
*                     False for empty symbols
*    EXPAND    r      Factor by which symbol size is multiplied
*    ANGLE     r      Angle in radians by which symbols are rotated

*  Called: GSFAIS, GSFACI, GSPLCI, GFA, LINE, PLOTIT( 0, 0, 2 ), SGS_FLUSH
*  Called: snx_AGUGX, snx_AGUGY, snx_AGCS

* SYMBOL TYPE - this is determined by N and STYLE

*  N is the number of sides or points on a symbol

* STYLE:  The current version of the this routine offers
*          four symbol styles as follows:

*  STYLE is     1 -- polygons (large N generates a circle)
*               2 -- Star
*               3 -- Asterix
*               4 -- Arrow (For this option NSIDES is irrelevant)
*
** The symbols will be filled or empty (if appropriate) according to whether
*  FILL is true or false

** Base size for symbol unit is set at 1/100th of xgrid length
*  Actual size is changed by expansion factor EXPAND
****
**********************************************************************
*      IMPLICIT NONE

* Subroutine arguments
      INTEGER NPTS,N,STYLE
      REAL X(NPTS),Y(NPTS)
      LOGICAL FILL
      REAL EXPAND,ANGLE
      INTEGER NS1

*    IPAL           Colour index
*    COLOUR         True if device supports colour, false otherwise
      LOGICAL COLOUR
      INTEGER IPAL

* Polygon array variables
      REAL XS(1000),YS(1000)
      INTEGER NS

*  Integer to set fill/nofill parameter
      INTEGER GSOLID


*  ASPRATIO is  Aspect ratio of current device
*  (This is used to ensure symbols have same appearance regardless of device)
*  GRIDRATIO is Aspect ratio of current grid window
*  (This is used to ensure symbols have same appearance regardless of shape
*   of grid window)

      REAL ASPRATIO,GRIDRATIO,XYRATIO
      REAL X1,X2,Y1,Y2,XM,YM

      INTEGER I,J,K,IERROR
      REAL XX,YY
      REAL snx_AGUGX, snx_AGUGY

      REAL PI
      PARAMETER (PI=3.14159265)


*****************************************************************************
*
*   Inquire polymarker colour index
      CALL GQPMCI(IERROR,IPAL)
      IF(IERROR.NE.0)GOTO 999

      CALL SGS_IZONE(X1,X2,Y1,Y2,XM,YM)
      GRIDRATIO=(X2-X1)/(Y2-Y1)
      ASPRATIO=XM/YM
      XYRATIO=ASPRATIO/GRIDRATIO

** Set up GKS fill or nofill parameter
      IF(FILL)THEN
         GSOLID=1
      ELSE
         GSOLID=0
      ENDIF
      CALL GSFAIS(GSOLID)

** Select chosen colour for symbols (colour devices only)
      IF(COLOUR)THEN
         CALL GSFACI(IPAL)
         CALL GSPLCI(IPAL)
      ENDIF

        CALL SNX_TO('SGS')

** Draw the symbols

         DO I=1,NPTS
            XX=SNX_AGUGX(X(I))
            YY=SNX_AGUGY(Y(I))
            IF(STYLE.EQ.1)THEN
**      Draw Polygons
               NS=N
               CALL POLY(XX,YY,N,ANGLE,EXPAND,XYRATIO,XS,YS,NS)
            ELSEIF(STYLE.EQ.2)THEN
**      Draw Stars
               NS=2*N
               CALL STAR(XX,YY,N,ANGLE,EXPAND,XYRATIO,XS,YS,NS)
            ELSEIF(STYLE.EQ.3)THEN
**      Draw n-pointed asterix style
               NS=N+1
               CALL ASTERIX(XX,YY,N,ANGLE,EXPAND,XYRATIO,XS,YS,NS)
            ELSEIF(STYLE.EQ.4)THEN
**      Draw Arrows
               NS=5
               CALL ARROW(XX,YY,N,ANGLE,EXPAND,XYRATIO,XS,YS,NS)
            ELSE
               WRITE(*,*)' Invalid symbol style'
               GOTO 999
            ENDIF

**  Draw the symbol (filled or empty as specified above)
         IF(STYLE.LE.2)THEN
            IF(GSOLID.EQ.0)THEN
               NS1=NS+1
               XS(NS1)=XS(1)
               YS(NS1)=YS(1)
               CALL GPL(NS1,XS,YS)
            ELSE
               CALL GFA(NS,XS,YS)
            ENDIF

         ELSEIF(STYLE.EQ.3)THEN
            DO K=1,NS-1
               CALL SGS_LINE(XS(K),YS(K),XS(K+1),YS(K+1))
            ENDDO
         ELSEIF(STYLE.EQ.4)THEN
            IF(GSOLID.EQ.0)THEN
               NS1=NS+1
               XS(NS1)=XS(1)
               YS(NS1)=YS(1)
               CALL GPL(NS1,XS,YS)
            ELSE
               CALL GFA(3,XS,YS)
            ENDIF
            CALL SGS_LINE(XS(4),YS(4),XS(5),YS(5))
         ENDIF
      ENDDO
** Flush graphics buffers
      CALL SGS_FLUSH
      CALL PLOTIT( 0, 0, 2 )
      CALL SNX_TO('NCAR')
999   CONTINUE
      RETURN
      END
