C*PGHI2D -- cross-sections through a 2D data array
C%void cpghi2d(const float *data, int nxv, int nyv, int ix1, \
C% int ix2, int iy1, int iy2, const float *x, int ioff, float bias, \
C% Logical center, float *ylims);
C+
      SUBROUTINE PGHI2D (DATA, NXV, NYV, IX1, IX2, IY1, IY2, X, IOFF,
     1                   BIAS, CENTER, YLIMS)
      INTEGER NXV, NYV, IX1, IX2, IY1, IY2
      REAL    DATA(NXV,NYV)
      REAL    X(IX2-IX1+1), YLIMS(IX2-IX1+1)
      INTEGER IOFF
      REAL    BIAS
      LOGICAL CENTER
C
C Plot a series of cross-sections through a 2D data array.
C Each cross-section is plotted as a hidden line histogram.  The plot
C can be slanted to give a pseudo-3D effect - if this is done, the
C call to PGENV may have to be changed to allow for the increased X
C range that will be needed.
C
C Arguments:
C  DATA   (input)  : the data array to be plotted.
C  NXV    (input)  : the first dimension of DATA.
C  NYV    (input)  : the second dimension of DATA.
C  IX1    (input)
C  IX2    (input)
C  IY1    (input)
C  IY2    (input)  : PGHI2D plots a subset of the input array DATA.
C                    This subset is delimited in the first (x)
C                    dimension by IX1 and IX2 and the 2nd (y) by IY1
C                    and IY2, inclusively. Note: IY2 < IY1 is
C                    permitted, resulting in a plot with the
C                    cross-sections plotted in reverse Y order.
C                    However, IX2 must be => IX1.
C  X      (input)  : the abscissae of the bins to be plotted. That is,
C                    X(1) should be the X value for DATA(IX1,IY1), and
C                    X should have (IX2-IX1+1) elements.  The program
C                    has to assume that the X value for DATA(x,y) is
C                    the same for all y.
C  IOFF   (input)  : an offset in array elements applied to successive
C                    cross-sections to produce a slanted effect.  A
C                    plot with IOFF > 0 slants to the right, one with
C                    IOFF < 0 slants left.
C  BIAS   (input)  : a bias value applied to each successive cross-
C                    section in order to raise it above the previous
C                    cross-section.  This is in the same units as the
C                    data.
C  CENTER (input)  : if .true., the X values denote the center of the
C                    bins; if .false. the X values denote the lower
C                    edges (in X) of the bins.
C  YLIMS  (input)  : workspace.  Should be an array of at least
C                    (IX2-IX1+1) elements.
C--
C 21-Feb-1984 - Keith Shortridge.
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      LOGICAL FIRST,PENDOW,HPLOT,VPLOT
      INTEGER IY,INC,IX,NELMX,IXPT,NOFF
      REAL CBIAS,YNWAS,XNWAS,YN,XN,VTO,VFROM,YLIMWS,YLIM
      REAL PGHIS1
      LOGICAL PGNOTO
C
C Check arguments.
C
      IF (IX1.GT.IX2) RETURN
      IF (PGNOTO('PGHI2D')) RETURN
      CALL PGBBUF
C
C Check Y order.
C
      IF (IY1.GT.IY2) THEN
         INC = -1
      ELSE
         INC = 1
      END IF
C
C Clear limits array.
C
      NELMX = IX2 - IX1 + 1
      DO 10 IX=1,NELMX
         YLIMS(IX) = PGYBLC(PGID)
 10   CONTINUE
C
C Loop through Y values.
C
      NOFF = 0
      CBIAS = 0.
      DO 200 IY=IY1,IY2,INC
         YNWAS = CBIAS
         YLIMWS = YNWAS
         XNWAS = PGHIS1(X,NELMX,CENTER,1+NOFF)
         PENDOW = .FALSE.
         FIRST = .TRUE.
         IXPT = 1
C
C Draw histogram for this Y value.
C
         DO 100 IX=IX1,IX2
            YN = DATA(IX,IY) + CBIAS
            XN = PGHIS1(X,NELMX,CENTER,IXPT+NOFF+1)
            YLIM = YLIMS(IXPT)
C
C Given X and Y old and new values, and limits, see which parts of the
C lines are to be drawn.
C
            IF (YN.GT.YLIM) THEN
               YLIMS(IXPT) = YN
               HPLOT = .TRUE.
               VPLOT = .TRUE.
               VTO = YN
               VFROM = YLIM
               IF (YNWAS.GT.YLIMWS) VFROM = YNWAS
            ELSE
               HPLOT = .FALSE.
               IF (YNWAS.GT.YLIMWS) THEN
                  VPLOT = .TRUE.
                  VFROM = YNWAS
                  VTO = YLIM
               ELSE
                  VPLOT = .FALSE.
               END IF
            END IF
C
C Plot the bin.
C
            IF (VPLOT) THEN
               IF (.NOT.PENDOW) THEN
                  IF (FIRST) THEN
                     CALL GRMOVA(XNWAS,MAX(VTO,CBIAS))
                     FIRST = .FALSE.
                  ELSE
                     CALL GRMOVA(XNWAS,VFROM)
                  END IF
               END IF
               CALL GRLINA(XNWAS,VTO)
               IF (HPLOT) THEN
                  CALL GRLINA(XN,YN)
               END IF
            END IF
            PENDOW = HPLOT
            YLIMWS = YLIM
            YNWAS = YN
            XNWAS = XN
            IXPT = IXPT + 1
 100     CONTINUE
         IF (PENDOW) CALL GRLINA(XN,MAX(YLIM,CBIAS))
C
C If any offset in operation, shift limits array to compensate for it.
C
         IF (IOFF.GT.0) THEN
            DO 110 IX=1,NELMX-IOFF
               YLIMS(IX) = YLIMS(IX+IOFF)
 110        CONTINUE
            DO 120 IX=NELMX-IOFF+1,NELMX
               YLIMS(IX) = PGYBLC(PGID)
 120        CONTINUE
         ELSE IF (IOFF.LT.0) THEN
            DO 130 IX=NELMX,1-IOFF,-1
               YLIMS(IX) = YLIMS(IX+IOFF)
 130        CONTINUE
            DO 140 IX=1,-IOFF
               YLIMS(IX) = PGYBLC(PGID)
 140        CONTINUE
         END IF
         CBIAS = CBIAS + BIAS
         NOFF = NOFF + IOFF
 200  CONTINUE
C
      CALL PGEBUF
      END
