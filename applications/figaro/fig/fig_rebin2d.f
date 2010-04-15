C+
      SUBROUTINE FIG_REBIN2D (DATA,NX,NY,NXRES,NYRES,XRESMIN,XRESMAX,
     :                    XLOG,YRESMIN,YRESMAX,YLOG,NCXY,NCXX,XCOEFF,
     :                    NCYY,NCYX,YCOEFF,XSHIFT,YSHIFT,NDX,NDY,
     :                    MODE,EXTRA,PX,PY,VALS,RESULT)
C
C     F I G _ R E B I N 2 D
C
C     Rebins a two-dimensional array according to a given set of
C     transformations.
C
C     Parameters -  (">" input, "!" modified, "<" output)
C
C     (>) DATA    (Real array DATA(NX,NY)) The array to be
C                 rebinned.
C     (>) NX      (Integer) The x-dimension of the array (ie the
C                 maximum value of the fastest changing index)
C     (>) NY      (Integer) The y-dimension of the array.
C     (>) NXRES   (Integer) The x-dimension of the result array -
C                 does not have to be the same as NX.
C     (>) NYRES   (Integer) The y-dimension of the result array -
C                 does not have to be the same as NY.
C     (>) XRESMIN (Real) The minimum x-value covered by the result
C                 array.  That is, the x-value of the leading edge
C                 of the first element of the array.
C     (>) XRESMAX (Real) The maximum x-value covered by the result
C                 array.  That is, the x-value of the trailing edge
C                 of the last element of the array.
C     (>) XLOG    (Logical) True if the data is to be rebinned
C                 logarithmicaly in x.
C     (>) YRESMIN (Real) The minimum y-value covered by the result
C                 array.
C     (>) YRESMAX (Real) The maximum y-value covered by the result
C                 array.
C     (>) YLOG    (Logical) True if the data is to be rebinned
C                 logarithmicaly in y.
C     (>) NCXY    (Integer) The first dimension of XCOEFF.  The number
C                 of y-terms in the transformation.
C     (>) NCXX    (Integer) The second dimension of XCOEFF.  The
C                 number of x-terms in the transformation.
C     (>) XCOEFF  (Double precision XCOEFF(NCXY,NCXX)) Gives the
C                 coefficients for the x-transformation from pixel
C                 number in DATA to the result coordinate values for
C                 RESULT.  That is, the X coordinate value (X') for
C                 pixel position X,Y is given by the polynomial
C                 X'=Cncxx*X**(NCXX-1)+...C2*X+C1 where the Cs are
C                 Cn=XCOEFF(NCXY,n)*Y**(NCXX-1)+...+XCOEFF(1,n)
C                 Note that this means that the coefficients giving
C                 the constant terms in the polynomials are the
C                 higher numbered elements.  This is possibly a
C                 little unusual.  Note also that for the purposes of
C                 this routine, the first pixel of an array is
C                 regarded as covering the range 0.0 to 1.0
C     (>) NCYY    (Integer) The first dimension of YCOEFF.
C     (>) NCYX    (Integer) The second dimension of YCOEFF.
C     (>) YCOEFF  (Double precision YCOEFF(NCYY,NCYX)) Gives the
C                 coefficients for the y-transformation from pixel
C                 number in DATA to the result coordinate values for
C                 RESULT.
C     (>) XSHIFT  (Real) Data will be shifted XSHIFT pixels in X towards
C                 the higher numbered pixels of RESULT, in addition to
C                 the transformation specified.
C     (>) YSHIFT  (Real) Like XSHIFT, but in the Y-direction.
C     (>) NDX     (Integer) To improve the accuracy of the rebinning,
C                 each original pixel will be split in X into NDX
C                 pixels, and each of these will be rebinned separately.
C                 This also allows interpolation of the original data to
C                 be performed.
C     (>) NDY     (Integer) Like NDY, but in Y.
C     (>) MODE    (Integer) Not used if both NDX and NDY are 1.
C                 Specifies the interpolation to be used to redistribute
C                 the flux in any pixel of the original data into the
C                 NDX by NDY sub-pixels.  Possible values at present are
C                 0 => equal distribution - assume data is flat over
C                 pixel.
C                 1 => fit 2d parabola (Z=a*x**2+b*y**2+c*x*y+d*x+e*y+f)
C                 to the local 9 pixels and use result to interpolate.
C     (>) EXTRA   (Real, perhaps an array) Reserved for use for any
C                 parameters that may be needed by other values of MODE
C                 in future versions.
C     (!) PX      (Real PX(NX+1,2)) Workspace.
C     (!) PY      (Real PY(NX+1,2)) Workspace.
C     (!) VALS    (Real VALS(NDX,NDY)) Workspace.
C     (<) RESULT  (Real RESULT(NXRES,NYRES)) The rebinned data.
C
C     Subroutines / functions used -
C
C     GEN_POLY2D  Evaluate a 2D polynomial
C     FIG_SFIT2D  Fit a 2D parabola to data
C     DSA_GET_WORK_ARRAY
C     DSA_FREE_WORKSPACE
C
C     Note -      It may clarify things to point out that the way to
C                 use this program so that all it does is apply a
C                 linear shift in both X and Y is to set
C                 XCOEFF(1,1)=1.0  XCOEFF(1,2)=XRESMIN
C                 YCOEFF(1,1)=1.0  YCOEFF(2,1)=YRESMIN
C
C                 (The program tests for this condition and will not
C                 waste time evaluating 2D polynomials if it applies.)
C
C     History -   Based on an original program by John Tonry.
C                 First version     KS / CIT 17th Feb 1983
C     Modified -
C
C     8th April 1987   KS / AAO.  GEN_SFIT2D is now FIG_SFIT2D - call
C                      modified.
C     18th March 1993  HME / UoE, Starlink.  GEN_POLY2D requires this
C                      routine to supply a work space. Calls changed.
C     2005 June 1      MJC / Starlink Use CNF_PVAL for pointers to
C                      mapped data.
C+
      IMPLICIT NONE

      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
C
C     Parameters
C
      LOGICAL LINEAR,XLOG,YLOG
      INTEGER NX,NY,NXRES,NYRES,NCXY,NCXX,NCYY,NCYX,NDX,NDY,MODE
      REAL DATA(NX,NY),XRESMIN,XRESMAX,YRESMIN,YRESMAX,XSHIFT,YSHIFT
      REAL EXTRA,PX(NX+1,2),PY(NX+1,2),VALS(NDX,NDY),RESULT(NXRES,NYRES)
      DOUBLE PRECISION XCOEFF(NCXY,NCXX),YCOEFF(NCYY,NCYX)
C
C     Functions
C
      REAL GEN_POLY2D
C
C     Local variables
C
      LOGICAL FLAT,KYOK,SAME,XBOUNDS,XUP,YBOUNDS,YUP
      INTEGER ISX,ISY,IX,IY,IZ,JX,JY
      INTEGER KX,KY,KZ,L1,L2,LT,MX,MY,NXP1
      INTEGER CT,CTSIZE,SLOT,STATUS
      REAL    ALXMIN,ALYMIN
      REAL    DENSITY,DVALUE,EXCESS,FACDXY,FLUX,FNX,FNXL,FNY,FNYL
      REAL    PX1,PX2,PXRESP1,PY1,PY2,PYRESP1,RND
      REAL    X,X1,X2,XBL,XBR,XPOSN,XPX1Y1,XPX1Y2,XPX2Y1,XPX2Y2
      REAL    XSTEP,XSTEPB,XSTEPL,XSTEPR,XSTEPT
      REAL    XTL,XTR,XVALS(9),XV,XVBL,XVBR,XVTL,XVTR
      REAL    Y,Y1,Y2,YBL,YBR,YPOSN,YPX1Y1,YPX1Y2,YPX2Y1,YPX2Y2
      REAL    YSTEP,YSTEPB,YSTEPL,YSTEPR,YSTEPT
      REAL    YTL,YTR,YVALS(9),YV,YVBL,YVBR,YVTL,YVTR
      REAL    ZTOTAL,ZVALS(9)
      DOUBLE  PRECISION  CF2D(6),MATINV(6,6)
C
C     Statement functions
C
      REAL XPOS,XPOSL,YPOS,YPOSL,ZV
C
C     XVALS and YVALS are the X and Y values for the 3 by 3 pixel
C     array used for surface fitting, and are constants.
C
      DATA XVALS/1.,2.,3.,1.,2.,3.,1.,2.,3./
      DATA YVALS/1.,1.,1.,2.,2.,2.,3.,3.,3./
C
      XPOS(XV)=FNX*(XV-XRESMIN)
      XPOSL(XV)=FNXL*(ALOG(XV)-ALXMIN)
      YPOS(YV)=FNY*(YV-YRESMIN)
      YPOSL(YV)=FNYL*(ALOG(YV)-ALYMIN)
      ZV(XV,YV)=XV*(CF2D(4)+XV*CF2D(1)+YV*CF2D(3))
     :           +YV*(CF2D(5)+YV*CF2D(2))+CF2D(6)
C
C     Start of executable code
C
C     Get the work space for GEN_POLY2D.
C
      STATUS=0
      CTSIZE=MAX(NCXX,NCYX)
      CALL DSA_GET_WORK_ARRAY(CTSIZE,'DOUBLE',CT,SLOT,STATUS)
C
C     Calculate constants for statement functions
C
      FNX=FLOAT(NXRES)/(XRESMAX-XRESMIN)
      IF (XLOG) THEN
         ALXMIN=ALOG(XRESMIN)
         FNXL=FLOAT(NXRES)/(ALOG(XRESMAX)-ALXMIN)
      END IF
      FNY=FLOAT(NYRES)/(YRESMAX-YRESMIN)
      IF (YLOG) THEN
         ALYMIN=ALOG(YRESMIN)
         FNYL=FLOAT(NYRES)/(ALOG(YRESMAX)-ALYMIN)
      END IF
C
C     And some other useful constants
C
      PXRESP1=NXRES+1
      PYRESP1=NYRES+1
      XSTEP=1./FLOAT(NDX)
      YSTEP=1./FLOAT(NDY)
      FACDXY=XSTEP*YSTEP
      NXP1=NX+1
      RND=.99999
C
C     Test for the linear shift condition
C
      IF ((NCXY.EQ.1).AND.(NCXX.EQ.2).AND.
     :    (NCYY.EQ.2).AND.(NCYX.EQ.1)) THEN
         LINEAR=((XCOEFF(1,1).EQ.1.).AND.(XCOEFF(1,2).EQ.XRESMIN).AND.
     :           (YCOEFF(1,1).EQ.1.).AND.(YCOEFF(2,1).EQ.YRESMIN))
      ELSE
         LINEAR=.FALSE.
      END IF
C
C     Zero out result array
C
      DO IY=1,NYRES
         DO IX=1,NXRES
            RESULT(IX,IY)=0.0
         END DO
      END DO
C
C     Initialise the picture x,y pixel location arrays
C     Note: The four loops used here look messy, but are faster
C     than a set of tests within one loop. The only differences
C     concern whether the logarithmic versions of the position
C     functions are used or not.  The PX values give the transformed
C     X values for the lowest valued corner of the pixel, ditto the
C     PY values.
C
      Y=0.
      IF (XLOG) THEN
         IF (YLOG) THEN
            DO IY=1,2
               X=0.
               DO IX=1,NXP1
                  PX(IX,IY)=
     :              XPOSL(GEN_POLY2D(X,Y,NCXX,NCXY,XCOEFF,
     :                    %VAL(CNF_PVAL(CT))))+XSHIFT
                  PY(IX,IY)=
     :              YPOSL(GEN_POLY2D(X,Y,NCYX,NCYY,YCOEFF,
     :                    %VAL(CNF_PVAL(CT))))+YSHIFT
                  X=X+1.
               END DO
               Y=Y+1.
            END DO
         ELSE
            DO IY=1,2
               X=0.
               DO IX=1,NXP1
                  PX(IX,IY)=
     :              XPOSL(GEN_POLY2D(X,Y,NCXX,NCXY,XCOEFF,
     :                    %VAL(CNF_PVAL(CT))))+XSHIFT
                  PY(IX,IY)=
     :              YPOS(GEN_POLY2D(X,Y,NCYX,NCYY,YCOEFF,
     :                   %VAL(CNF_PVAL(CT))))+YSHIFT
                  X=X+1.
               END DO
               Y=Y+1.
            END DO
         END IF
      ELSE
         IF (YLOG) THEN
            DO IY=1,2
               X=0.
               DO IX=1,NXP1
                  PX(IX,IY)=
     :               XPOS(GEN_POLY2D(X,Y,NCXX,NCXY,XCOEFF,
     :                    %VAL(CNF_PVAL(CT))))+XSHIFT
                  PY(IX,IY)=
     :               YPOSL(GEN_POLY2D(X,Y,NCYX,NCYY,YCOEFF,
     :                     %VAL(CNF_PVAL(CT))))+YSHIFT
                  X=X+1.
               END DO
               Y=Y+1.
            END DO
         ELSE
            IF (LINEAR) THEN
               Y=YSHIFT
               DO IY=1,2
                  X=XSHIFT
                  DO IX=1,NXP1
                     PX(IX,IY)=X
                     PY(IX,IY)=Y
                     X=X+1.
                  END DO
                  Y=Y+1.
               END DO
            ELSE
               DO IY=1,2
                  X=0.
                  DO IX=1,NXP1
                     PX(IX,IY)=
     :                 XPOS(GEN_POLY2D(X,Y,NCXX,NCXY,XCOEFF,
     :                 %VAL(CNF_PVAL(CT))))+XSHIFT
                     PY(IX,IY)=
     :                 YPOS(GEN_POLY2D(X,Y,NCYX,NCYY,YCOEFF,
     :                      %VAL(CNF_PVAL(CT))))+YSHIFT
                     X=X+1.
                  END DO
                  Y=Y+1.
               END DO
            END IF
         END IF
      END IF
C
C     Do the position functions increase or decrease with X and Y?
C
      XUP=XPOS(GEN_POLY2D(FLOAT(NX/2),FLOAT(NY/2),
     :                    NCXX,NCXY,XCOEFF,%VAL(CNF_PVAL(CT)))).LT.
     :    XPOS(GEN_POLY2D(FLOAT(NX/2+1),FLOAT(NY/2),
     :                    NCXX,NCXY,XCOEFF,%VAL(CNF_PVAL(CT))))
      YUP=YPOS(GEN_POLY2D(FLOAT(NX/2),FLOAT(NY/2),
     :                    NCYX,NCYY,YCOEFF,%VAL(CNF_PVAL(CT)))).LT.
     :    YPOS(GEN_POLY2D(FLOAT(NX/2),FLOAT(NY/2+1),
     :                    NCYX,NCYY,YCOEFF,%VAL(CNF_PVAL(CT))))
C
C     Do we assume that the data is flat?  Set 'matrix inverted'
C     flag ready for first call to SFIT2D.
C
      FLAT=MODE.EQ.0
      IF ((NDX.LE.1).AND.(NDY.LE.1)) FLAT=.TRUE.
      SAME=.FALSE.
C
C     L1,2 are the indices in PX,PY of the locations of the
C     present and next row position data.
C
      L1=1
      L2=2
C
C     Start main loop through rows of original data
C
      DO JY=1,NY
C
C        Loop through the pixels in the present data row
C
         DO JX=1,NX
C
C           Get data for pixel
C
            DVALUE=DATA(JX,JY)
C
C           Is data is to be interpolated by surface fitting?
C
            IF (.NOT.FLAT) THEN
C
C              Perform the initial fit
C
               KZ=1
               DO KY=JY-1,JY+1
                  KYOK=(KY.LE.NY).AND.(KY.GE.1)
                  DO KX=JX-1,JX+1
                     IF (KYOK.AND.(KX.LE.NX).AND.(KX.GE.1)) THEN
                        ZVALS(KZ)=DATA(KX,KY)
                     ELSE
                        ZVALS(KZ)=DATA(JX,JY)
                     END IF
                     KZ=KZ+1
                  END DO
               END DO
               CALL FIG_SFIT2D(XVALS,YVALS,ZVALS,9,SAME,MATINV,CF2D)
               SAME=.TRUE.
C
C              Now evaluate the interpolated Z-values at the centers
C              of each of the sub-pixels, and get the total.  Note
C              that for purposes of the surface fitting only, the
C              XV,YV values are both 1.5 at the lowest valued corner
C              of the center pixel.
C
               YPOSN=1.5+YSTEP*.5
               ZTOTAL=0.
               IZ=1
               DO IY=1,NDY
                  XPOSN=1.5+XSTEP*.5
                  DO IX=1,NDX
                     VALS(IX,IY)=ZV(XPOSN,YPOSN)*FACDXY
                     ZTOTAL=ZTOTAL+VALS(IX,IY)
                     IZ=IZ+1
                     XPOSN=XPOSN+XSTEP
                  END DO
                  YPOSN=YPOSN+YSTEP
               END DO
               EXCESS=(DVALUE-ZTOTAL)*FACDXY
            END IF
C
C           Work out the vertices of the destination polygon - we
C           don't yet make the rectangular assumption. Check the
C           destination lies within bounds.
C
            XBOUNDS=.FALSE.
            YBOUNDS=.FALSE.
            XPX1Y1=PX(JX,L1)
            XPX2Y1=PX(JX+1,L1)
            XPX1Y2=PX(JX,L2)
            XPX2Y2=PX(JX+1,L2)
            IF (XUP) THEN
               IF (((XPX1Y1.LE.PXRESP1).OR.(XPX1Y2.LE.PXRESP1)).AND.
     :             ((XPX2Y1.GE.0.).OR.(XPX2Y2.GE.0.))) XBOUNDS=.TRUE.
            ELSE
               IF (((XPX2Y1.LE.PXRESP1).OR.(XPX2Y2.LE.PXRESP1)).AND.
     :             ((XPX1Y1.GE.0.).OR.(XPX1Y2.GE.0.))) XBOUNDS=.TRUE.
            END IF
            YPX1Y1=PY(JX,L1)
            YPX2Y1=PY(JX+1,L1)
            YPX1Y2=PY(JX,L2)
            YPX2Y2=PY(JX+1,L2)
            IF (YUP) THEN
               IF (((YPX1Y1.LE.PYRESP1).OR.(YPX2Y1.LE.PYRESP1)).AND.
     :             ((YPX1Y2.GE.0.).OR.(YPX2Y2.GE.0.))) YBOUNDS=.TRUE.
            ELSE
               IF (((YPX1Y2.LE.PYRESP1).OR.(YPX2Y2.LE.PYRESP1)).AND.
     :             ((YPX1Y1.GE.0.).OR.(YPX2Y1.GE.0.))) YBOUNDS=.TRUE.
            END IF
C
C           Ignore pixel if out of bounds
C
            IF (XBOUNDS.AND.YBOUNDS) THEN
C
C              Loop through the sub pixels in Y and in X
C
               XSTEPL=(XPX1Y2-XPX1Y1)*YSTEP
               XSTEPR=(XPX2Y2-XPX2Y1)*YSTEP
               YSTEPL=(YPX1Y2-YPX1Y1)*YSTEP
               YSTEPR=(YPX2Y2-YPX2Y1)*YSTEP
               XVBL=XPX1Y1
               XVBR=XPX2Y1
               XVTL=XVBL+XSTEPL
               XVTR=XVBR+XSTEPR
               YVBL=YPX1Y1
               YVBR=YPX2Y1
               YVTL=YVBL+YSTEPL
               YVTR=YVBR+YSTEPR
               DO ISY=1,NDY
                  XSTEPB=(XVBR-XVBL)*XSTEP
                  XSTEPT=(XVTR-XVTL)*XSTEP
                  YSTEPB=(YVBR-YVBL)*XSTEP
                  YSTEPT=(YVTR-YVTL)*XSTEP
                  XBL=XVBL
                  XTL=XVTL
                  XTR=XVTL+XSTEPT
                  XBR=XVBL+XSTEPB
                  YBL=YVBL
                  YTL=YVTL
                  YTR=YVTL+YSTEPT
                  YBR=YVBL+YSTEPB
                  DO ISX=1,NDX
C
C                    Work out the vertices of the destination rectangle
C                    for this sub-pixel (this is where we make the
C                    rectangular and non-rotated assumptions).
C
                     IF (XUP) THEN
                        PX1=.5*(XBL+XTL)
                        PX2=.5*(XBR+XTR)
                     ELSE
                        PX1=.5*(XBR+XTR)
                        PX2=.5*(XBL+XTL)
                     END IF
                     IF (YUP) THEN
                        PY2=.5*(YTL+YTR)
                        PY1=.5*(YBL+YBR)
                     ELSE
                        PY2=.5*(YBL+YTR)
                        PY1=.5*(YTL+YTR)
                     END IF
C
C                    See how much flux we get for this sub-pixel
C
                     IF (FLAT) THEN
                        FLUX=DVALUE*FACDXY
                     ELSE
                        FLUX=VALS(ISX,ISY)+EXCESS
                     END IF
C
C                    Loop over the destination bins and give them their
C                    data.
C
                     DENSITY=FLUX/(PX2-PX1)/(PY2-PY1)
                     DO MY=IFIX(PY1+RND),IFIX(PY2+RND)
                        IF ((MY.GE.1).AND.(MY.LE.NYRES)) THEN
                           Y1=AMAX1(FLOAT(MY)-1.,PY1)
                           Y2=AMIN1(FLOAT(MY),PY2)
                           IF (Y2.GT.Y1) THEN
                              DO MX=IFIX(PX1+RND),IFIX(PX2+RND)
                                 IF ((MX.GE.1).AND.(MX.LE.NXRES)) THEN
                                    X1=AMAX1(FLOAT(MX)-1.,PX1)
                                    X2=AMIN1(FLOAT(MX),PX2)
                                    IF (X2.GT.X1) THEN
                                       RESULT(MX,MY)=RESULT(MX,MY)+
     :                                         DENSITY*(X2-X1)*(Y2-Y1)
                                    END IF
                                END IF
                              END DO
                           END IF
                        END IF
                     END DO
C
C                    Calculate corner values for next sub-pixel
C
                     XBL=XBR
                     XBR=XBR+XSTEPB
                     XTL=XTR
                     XTR=XTR+XSTEPT
                     YBL=YBR
                     YBR=YBR+YSTEPB
                     YTL=YTR
                     YTR=YTR+YSTEPT
                  END DO
C
C                 Calculate edge values for next row of sub-pixels
C
                  XVBL=XVTL
                  XVTL=XVTL+XSTEPL
                  XVBR=XVTR
                  XVTR=XVTR+XSTEPR
                  YVBL=YVTL
                  YVTL=YVTL+YSTEPL
                  YVBR=YVTR
                  YVTR=YVTR+YSTEPR
               END DO
C
C              End test for pixel within bounds of result array
C
            END IF
C
C           End loop through pixels in current row
C
         END DO
C
C        Get positions for next row of data
C
         LT=L1
         L1=L2
         L2=LT
         Y=FLOAT(JY+1)
         X=0.
         IF (XLOG) THEN
            IF (YLOG) THEN
               DO IX=1,NXP1
                  PX(IX,L2)=
     :                 XPOSL(GEN_POLY2D(X,Y,NCXX,NCXY,XCOEFF,
     :                      %VAL(CNF_PVAL(CT))))+XSHIFT
                  PY(IX,L2)=
     :                 YPOSL(GEN_POLY2D(X,Y,NCYX,NCYY,YCOEFF,
     :                      %VAL(CNF_PVAL(CT))))+YSHIFT
                  X=X+1.
               END DO
            ELSE
               DO IX=1,NXP1
                  PX(IX,L2)=
     :                 XPOSL(GEN_POLY2D(X,Y,NCXX,NCXY,XCOEFF,
     :                      %VAL(CNF_PVAL(CT))))+XSHIFT
                  PY(IX,L2)=
     :                 YPOS(GEN_POLY2D(X,Y,NCYX,NCYY,YCOEFF,
     :                      %VAL(CNF_PVAL(CT))))+YSHIFT
                  X=X+1.
               END DO
            END IF
         ELSE
            IF (YLOG) THEN
               DO IX=1,NXP1
                  PX(IX,L2)=
     :                 XPOS(GEN_POLY2D(X,Y,NCXX,NCXY,XCOEFF,
     :                      %VAL(CNF_PVAL(CT))))+XSHIFT
                  PY(IX,L2)=
     :                 YPOSL(GEN_POLY2D(X,Y,NCYX,NCYY,YCOEFF,
     :                      %VAL(CNF_PVAL(CT))))+YSHIFT
                  X=X+1.
               END DO
            ELSE
               IF (LINEAR) THEN
                  DO IX=1,NXP1
                     PY(IX,L2)=Y+YSHIFT
                  END DO
               ELSE
                  DO IX=1,NXP1
                     PX(IX,L2)=
     :                 XPOS(GEN_POLY2D(X,Y,NCXX,NCXY,XCOEFF,
     :                      %VAL(CNF_PVAL(CT))))+XSHIFT
                     PY(IX,L2)=
     :                 YPOS(GEN_POLY2D(X,Y,NCYX,NCYY,YCOEFF,
     :                      %VAL(CNF_PVAL(CT))))+YSHIFT
                     X=X+1.
                  END DO
               END IF
            END IF
         END IF
C
C        End loop through rows
C
      END DO
C
C     Release the work space
C
      CALL DSA_FREE_WORKSPACE(SLOT,STATUS)
C
C     END of program
C
C     ----------------------------------------------------------------
C
C     APPENDIX A - Nomenclature for the calculation of the transformed
C                  coordinates of the corners of the sub-pixels.
C
C     The transformed coordinates of each corner of the pixel itself
C     are calculated directly by polynomial evaluation, giving the
C     values PX1Y1,PX2Y1,PX1Y2,PX2Y2.  The diagram shows the pixel.
C
C           PX1Y2                             PX2Y2
C             -----------------------------------
C             |                                 |
C         --  | - - - - - - - - - - - - - - - - |  --
C       STEPL |           -> STEPT <-           | STEPR
C         --  | - - - - - - - - - - - - - - - - |  --
C             |            TL      TR           |
C       VTL ->| - - - - - - -------- - - - - - -|<- VTR
C             |             |      |            |
C       VBL ->| - - - - - - -------- - - - - - -|<- VBR
C             |            BL      BR           |
C             |---------------------------------|
C           PX1Y1          -> STEPB <-        PX2Y1
C
C     TL & TR are obtained by linear interpolation between VTL,VTR
C     BL & BR  "      "    "     "        "           "    VBL,VBR
C     VTL,VBL  "      "    "     "        "           "    PX1Y2,PX1Y1
C     VTR,VBR  "      "    "     "        "           "    PX2Y2,PX2Y1
C
C     These calculations are performed twice, once for the X values,
C     once for the Y, the variables in the program being given by the
C     names used above prefixed by X or Y, eg XVTL,XVTR.
C
C     ------------------------------------------------------------------
C
C     APPENDIX B - A little bit more detail about the algorithm
C
C     This is quite a complex program, so some more explanation may be
C     in order.
C
C     In general, each pixel of the input array maps onto the result
C     array as a quadrilateral oriented at random and with no
C     requirement that the sides be parallel.  The actual distribution
C     of data over the original pixel is essentially unknown - only the
C     total is known (being the contents of the pixel), and there may be
C     some help to be gained by looking at the contents of the
C     surrounding pixels.
C
C     Ideally, the calculation would consist of looking at all the
C     result pixels covered by a given input pixel, calculating the
C     volume of the input data (the body with the quadrilateral base and
C     top surface determined by the data distribution over the pixel)
C     covering each result pixel, and adding that number to the result
C     pixel.  This calculation is NOT atempted by this program.
C
C     In the simplest (and fastest!) case, with NDX and NDY both 1,
C     the program assumes that each input pixel maps onto the result
C     array as a non-rotated rectangle.  The X and Y positions of the
C     sides are taken as the average of the X and Y positions of the
C     nearest vertices.  The data distribution is assumed to be flat.
C     The data of the input pixel is distributed to the result pixels
C     it covers in proportion to the fraction of the input pixel that
C     covers each result pixel.
C
C     If NDX and/or NDY are greater than 1, the calculation is basically
C     the same, except that the input pixel is sub-divided into NDX by
C     NDY sub-pixels first.  It is then the sub-pixels that are
C     assumed to map in a rectangular and non-rotated way into the
C     result array.  As NDX and NDY increase so do a) the CPU time
C     used, and b) the goodness of the approximation of the set of
C     non-rotated rectangles to the actual general quadrilateral.  If
C     MODE is 0, the surface of the data is assumed to be flat over the
C     pixel and each of the sub-pixels is simply given 1/(NDX*NDY) of
C     the data in the input pixel.
C
C     If MODE is 1, a 2D parabola is fitted
C     to the local 9 input pixels and the value of the parabola at the
C     center of each sub-pixel, divided by (NDX*NDY), is taken to be
C     the data in that sub-pixel.  Since this will generally not
C     conserve flux (ie the sum of the data given to each sub-pixel will
C     not exactly equal the original data in the large pixel) the excess
C     is calculated and distributed equally between the sub-pixels.
C     This may seem a rather crude approximation, but it should be good
C     except in cases where the data fluctuates rapidly between adjacent
C     pixels; however, if this is the case the data is quite simply
C     badly under-sampled and there is no way of knowing what the
C     correct distribution over the original pixel was, so you shouldn't
C     complain too much so long as the program makes a plausible guess.
C
      END
