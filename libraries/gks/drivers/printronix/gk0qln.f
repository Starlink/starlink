


      SUBROUTINE GK0QLN(N,X,Y)
*
*--------------------------------------------------------------------------
*
*  RUTHERFORD / ICL GKS SYSTEM
*
*  Type of routine:  (Part of) workstation driver
*
*  Author:           PTW
*
      INCLUDE '../../include/check.inc'
*
*  PURPOSE OF THE ROUTINE
*  ----------------------
*     Plot polyline
*
*
*  ARGUMENTS
*  ---------
*     INP N      - Number of points
*     INP X,Y    - Coordinates of points
*
      INTEGER N
      REAL X(N),Y(N)
*
*  COMMON BLOCK USAGE
*  ------------------
      INCLUDE '../../include/GKS_PAR'
      INCLUDE '../../include/gkdt.par'
      INCLUDE '../../include/gkerr.cmn'
      INCLUDE '../../include/gkwkd.cmn'
      INCLUDE '../../include/gkwca.cmn'
      INCLUDE '../../include/gkwsl.cmn'

*
*     Intrinsic functions declaration
*
      INTRINSIC ISIGN,NINT,REAL,ABS
*
*  LOCALS
*  ------
*
*     Offsets in KWKDAT
*
      INTEGER NVIS
      PARAMETER (NVIS=4)
      INTEGER NWSTYP,LPORT
      PARAMETER (NWSTYP=6,LPORT=0)
*
      INTEGER I, IXY, I1, I2, INC, IX, IY, LVIS
      REAL X1, Y1, X2, Y2, DX, DY, ADX, ADY, RG1, XY1, SLOPE
*
*
*  ALGORITHM
*  ---------
*     Simple DDA (see Newman & Sproull p 20), but coded to minimise
*     rounding errors.  Two time optimizations are employed: (a) lines
*     which are parallel to the X-axis are plotted in bytes rather
*     than bits, and (b) the duplicate inner loops for the different
*     combinations of tilt and set/clear are used to minimise the
*     code within each loop.  Care has been taken to minimise
*     rounding errors in spite of a small time penalty.
*
*
*--------------------------------------------------------------------

*
*     Initialise locals. For PORTrait orientation this is when
*     "rotation" takes place: X coordinates change the sign
*     and become Y coordinates.
*
      IF(KWKDAT(NWSTYP,KWKIX).EQ.LPORT)THEN
         X2 = Y(1)
         Y2 = ABS(X(1))
      ELSE
         X2 = X(1)
         Y2 = Y(1)
      ENDIF
*
*     LOOP - line by line along polyline
*
*     Indicator of line visibility
      LVIS = KWKDAT(NVIS,KWKIX)
*
      DO 100 I = 2,N

*        Distances in X and Y

         X1 = X2
         Y1 = Y2
         IF(KWKDAT(NWSTYP,KWKIX).EQ.LPORT)THEN
            X2 = Y(I)
            Y2 = ABS(X(I))
         ELSE
            X2 = X(I)
            Y2 = Y(I)
         ENDIF
*
         DX = X2 - X1
         DY = Y2 - Y1

*        If line is parallel to X axis treat as special case
         ADY = ABS(DY)
         IF (ADY.LT.0.5) THEN
            CALL GK0QXL(X1,Y1,X2,LVIS)
         ELSE

*           Line not parallel to X axis
            ADX = ABS(DX)

*           Plot lines which are nearer horizontal than vertical
            IF (ADX.GT.ADY) THEN

*              Grid X coordinates of ends of line
               RG1 = NINT(X1)
               I1  = RG1
               I2 = NINT(X2)

*              Direction:  -1 = leftwards, +1 = rightwards
               INC = ISIGN(1,NINT(DX))

*              Slope
               SLOPE = DY/DX

*              Y coordinate at start X grid point
               XY1 = Y1 - (X1-RG1)*SLOPE

*
*                 LOOPS - point by point along line, clear or set
*
                  DO 200 IXY = I1,I2,INC

*                    Dot position
                     IX = IXY
                     IY = NINT(XY1+REAL(IXY-I1)*SLOPE)

                     IF(LVIS.EQ.0)THEN
                        CALL GK0QCB(IX,IY)
                     ELSE
                        CALL GK0QSB(IX,IY)
                     ENDIF
*                 Next bit along line
  200             CONTINUE

*           Plot lines which are nearer vertical than horizontal
            ELSE

*              Grid Y coordinates of ends of line
               RG1 = NINT(Y1)
               I1  = RG1
               I2 = NINT(Y2)

*              Direction:  -1 = down, +1 = up
               INC = ISIGN(1,NINT(DY))

*              Slope
               SLOPE = DX/DY

*              X coordinate at start Y grid point
               XY1 = X1 - (Y1-RG1)*SLOPE

*
*                 LOOPS - point by point along line, clear or set
*
                  DO 300 IXY = I1,I2,INC

*                    Dot position
                     IX = NINT(XY1+REAL(IXY-I1)*SLOPE)
                     IY = IXY

                     IF(LVIS.EQ.0)THEN
                        CALL GK0QCB(IX,IY)
                     ELSE
                        CALL GK0QSB(IX,IY)
                     ENDIF
  300             CONTINUE


            END IF

         END IF

*     Next line within polyline
  100 CONTINUE

      END
