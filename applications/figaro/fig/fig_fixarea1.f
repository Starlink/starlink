C+
      SUBROUTINE FIG_FIXAREA1 (DATA,NX,NY,COORD,NCOEFF,FLAG,DIRECTION)
C
C     F I G _ F I X A R E A
C
C     Interpolates over a small punched out area in an image.
C     The fix is attempted twice, once using vertical interpolation
C     and once using horizontal interpolation.  The criteria for
C     choosing one over the other is based on a comparison of the
C     average difference in data over the boundaries of the fixed
C     area, compared with the average difference in adjacent unfixed
C     pixels.  The direction of fixing that gives the lower relative
C     difference is the one used.
C
C     Parameters -  (">" input, "!" modified)
C
C     (!) DATA     (Real array DATA(NX,NY)) The image data.
C     (>) NX       (Integer) The horizontal dimension of DATA
C     (>) NY       (Integer) The vertical dimension of DATA
C     (>) COORD    (Integer array COORD(4)) The coordinates of the
C                  area to be fixed; Low x-coord, Low y-coord, High
C                  x-ccord, High y-coord, in that order.
C     (>) NCOEFF   (Integer) Number of coordinates to be used for
C                  the interpolative fits.  Must be <= 8.
C     (>) FLAG     (Real) The value used to flag invalid pixels.
C     (>) DIRECTION(Integer) The direction along which interpolation will be done.
C
C     Common variables used - None
C
C     Functions / subroutines used -
C
C     FIG_HORIZONTAL  (FIG_ package) Fix pixels by horiz. interpolation.
C     FIG_VERITCAL    (  "     "   )  "     "   "  vert.      "
C
C     This routine is based on an original routine by John Tonry
C                                             KS / CIT 26th Feb 1984
C     SJM/MSSSO 17 July 1993. Added the user the option of which
C     direction in which to do the interpolation.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,COORD(4),NCOEFF,DIRECTION
      REAL    DATA(NX,NY),FLAG
C
C     Maximum number of points for each fit
C
      INTEGER MAXPT
      PARAMETER (MAXPT=35)
C
C     Local variables
C
      INTEGER IX,IX1,IX2,IY,IY1,IY2,LIMX1,LIMX2,LIMY1,LIMY2
      INTEGER NCH,NCV,NDH,NDV,STATUS
      REAL    CH,CV,DATUM,DH,DV,X(MAXPT),Y(MAXPT),Z(MAXPT)
C
C     Get boundaries
C
      IX1=COORD(1)
      IY1=COORD(2)
      IX2=COORD(3)
      IY2=COORD(4)
C
C     Which interpolation scheme is going to be used?
C
      IF (DIRECTION .EQ. 1) THEN	!Vertical!
        LIMY1=IY1-2*NCOEFF
        LIMY2=IY2+2*NCOEFF
        DO IX=IX1,IX2
           CALL FIG_VERTICAL(DATA,NX,NY,IX,IY1,IY2,LIMY1,LIMY2,
     :                              NCOEFF,FLAG,MAXPT,Y,Z,STATUS)
        END DO
      ELSE IF (DIRECTION .EQ. -1) THEN	!Horizontal!
        LIMX1=IX1-2*NCOEFF
        LIMX2=IX2+2*NCOEFF
        DO IY=IY1,IY2
           CALL FIG_HORIZONTAL(DATA,NX,NY,IY,IX1,IX2,LIMX1,LIMX2,
     :                                NCOEFF,FLAG,MAXPT,X,Z,STATUS)
        END DO
      ELSE	!Let the computer decide which to use.
C
C     First, try a vertical fit
C
      LIMY1=IY1-2*NCOEFF
      LIMY2=IY2+2*NCOEFF
      DO IX=IX1,IX2
         CALL FIG_VERTICAL(DATA,NX,NY,IX,IY1,IY2,LIMY1,LIMY2,
     :                            NCOEFF,FLAG,MAXPT,Y,Z,STATUS)
      END DO
C
C     Evaluate average difference between fit and adjoining data
C
      NDV=0
      DV=0.
      NCV=0
      CV=0.
      DO IY=IY1,IY2
         IF ((IX1-1).GE.1) THEN
            DATUM=DATA(IX1-1,IY)
            IF (DATUM.NE.FLAG) THEN
               DV=DV+ABS(DATA(IX1,IY)-DATUM)
               NDV=NDV+1
               IF ((IX1-2).GE.1) THEN
                  IF (DATA(IX1-2,IY).NE.FLAG) THEN
                     CV=CV+ABS(DATA(IX1-2,IY)-DATUM)
                     NCV=NCV+1
                  END IF
               END IF
            END IF
         END IF
         IF ((IX2+1).LE.NX) THEN
            DATUM=DATA(IX2+1,IY)
            IF (DATUM.NE.FLAG) THEN
               DV=DV+ABS(DATA(IX2,IY)-DATUM)
               NDV=NDV+1
               IF ((IX2+2).GE.NX) THEN
                  IF (DATA(IX2+2,IY).NE.FLAG) THEN
                     CV=CV+ABS(DATA(IX2+2,IY)-DATUM)
                     NCV=NCV+1
                  END IF
               END IF
            END IF
         END IF
      END DO
C
      IF (NDV.GT.0) THEN
         DV=DV/NDV
      ELSE
         DV=100.
      END IF
      IF ((NCV.GT.0).AND.(CV.GT.0.)) THEN
         CV=CV/NCV
      ELSE
         CV=1.
      END IF
C
C     Now try for a horizontal fit.
C
      LIMX1=IX1-2*NCOEFF
      LIMX2=IX2+2*NCOEFF
      DO IY=IY1,IY2
         CALL FIG_HORIZONTAL(DATA,NX,NY,IY,IX1,IX2,LIMX1,LIMX2,
     :                                NCOEFF,FLAG,MAXPT,X,Z,STATUS)
      END DO
      NDH=0
      DH=0.
      NCH=0
      CH=0.
      DO IX=IX1,IX2
         IF ((IY1-1).GE.1) THEN
            DATUM=DATA(IX,IY1-1)
            IF (DATUM.NE.FLAG) THEN
               DH=DH+ABS(DATA(IX,IY1)-DATUM)
               NDH=NDH+1
               IF ((IY1-2).GE.1) THEN
                  IF (DATA(IX,IY1-2).NE.FLAG) THEN
                     CH=CH+ABS(DATA(IX,IY1-2)-DATUM)
                     NCH=NCH+1
                  END IF
               END IF
            END IF
         END IF
         IF ((IY2+1).LE.NY) THEN
            DATUM=DATA(IX,IY2+1)
            IF (DATUM.NE.FLAG) THEN
               DH=DH+ABS(DATA(IX,IY2)-DATUM)
               NDH=NDH+1
               IF ((IY2+2).LE.NY) THEN
                  IF (DATA(IX,IY2+2).NE.FLAG) THEN
                     CH=CH+ABS(DATA(IX,IY2+2)-DATUM)
                     NCH=NCH+1
                  END IF
               END IF
            END IF
         END IF
      END DO
      IF (NDH.GT.0) THEN
         DH=DH/NDH
      ELSE
         DH=100.
      END IF
      IF ((NCH.GT.0).AND.(CH.GT.0.)) THEN
         CH=CH/NCH
      ELSE
         CH=1.
      END IF
C
C     Now, choose between them, and repeat fix if vertical looks best.
C
      IF (ABS(DV/CV).LT.ABS(DH/CH)) THEN
         DO IX=IX1,IX2
            CALL FIG_VERTICAL(DATA,NX,NY,IX,IY1,IY2,LIMY1,LIMY2,
     :                               NCOEFF,FLAG,MAXPT,Y,Z,STATUS)
         END DO
      END IF
      ENDIF	!end of DIRECTION if.
C
      END
