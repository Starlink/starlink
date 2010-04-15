C+
      SUBROUTINE FIG_CPCENT(IMAGE,NX,NY,IX,IY,NRAD,SUMX,SUMY,
     :                                              X,Y,STATUS)
C
C     F I G _ C P C E N T
C
C     Simple unweighted a la mountain photometry code
C     (for further details see 'Stellar Magnitudes from Digital
C     Pictures', KPNO Dec 1980, by Adams,Christian,Mould,Stryker
C     and Tody.)
C
C     Determines the centroid of an object.  This routine
C     is essentially the same as JRM's CENTROID routine, with
C     only cosmetic changes to make it fit in with the Figaro
C     CPOS routine. Mainly, the whole data array is now passed
C     as an argument, rather than having a subset in common.
C
C     Parameters -   (">" input, "W" workspace, "<" output)
C
C     (>) IMAGE    (Real array IMAGE(NX,NY)) The image data.
C     (>) NX       (Integer) The number of x-pixels in IMAGE.
C     (>) NY       (Integer) The number of y-pixels in IMAGE.
C     (>) IX       (Integer) The estimated coordinates of the
C     (>) IY       (Integer) center pixel of the object.
C     (>) NRAD     (Integer) The aperture size to be used for
C                  centroiding - a number of pixels.
C     (W) SUMX     (Real array SUMX(2*NRAD+1)) Workspace.
C     (W) SUMY     (Real array SUMY(2*NRAD+1)) Workspace.
C     (<) X        (Real) The coordinates of the centroid of
C     (<) Y        (Real) the object.
C     (<) STATUS   (Integer) Returns a status code.
C                  0 => OK.
C                  -1 => Centroid sums were not positive.
C                  -2 => Centroid failed to converge.
C                  -3 => Point is too close to the edge of the data.
C
C                                     KS / CIT 7th May 1983
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER NX,NY,NRAD,IX,IY,STATUS
      REAL IMAGE(NX,NY),SUMX(2*NRAD+1),SUMY(2*NRAD+1),X,Y
C
C     Local variables
C
      INTEGER I,J,K,L,NUMX,NUMY,OX,OXST,OY,OYST
      REAL OLDX,OLDY,VALUE,XMEAN,YMEAN,Z,ZX,ZY
C
C     Loop through iterations
C
      X=FLOAT(IX)
      Y=FLOAT(IY)
      L=NRAD*2+1
      DO K=1,5
         OLDX=X
         OLDY=Y
         OX=OLDX+.5
         OY=OLDY+.5
         OXST=OX-NRAD-1
         OYST=OY-NRAD-1
C
C        Test for edge
C
         IF (((OX+NRAD).GT.NX).OR.((OX-NRAD).LT.1).OR.
     :       ((OY+NRAD).GT.NY).OR.((OY-NRAD).LT.1)) THEN
            STATUS=-3
            RETURN
         END IF
C
C        Marginals
C
         DO I = 1,L
            SUMX(I)=0.
            SUMY(I)=0.
         END DO
         DO I=1,L
            DO J=1,L
               VALUE=IMAGE(OXST+I,OYST+J)
               SUMX(I)=SUMX(I)+VALUE
               SUMY(J)=SUMY(J)+VALUE
            END DO
         END DO
C
C        Means
C
         Z=L
         XMEAN=0
         YMEAN=0
         DO I=1,L
            XMEAN=XMEAN+SUMX(I)/Z
            YMEAN=YMEAN+SUMY(I)/Z
         END DO
         ZX=0
         ZY=0
         X=0
         Y=0
C
C        Centroids
C
         NUMX=0
         NUMY=0
         DO I=1,L
            IF(SUMX(I).GT.XMEAN) THEN
               X=X+(SUMX(I)-XMEAN)*FLOAT(OXST+I)
               NUMX=NUMX+1
               ZX=ZX+SUMX(I)-XMEAN
            END IF
            IF(SUMY(I).GT.YMEAN) THEN
               Y=Y+(SUMY(I)-YMEAN)*FLOAT(OYST+I)
               NUMY=NUMY+1
               ZY=ZY+SUMY(I)-YMEAN
            END IF
         END DO
         IF(ZX.LE.0.OR.ZY.LE.0.)THEN
            STATUS=-1
            RETURN
         END IF
         X=X/ZX
         Y=Y/ZY
         IF((OLDX-X)**2+(OLDY-Y)**2.LT.1.) THEN
            STATUS=0
            RETURN
         END IF
      END DO
      STATUS=-2
      RETURN
      END
