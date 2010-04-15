C+
      SUBROUTINE GEN_GCONV (IN,NX,SIGMA,WIDTH,WORK,OUT)
C
C     G E N _ G C O N V
C
C     Convolves a 1D array with a gaussian.  The caller has to
C     supply the standard deviation and the number of elements
C     over which the gaussian is to be calculated (the width).
C     Each point of the input array becomes, in the output array,
C     the convolution of a the input array with a gaussian of
C     s.d. SIGMA, centered on the original point and cut-off after
C     (WIDTH-1)/2 elements on either side.
C
C     Parameters -   (">" input, "W" workspace, "<" output)
C
C     (>) IN      (Real array IN(NX)) The input data.
C     (>) NX      (Integer) The dimension of the input array.
C     (>) SIGMA   (Real) The standard deviation of the gaussian
C                 (as a number of elements).
C     (>) WIDTH   (Integer) The number of elements over which the
C                 gaussian is to be calculated.  This should be
C                 an odd number, ideally (if even, the effect will
C                 be the same as for WIDTH-1).
C     (W) WORK    (Real array WORK(WIDTH) Workspace.
C     (<) OUT     (Real array OUT(NX)).  The result array.  Note
C                 that OUT must NOT be the same array as IN.
C
C     Subroutines / functions used -  None
C
C     Common variables used - None
C                                           KS / CIT 22nd March 1983
C
C     Modifications:
C
C     22nd Mar 1983  KS / CIT.  Original version.
C     23rd Sep 1992  HME / UoE, Starlink.  The inner loop through the
C                    data used to go till J=WIDTH, when it should only
C                    go till J=IWIDTH. Thus an uninitialised WORK(J)
C                    would be used if WIDTH was even.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,WIDTH
      REAL IN(NX),SIGMA,WORK(WIDTH),OUT(NX)
C
C     Local variables
C
      INTEGER I,ICENT,IWD,IWIDTH,J,K
      REAL FAC,RES,SS,X
C
C     Fill work array with the values of the gaussian.
C
      SS=SIGMA*SIGMA*2
      IWD=(WIDTH-1)/2
      IWIDTH=IWD+IWD+1
      ICENT=IWD+1
      DO J=1,IWD
         X=EXP(-FLOAT(J*J)/SS)
         WORK(ICENT-J)=X
         WORK(ICENT+J)=X
      END DO
      WORK(ICENT)=1.
C
C     Loop through the data
C
      DO I=1,NX
         FAC=0.
         RES=0.
         K=I-IWD
         DO J=1,IWIDTH
            IF ((K.GE.1).AND.(K.LE.NX)) THEN
               RES=RES+WORK(J)*IN(K)
               FAC=FAC+WORK(J)
            END IF
            K=K+1
         END DO
         OUT(I)=RES/FAC
      END DO
C
      END
