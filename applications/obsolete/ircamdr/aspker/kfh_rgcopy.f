
*+  KFH_RGCOPY - Makes a copy of the patch concerned.
      SUBROUTINE KFH_RGCOPY(IMAGE,A,XDIM,YDIM,DIMX,DIMY,
     : XLO,XHI,YLO,YHI)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER DIMX
      INTEGER DIMY
      INTEGER XDIM
      INTEGER YDIM
      REAL A(512,512)
      INTEGER I
      INTEGER I1
      REAL IMAGE(XDIM,YDIM)
      INTEGER J
      INTEGER J1
      INTEGER XHI
      INTEGER XLO
      INTEGER YHI
      INTEGER YLO
*-

*
*    Copy the region into an array A.
*

      J1 = 1

      DO J = YLO-1,YHI+1

         I1 = 1

         DO I = XLO-1,XHI+1

            A(I1,J1) = IMAGE(I,J)
            I1 = I1+1

         END DO

         J1 = J1+1

      END DO

      END
