*+  KFH_ARRCRE - Creates an array of given dimensions.
      SUBROUTINE KFH_ARRCRE(XDIM,YDIM,B,A)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      INTEGER XDIM
      INTEGER YDIM
      REAL A(512,512)
      REAL B(512,512)
      INTEGER I
      INTEGER J
*-

*
*    Initialise array.
*

      DO J = 1,YDIM

         DO I = 1,XDIM

            B(I,J) = A(I,J)

         END DO

      END DO

      END
