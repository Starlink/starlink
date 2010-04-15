
*+  KFH_PSCALE - Scales patch ready for output.
      SUBROUTINE KFH_PSCALE(PATCH,PXDIM,PYDIM,SCRTCH,XDIM,
     : YDIM,A,AXDIM,AYDIM,XLO,YLO,IMLO,IMHI)
*    Type Definitions :
      IMPLICIT NONE
*    Local variables :
      INTEGER PXDIM

      INTEGER PYDIM
      INTEGER AXDIM
      INTEGER AYDIM
      REAL A(512,512)
      INTEGER PATCH(PXDIM,PYDIM)
      INTEGER XDIM
      INTEGER YDIM
      INTEGER SCRTCH(XDIM,YDIM)
      INTEGER I
      REAL IMHI
      REAL IMLO
      INTEGER J
      INTEGER XLO
      INTEGER YLO
*-

*
*    Load patch with original data.
*


      DO I = 1,PYDIM

         DO J = 1,PXDIM

            PATCH(J,I) = SCRTCH(J+XLO-1,YDIM-YLO-PYDIM+I+1)

         END DO

      END DO


*
*    Load patch with calculated data.
*


      DO J = 1,PYDIM

         DO I = 1,PXDIM

            PATCH(I,PYDIM+1-J) =
     :       MIN(MAX(NINT((A(I+1,J+1)-IMLO)/(IMHI-IMLO)*255.0),0),255)

         END DO

      END DO


      END
