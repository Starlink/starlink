C+
      SUBROUTINE FIG_YTRACT(IMAGE,NX,NY,IXST,IXEN,SPECT)
C
C     F I G _ Y T R A C T
C
C     Adds together consecutive columns of an image to form
C     a 1-dimensional spectrum.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) IMAGE     (Real array, IMAGE(NX,NY)) The image.
C     (>) NX        (Integer) The x-dimension of the image.
C     (>) NY        (Integer) The y-dimension of the image.
C     (>) IXST      (Integer) The number of the first of the
C                   columns to be added.
C     (>) IXEN      (Integer) The number of the last of the
C                   columns to be added.
C     (<) SPECT     (Real array SPECT(NY)) The resulting spectrum.
C
C     Subroutines / functions used -  Only standard Fortran.
C
C                                    KS / CIT 6th April 1983
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER NX,NY,IXST,IXEN
      REAL IMAGE(NX,NY),SPECT(NY)
C
C     Local variables
C
      INTEGER I1,I2,I,J
C
C     Perform extraction
C
      I1=MAX(IXST,1)
      I2=MIN(IXEN,NX)
      DO I=1,NY
         SPECT(I)=0
         DO J=I1,I2
            SPECT(I)=SPECT(I)+IMAGE(J,I)
         END DO
      END DO
C
      END
