C+
      SUBROUTINE FIG_XTRACT(IMAGE,NX,NY,IYST,IYEN,SPECT)
C
C     F I G _ X T R A C T
C
C     Adds together consecutive rows of an image to form
C     a 1-dimensional spectrum.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) IMAGE     (Real array, IMAGE(NX,NY)) The image.
C     (>) NX        (Integer) The x-dimension of the image.
C     (>) NY        (Integer) The y-dimension of the image.
C     (>) IYST      (Integer) The number of the first of the
C                   rows to be added.
C     (>) IYEN      (Integer) The number of the last of the
C                   rows to be added.
C     (<) SPECT     (Real array SPECT(NX)) The resulting spectrum.
C
C     Subroutines / functions used -  Only standard Fortran.
C
C                                    KS / CIT 17th March 1983
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER NX,NY,IYST,IYEN
      REAL IMAGE(NX,NY),SPECT(NX)
C
C     Local variables
C
      INTEGER I1,I2,I,J
C
C     Perform extraction
C
      I1=MAX(IYST,1)
      I2=MIN(IYEN,NY)
      DO I=1,NX
         SPECT(I)=IMAGE(I,I1)
      END DO
      IF (I2.GT.I1) THEN
         DO J=I1+1,I2
            DO I=1,NX
               SPECT(I)=SPECT(I)+IMAGE(I,J)
            END DO
         END DO
      END IF
C
      END
