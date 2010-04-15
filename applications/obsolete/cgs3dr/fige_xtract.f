C+
      SUBROUTINE FIGE_XTRACT(IMAGE,IMAGE_VARIANCE,IMAGE_QUALITY,
     :   NX,NY,IYST,IYEN,VARIANCE,QUALITY,FLAGGED,FBAD,SPECT,
     :   SPECT_VARIANCE,SPECT_QUALITY,NUMBER,SUM,SUMSQ)
C
C     F I G E _ X T R A C T
C
C     Averages consecutive rows of an image to form a 1-dimensional spectrum,
C     and calculates the error on the mean.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) IMAGE            (Real array, IMAGE(NX,NY)) The image.
C     (>) IMAGE_VARIANCE   (Real array (NX,NY)), variance of image points.
C     (>) IMAGE_QUALITY    (Byte array (NX,NY)), quality of image points.
C     (>) NX               (Integer) The x-dimension of the image.
C     (>) NY               (Integer) The y-dimension of the image.
C     (>) IYST             (Integer) The number of the first of the
C                          rows to be added.
C     (>) IYEN             (Integer) The number of the last of the
C                          rows to be added.
C     (>) VARIANCE         (Logical) TRUE if IMAGE has error array.
C     (>) QUALITY          (Logical) TRUE if IMAGE has quality array
C     (>) FLAGGED          (Logical) TRUE if IMAGE has flagged bad values.
C     (>) FBAD             (Real) The value of the bad data flag.
C     (<) SPECT            (Real array SPECT(NX)) The resulting spectrum.
C     (<) SPECT_VARIANCE   (Real array (NX)) The variance on the spectrum.
C     (<) SPECT_QUALITY    (Byte array (NX)) The quality of the spectrum.
C     (<) NUMBER           (Integer array (NX)) Work array.
C     (<) SUM              (Real array (NX)) Work array.
C     (<) SUMSQ            (Real array (NX)) Work array.
C
C     Subroutines / functions used -  Only standard Fortran.
C
C                                            KS / CIT 17th March 1983
C     Modified to handle errors and quality JFL / JACH 20th July 1990
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER NX,NY,IYST,IYEN,NUMBER(NX)
      REAL IMAGE(NX,NY),IMAGE_VARIANCE(NX,NY),SPECT(NX),
     :   SPECT_VARIANCE(NX),SUM(NX),SUMSQ(NX),FBAD
      LOGICAL VARIANCE,QUALITY,FLAGGED
      BYTE IMAGE_QUALITY(NX,NY),SPECT_QUALITY(NX)
C
C     Local variables
C
      INTEGER I1,I2,I,J,GOOD,BAD
      PARAMETER (GOOD=0,BAD=1)
C
C     Perform extraction
C
      I1=MAX(IYST,1)
      I2=MIN(IYEN,NY)

      IF (I1 .EQ. I2) THEN

         DO I=1,NX
            SPECT(I)=IMAGE(I,I1)
         END DO

         IF (VARIANCE) THEN
            DO I=1,NX
               SPECT_VARIANCE(I)=IMAGE_VARIANCE(I,I1)
            END DO
         ELSE
            DO I=1,NX
               SPECT_VARIANCE(I)=0.0
            END DO
         END IF

         IF (QUALITY) THEN
            DO I=1,NX
               SPECT_QUALITY(I)=IMAGE_QUALITY(I,I1)
            END DO
         END IF

      ELSE IF (I2 .GT. I1) THEN

         DO I=1,NX
            NUMBER(I) = 0
            SUM(I) = 0.0
            SUMSQ(I) = 0.0
         END DO

         IF ((.NOT.QUALITY) .AND. (.NOT.FLAGGED)) THEN

            DO J=I1,I2

               DO I=1,NX
                  SUM(I) = SUM(I) + IMAGE(I,J)
                  SUMSQ(I) = SUMSQ(I) + IMAGE(I,J)**2
                  NUMBER(I) = NUMBER(I) + 1
               END DO

            END DO

         ELSE IF (QUALITY) THEN

            DO J=I1,I2

               DO I=1,NX
                  IF (IMAGE_QUALITY(I,J) .EQ. GOOD) THEN
                     SUM(I) = SUM(I) + IMAGE(I,J)
                     SUMSQ(I) = SUMSQ(I) + IMAGE(I,J)**2
                     NUMBER(I) = NUMBER(I) + 1
                  ENDIF
               END DO

            END DO

         ELSE IF (FLAGGED) THEN

            DO J=I1,I2

               DO I=1,NX
                  IF (IMAGE(I,J) .NE. FBAD) THEN
                     SUM(I) = SUM(I) + IMAGE(I,J)
                     SUMSQ(I) = SUMSQ(I) + IMAGE(I,J)**2
                     NUMBER(I) = NUMBER(I) + 1
                  ENDIF
               END DO

            END DO

         ENDIF

         DO I=1,NX

            IF (NUMBER(I).GT.0) THEN

               SPECT(I) = SUM(I) / NUMBER(I)
               SPECT_VARIANCE(I) = (SUMSQ(I) - SUM(I)**2/NUMBER(I)) /
     :            (NUMBER(I)*(NUMBER(I) - 1))
               IF (QUALITY) THEN
                  SPECT_QUALITY(I)=GOOD
               ENDIF

            ELSE

               SPECT(I)=0.0
               SPECT_VARIANCE(I)=0.0
               IF (QUALITY) THEN
                  SPECT_QUALITY(I)=BAD
               ELSE IF (FLAGGED) THEN
                  SPECT(I)=FBAD
               ENDIF

            ENDIF

         END DO

      ENDIF

      END
