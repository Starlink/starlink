C+
      SUBROUTINE FIGE_XTRACT4(IMAGE,IMAGE_VARIANCE,IMAGE_QUALITY,
     :   NX,NY,IYST,IYEN,VARIANCE,QUALITY,FLAGGED,FBAD,SPECT,
     :   SPECT_VARIANCE,SPECT_QUALITY,NUMBER,SUM,SUMVAR)
C
C     F I G E _ X T R A C T 4
C
C     Averages consecutive rows of an image to form a 1-dimensional spectrum,
C     and combines the variances to form a variance array. The variances
C     calculated are actually the standard error squared.
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
C     (<) SUMVAR           (Real array (NX)) Work array.
C
C     Subroutines / functions used -  Only standard Fortran.
C
C                                                KS / CIT 17th March 1983
C     Modified to handle errors and quality      JFL / JACH 20th July 1990
C     JFL's version was determining the variance
C     of the spectrum from the dispersion of the
C     data values about the mean. This was
C     suitable for CGS3 data, but is not suitable
C     for any general 2-D spectra. Modified so the
C     variance of the spectrum is now a combination
C     of the variances from the image. JFL's
C     version is FIGE_XTRACT. This version renamed
C     to FIGE_XTRACT4, to avoid clashing.       SMB / ROE 5th December 1990
C     Made to multiply the result by the total
C     number of rows, to produce values
C     consistent with the standard FIG_XTRACT.  SMB / JACH 17th February 1991
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER NX,NY,IYST,IYEN,NUMBER(NX)
      REAL IMAGE(NX,NY),IMAGE_VARIANCE(NX,NY),SPECT(NX),
     :   SPECT_VARIANCE(NX),SUM(NX),SUMVAR(NX),FBAD
      LOGICAL VARIANCE,QUALITY,FLAGGED
      BYTE IMAGE_QUALITY(NX,NY),SPECT_QUALITY(NX)
C
C     Local constants
C
      BYTE GOOD, BAD
      PARAMETER (GOOD=0,BAD=1)
C
C     Local variables
C
      INTEGER I1,I2,I,J,NROWS,NROWSSQ
C
C     Perform extraction
C
      I1=MAX(IYST,1)
      I2=MIN(IYEN,NY)
      NROWS = I2 - I1 + 1
      NROWSSQ = NROWS * NROWS
C
C     Check if there is only one row to extract
C
      IF (I1 .EQ. I2) THEN
C
C        There is only one row. Simply copy the data and variance arrays
C        from the original image.
C
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
C
C        There is more than one row. First zero the counters.
C
         DO I=1,NX
            NUMBER(I) = 0
            SUM(I) = 0.0
            SUMVAR(I) = 0.0
         END DO
C
C        Check if there is quality information which needs to be checked.
C
         IF ((.NOT.QUALITY) .AND. (.NOT.FLAGGED)) THEN
C
C           There is no quality information. Simply sum all the data.
C           If there is a variance array, sum that as well.
C
            IF ( VARIANCE ) THEN

               DO J=I1,I2

                  DO I=1,NX
                     SUM(I) = SUM(I) + IMAGE(I,J)
                     SUMVAR(I) = SUMVAR(I) + IMAGE_VARIANCE(I,J)
                     NUMBER(I) = NUMBER(I) + 1
                  END DO

               END DO
            ELSE

               DO J=I1,I2

                  DO I=1,NX
                     SUM(I) = SUM(I) + IMAGE(I,J)
                     NUMBER(I) = NUMBER(I) + 1
                  END DO

               END DO
            END IF

         ELSE IF (QUALITY) THEN
C
C           There is a data quality array. Sum all the data values whose
C           quality values are GOOD. If there is a variance array, sum
C           that as well.
C
            IF ( VARIANCE ) THEN

               DO J=I1,I2

                  DO I=1,NX
                     IF (IMAGE_QUALITY(I,J) .EQ. GOOD) THEN
                        SUM(I) = SUM(I) + IMAGE(I,J)
                        SUMVAR(I) = SUMVAR(I) + IMAGE_VARIANCE(I,J)
                        NUMBER(I) = NUMBER(I) + 1
                     ENDIF
                  END DO

               END DO
            ELSE


               DO J=I1,I2

                  DO I=1,NX
                     IF (IMAGE_QUALITY(I,J) .EQ. GOOD) THEN
                        SUM(I) = SUM(I) + IMAGE(I,J)
                        NUMBER(I) = NUMBER(I) + 1
                     ENDIF
                  END DO

               END DO
            END IF

         ELSE IF (FLAGGED) THEN
C
C           Flagged values are being used. Sum all the data values not
C           flagged as BAD. If there is a variance array, sum that
C           as well.
C
            IF ( VARIANCE ) THEN

               DO J=I1,I2

                  DO I=1,NX
                     IF (IMAGE(I,J) .NE. FBAD) THEN
                        SUM(I) = SUM(I) + IMAGE(I,J)
                        SUMVAR(I) = SUMVAR(I) + IMAGE_VARIANCE(I,J)
                        NUMBER(I) = NUMBER(I) + 1
                     ENDIF
                  END DO

               END DO
            ELSE


               DO J=I1,I2

                  DO I=1,NX
                     IF (IMAGE(I,J) .NE. FBAD) THEN
                        SUM(I) = SUM(I) + IMAGE(I,J)
                        NUMBER(I) = NUMBER(I) + 1
                     ENDIF
                  END DO

               END DO
            END IF
         ENDIF
C
C        Now convert the sums into mean and standard error squared
C        (if there is a variance array). Multiply these values by
C        the total number of rows, to produce a result consistent
C        with the standard Figaro EXTRACT.
C
         IF ( VARIANCE ) THEN

            DO I=1,NX

               IF (NUMBER(I).GT.0) THEN

                  SPECT(I) = SUM(I) * REAL( NROWS ) / REAL( NUMBER(I) )
                  SPECT_VARIANCE(I) = SUMVAR(I) * REAL( NROWSSQ ) /
     :                                REAL( NUMBER(I)*NUMBER(I) )
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
         ELSE

            DO I=1,NX

               IF (NUMBER(I).GT.0) THEN

                  SPECT(I) = SUM(I) * REAL( NROWS ) / REAL( NUMBER(I) )
                  IF (QUALITY) THEN
                     SPECT_QUALITY(I)=GOOD
                  ENDIF

               ELSE

                  SPECT(I)=0.0
                  IF (QUALITY) THEN
                     SPECT_QUALITY(I)=BAD
                  ELSE IF (FLAGGED) THEN
                     SPECT(I)=FBAD
                  ENDIF

               ENDIF

            END DO
         END IF

      ENDIF

      END
