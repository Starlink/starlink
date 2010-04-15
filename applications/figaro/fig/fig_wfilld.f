C+
      SUBROUTINE FIG_WFILLD (WSTART,WEND,LOGR,NBINR,ARRAY)
C
C     F I G _ W F I L L D
C
C     Fills a double precision array with wavelength values.
C     SCRUNCH utility.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) WSTART    (Double precision) The wavelength of the center of
C                   the first bin in the wavelength array.
C     (>) WEND      (Double precision) The wavelength of the center of
C                   the last bin in the wavelength array.  Note that WEND
C                   is allowed to be less than WSTART.
C     (>) LOGR      (Logical) True if the wavelengths are to increase
C                   logarithmically.  Otherwise, the increase will
C                   be linear.
C     (>) NBINR     (Integer) The number of wavelength bins.
C     (<) ARRAY     (Double precision array ARRAY(NBINR)) The output
C                   wavelength array.
C
C                                              KS / AAO 13th Sept 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LOGR
      INTEGER NBINR
      DOUBLE PRECISION WSTART,WEND,ARRAY(NBINR)
C
C     Local variables
C
      INTEGER I
      DOUBLE PRECISION DWAVEL,DWINC
C
C     Fill the array
C
      IF (LOGR) THEN
         DWAVEL=LOG(DBLE(WSTART))
         DWINC=(LOG(DBLE(WEND))-DWAVEL)/DBLE(NBINR-1)
         DO I=1,NBINR
            ARRAY(I)=EXP(DWAVEL)
            DWAVEL=DWAVEL+DWINC
         END DO
      ELSE
         DWINC=DBLE(WEND-WSTART)/DBLE(NBINR-1)
         DWAVEL=WSTART
         DO I=1,NBINR
            ARRAY(I)=DWAVEL
            DWAVEL=DWAVEL+DWINC
         END DO
      END IF
C
      END
