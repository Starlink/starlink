C+
      SUBROUTINE FIG_WFILL (WSTART,WEND,LOGR,NBINR,ARRAY)
C
C     F I G _ W F I L L
C
C     Fills an array with wavelength values.  SCRUNCH utility.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) WSTART    (Real) The wavelength of the center of the first
C                   bin in the wavelength array.
C     (>) WEND      (Real) The wavelength of the center of the last
C                   bin in the wavelength array.  Note that WEND is
C                   allowed to be less than WSTART.
C     (>) LOGR      (Logical) True if the wavelengths are to increase
C                   logarithmically.  Otherwise, the increase will
C                   be linear.
C     (>) NBINR     (Integer) The number of wavelength bins.
C     (<) ARRAY     (Real array ARRAY(NBINR)) The output wavelength
C                   array.
C
C                                              KS / CIT 7th Feb 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL LOGR
      INTEGER NBINR
      REAL WSTART,WEND,ARRAY(NBINR)
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
