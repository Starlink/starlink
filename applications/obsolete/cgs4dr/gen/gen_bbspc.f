C+
      SUBROUTINE GEN_BBSPC (REFWAVE, TEMPERATURE, AXIS,
     :   NPTS, DATA)
C
C     G E N _ B B S P C
C
C     Calculate a black-body of TEMPERATURE, at wavelength points
C     defined by the AXIS array, normalised to the value at REFWAVE.
C     Axis wavelengths are in microns, temperature in Kelvin.
C     The units are assumed to be flux per unit wavelength (F-lambda).
C
C     Parameters -    (">" input, "<" output)
C
C     (>) REFWAVE     (Real)       Normalisation wavelength
C     (>) TEMPERATURE (Real)       BB Temperature
C     (>) AXIS        (Real array) Wavelengths in microns
C     (>) NPTS        (Integer)    Number of points in arrays
C     (<) DATA        (Real array  Result array
C
C                                     JFL / ROE  7th Jun 1990
C
C+
      IMPLICIT NONE
      INTEGER  I
      INTEGER  NPTS
      REAL     CONST                ! Constant for BB func.
      PARAMETER (CONST = 14388.3)   ! Value in microns.degrees
      REAL     NORM                 ! Normalising BB value
      REAL     REFWAVE
      REAL     TEMPERATURE
      REAL     AXIS (NPTS)
      REAL     DATA (NPTS)

*    calculate normalisation value from ref-wavelength

      NORM = 1.0 / (REFWAVE**5*(EXP(CONST/(REFWAVE*TEMPERATURE))-1.0))

*    set values in DATA

      DO I = 1, NPTS
         DATA (I) = 1.0 /
     :      (AXIS(I)**5*(EXP(CONST/(AXIS(I)*TEMPERATURE))-1.0))
         DATA (I) = DATA(I) / NORM
      END DO

      END

