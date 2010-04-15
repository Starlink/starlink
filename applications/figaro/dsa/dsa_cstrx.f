C+
C                           D S A _ C S T R x
C
C  Routine name:
C     DSA_CSTRx
C
C  Function:
C     Calculate and apply a scaling factor and offset to a data array.
C
C  Description:
C     DSA_CSTRx covers the routines DSA_CSTRF, DSA_CSTRI, DSA_CSTRB,
C     DSA_CSTRU, DSA_CSTRD and DSA_CSTRS, which calculate and apply a
C     scaling factor and offset to data arrays of type float, integer,
C     byte, unsigned short, double and short respectively.  In each
C     case the number of artihmetic errors that result from the scaling
C     is returned.
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL DSA_CSTRx (ELEMENTS,BSCALE,BZERO,BASE,ARRAY,ERRORS)
C
C  Parameters:   (">" input, "!" modified, "W" workspace, "<" output)
C
C     (>) ELEMENTS      (Integer,ref) The number of elements in the
C                       array.
C     (<) BSCALE        (Double precision,ref) The scaling factor to
C                       be applied.
C     (<) BZERO         (Double precision,ref) The offset value to be
C                       applied.
C     (<) BASE          (Short array, ref) The base array to be scaled.
C     (>) ARRAY         (Array of appropriate type, ref) The array to
C                       be scaled.  The type varies with the routine.
C                       The operation performed is
C                       BASE(i) = ( ARRAY(i) - BZERO ) / BSCALE, with
C                       BZERO and BSCALE calculated suitably.
C     (<) ERRORS        (Integer,ref) The number of errors (mostly
C                       overflows) resulting from the application of the
C                       scale and zero values.
C
C  Authors: Keith Shortridge, AAO
C           Horst Meyerdierks, UoE, Starlink
C-
C  History:
C     17th July  1987  Original version.  KS / AAO.
C     24th April 1989  Support for USHORT type added. KS / AAO.
C     20th Aug   1992  Avoid LIB$ calls by more robust coding. HME / UoE.
C     15th Aug   2005  Portability means that IAND arguments must both
C                      be of the same type. TIMJ / JACH
C+
      SUBROUTINE DSA_CSTRF (ELEMENTS,BSCALE,BZERO,BASE,ARRAY,ERRORS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENTS, ERRORS
      INTEGER*2 BASE(ELEMENTS)
      REAL ARRAY(ELEMENTS)
      DOUBLE PRECISION BSCALE, BZERO
C
C     Local variables
C
      INTEGER I
      DOUBLE PRECISION SCALE, VMAX, VMIN
      DOUBLE PRECISION TEMP
C
C     Find extrema.
C
      VMIN = DBLE(ARRAY(1))
      VMAX = VMIN
      DO I = 2, ELEMENTS
         IF ( DBLE(ARRAY(I)) .GT. VMAX ) VMAX = DBLE(ARRAY(I))
         IF ( DBLE(ARRAY(I)) .LT. VMIN ) VMIN = DBLE(ARRAY(I))
      END DO
C
C     BSCALE = (VMAX-VMIN)/65530 or
C     BSCALE = 1 if VMAX ~= VMIN
C     BSCALE < 3D-12 corresponds to VMAX-VMIN < 2D-7, which is near the
C     machine precision for REAL*4.
C
      BSCALE = VMAX / 65530D0
      BSCALE = BSCALE - VMIN / 65530D0
      IF ( BSCALE .LE. 3D-12 ) BSCALE = 1D0
      SCALE = 1D0 / BSCALE
C
C     BZERO = (VMAX+VMIN)/2
C
      BZERO = VMAX * .5D0
      BZERO = BZERO + VMIN * .5D0
C
C     BASE = ( ARRAY - BZERO ) * SCALE
C     The second statement has implied type conversion R*8 to I*2
C
      DO I = 1, ELEMENTS
         TEMP = DBLE(ARRAY(I)) - BZERO
         BASE(I) = TEMP * SCALE
      END DO
C
      ERRORS = 0
C
      END
C
      SUBROUTINE DSA_CSTRD (ELEMENTS,BSCALE,BZERO,BASE,ARRAY,ERRORS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENTS, ERRORS
      INTEGER*2 BASE(ELEMENTS)
      DOUBLE PRECISION ARRAY(ELEMENTS)
      DOUBLE PRECISION BSCALE, BZERO
C
C     Local variables
C
      INTEGER I
      DOUBLE PRECISION SCALE, VMIN, VMAX
      DOUBLE PRECISION TEMP
C
      VMIN = ARRAY(1)
      VMAX = VMIN
      DO I = 2, ELEMENTS
         IF ( ARRAY(I) .GT. VMAX ) VMAX = ARRAY(I)
         IF ( ARRAY(I) .LT. VMIN ) VMIN = ARRAY(I)
      END DO
C
      BSCALE = VMAX / 65530D0
      BSCALE = BSCALE - VMIN / 65530D0
      IF ( BSCALE .LE. 3D-12 ) BSCALE = 1D0
      SCALE = 1D0 / BSCALE
C
      BZERO = VMAX * .5D0
      BZERO = BZERO + VMIN * .5D0
C
      DO I = 1, ELEMENTS
         TEMP = ARRAY(I) - BZERO
         BASE(I) = TEMP * SCALE
      END DO
C
      ERRORS = 0
C
      END
C
      SUBROUTINE DSA_CSTRB (ELEMENTS,BSCALE,BZERO,BASE,ARRAY,ERRORS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENTS, ERRORS
      INTEGER*2 BASE(ELEMENTS)
      BYTE ARRAY(ELEMENTS)
      DOUBLE PRECISION BSCALE, BZERO
C
C     Local variables
C
      INTEGER I
C
      BZERO=0.0
      BSCALE=1.0
      DO I=1,ELEMENTS
         BASE(I)=ARRAY(I)
      END DO
      ERRORS=0
C
      END
C
      SUBROUTINE DSA_CSTRS (ELEMENTS,BSCALE,BZERO,BASE,ARRAY,ERRORS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENTS, ERRORS
      INTEGER*2 BASE(ELEMENTS)
      INTEGER*2 ARRAY(ELEMENTS)
      DOUBLE PRECISION BSCALE, BZERO
C
C     Local variables
C
      INTEGER I
C
      BZERO=0.0
      BSCALE=1.0
      DO I=1,ELEMENTS
         BASE(I)=ARRAY(I)
      END DO
      ERRORS=0
C
      END
C
      SUBROUTINE DSA_CSTRI (ELEMENTS,BSCALE,BZERO,BASE,ARRAY,ERRORS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ELEMENTS, ERRORS
      INTEGER*2 BASE(ELEMENTS)
      INTEGER ARRAY(ELEMENTS)
      DOUBLE PRECISION BSCALE, BZERO
C
C     Local variables
C
      INTEGER I
      DOUBLE PRECISION SCALE, VMIN, VMAX
      DOUBLE PRECISION TEMP
C
      VMIN = DBLE(FLOAT(ARRAY(1)))
      VMAX = VMIN
      DO I = 2, ELEMENTS
         IF ( DBLE(FLOAT(ARRAY(I))) .GT. VMAX )
     :      VMAX = DBLE(FLOAT(ARRAY(I)))
         IF ( DBLE(FLOAT(ARRAY(I))) .LT. VMIN )
     :      VMIN = DBLE(FLOAT(ARRAY(I)))
      END DO
C
      BSCALE = VMAX / 65530D0
      BSCALE = BSCALE - VMIN / 65530D0
      IF ( BSCALE .LE. 3D-12 ) BSCALE = 1D0
      SCALE = 1D0 / BSCALE
C
      BZERO = VMAX * .5D0
      BZERO = BZERO + VMIN * .5D0
C
      DO I = 1, ELEMENTS
         TEMP = DBLE(FLOAT(ARRAY(I))) - BZERO
         BASE(I) = TEMP * SCALE
      END DO
C
      ERRORS = 0
C
      END
C
      SUBROUTINE DSA_CSTRU (ELEMENTS,BSCALE,BZERO,BASE,ARRAY,ERRORS)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER*2 NUM__MINW
      INTEGER*2 NUM__MAXW
      PARAMETER ( NUM__MINW = -32768 )
      PARAMETER ( NUM__MAXW = 32767 )

      INTEGER ELEMENTS, ERRORS
      INTEGER*2 BASE(ELEMENTS)
      INTEGER*2 ARRAY(ELEMENTS)
      DOUBLE PRECISION BSCALE, BZERO
C
C     Local variables
C
      LOGICAL OFFSET
      INTEGER I
C
C     This is the only one of these routines that needs explanation, and
C     is odd only because Fortran doesn't support unsigned integer types.
C     If the data in the array is all in the range 0..32767 then the
C     signed and unsigned representations are the same and we leave it alone.
C     If there are values such as 'FFFF'X (=65535 unsigned, =-1 signed),
C     then we set BZERO to 32768.0 and flip the sign bit of each word.
C
C     Hex pattern (in ARRAY):               0000    7FFF    8000    FFFF
C     As an unsigned value
C        (ie the value we want):               0   32767   32768   65535
C     The value we want -32768
C       (ie the offsetted value we need): -32768      -1       0   32767
C     This offsetted value in hex
C       (ie what we want in BASE)           8000    FFFF    0000    7FFF
C
C     Note that the last line is the first line with the highest order
C     bit flipped.  So if we take an unsigned integer, flip the sign bit,
C     treat the result as signed and add 32768, we get the original
C     value.  Of course, this only works on machines that use 2s complement
C     for integers, but that's most machines...
C
      BZERO=0.0
      BSCALE=1.0
      OFFSET=.FALSE.
      DO I=1,ELEMENTS
         IF (ARRAY(I).LT.0) THEN
            OFFSET=.TRUE.
            GO TO 340
         END IF
      END DO
  340 CONTINUE
      IF (OFFSET) THEN
         BZERO=32768.0
         DO I=1,ELEMENTS
            IF (ARRAY(I).LT.0) THEN
               BASE(I)=IAND(ARRAY(I),NUM__MAXW)
            ELSE
               BASE(I)=IOR(ARRAY(I),NUM__MINW)
            END IF
         END DO
      END IF
      ERRORS=0
C
      END
