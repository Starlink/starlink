C+
C                         D T A _ F I L L
C
C  Routine name:
C     DTA_FILL
C
C  Function:
C     Sets an array of bytes to a specified value.
C
C  Description:
C     Fills every byte of an array with a specified value.
C
C  Language:
C     Fortran
C
C  Call:
C     CALL DTA_FILL(BYTES,VALUE,ARRAY)
C
C  Parameters -   (">" input, "<" output)
C
C     (>) BYTES (Integer, ref) The number of bytes to be filled.
C     (>) VALUE (Byte, ref) The value to be placed in each byte
C               of ARRAY.  (Can be any numeric quantity -
C               only the least significant byte will be used)
C     (<) ARRAY (Array of any type, ref) The array to be
C               filled.
C
C  External subroutines / functions used:   None
C
C  External variables used:  None
C
C  Author: Keith Shortridge, AAO
C
C  Date: 17th Jan 1992
C
C  Note:
C     The original DTA package on the VAX used a VAX MACRO routine called
C     DTA_FILL that did a fast fill of an array of bytes. This more portable
C     version can be used on any machine where the compiler supports the BYTE
C     data type, but is not going to be the byte filler available.
C+
      SUBROUTINE DTA_FILL(BYTES,VALUE,ARRAY)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER BYTES
      BYTE VALUE, ARRAY(BYTES)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,BYTES
         ARRAY(I)=VALUE
      END DO
C
      END

