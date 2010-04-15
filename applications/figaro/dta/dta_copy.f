C+
C                          D T A _ C O P Y
C
C  Routine name:
C     DTA_COPY
C
C  Function:
C     General copy of data from one array to another.
C
C  Description:
C     General purpose mover of bytes from one area of core to another.
C
C  Language:
C     Fortran.
C
C  Call:
C     CALL DTA_COPY (BYTES,FROM,TO)
C
C  Parameters -   (">" input, "<" output)
C
C     (>) BYTES    (Integer, ref) Number of bytes to be moved.
C     (>) FROM     (Array of any type, ref)  Source array.
C     (<) TO       (Array of any type, ref)  Destination array.
C
C  External subroutines / functions used:   None
C
C  External variables used:  None
C
C  Prior requirements:  None
C
C  Author: Keith Shortridge, AAO
C
C  Date: 17th Jan 1992.
C
C  Note:
C     The original DTA package on the VAX used a VAX MACRO routine called
C     DTA_COPY that did a fast data transfer between two arrays very
C     quickly and allowed for the case where the two arrays overlapped.
C     This more portable version can be used on any machine where
C     the compiler supports the BYTE data type, but is not going to be the
C     fastest byte copier available.  It does not support the overlapping
C     array case, but this is not something DTA needs.
C+
      SUBROUTINE DTA_COPY (BYTES,FROM,TO)
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER BYTES
      BYTE FROM(BYTES),TO(BYTES)
C
C     Local variables
C
      INTEGER I
C
      DO I=1,BYTES
         TO(I)=FROM(I)
      END DO
C
      END

