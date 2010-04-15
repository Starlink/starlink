C+
      SUBROUTINE GEN_FILL(BYTES,VALUE,ARRAY)
C
C     G E N _ F I L L
C
C     Sets an array of bytes to a specified value.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) BYTES (Integer, ref) The number of bytes to be filled.
C     (>) VALUE (Byte, ref) The value to be placed in each byte
C               of ARRAY.
C     (<) ARRAY (Array of any type, ref) The array to be
C               filled.
C
C     History:
C
C     28th Feb 1983  Original Author in VAX MACRO: Keith Shortridge, CIT
C     5th  Feb 1988  (CKL/CIT) Converted to Convex.
C     28th Sep 1992  HME / UoE, Starlink.  TABs removed.
C+
      IMPLICIT NONE
C
      INTEGER BYTES
      BYTE ARRAY(BYTES),VALUE
C
      INTEGER I
      DO 10 I=1,BYTES
         ARRAY(I)=VALUE
10    CONTINUE
      END
