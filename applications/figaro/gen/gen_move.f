C+
      SUBROUTINE GEN_MOVE(BYTES,FROM,TO)
C
C     G E N _ M O V E
C
C     Fast copy of data from one array to another.
C     GEN_MOVE handles correctly the case
C     where the source and destination arrays overlap.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) BYTES    (Integer, ref) Number of bytes to be moved.
C     (>) FROM     (Array of any type, ref)  Source array.
C     (<) TO       (Array of any type, ref)  Destination array.
C
C     History:
C
C     31st Oct 1982  Original Author in VAX MACRO: Keith Shortridge, CIT
C     5th  Feb 1988  (CKL/CIT) Converted to Convex
C     20th Mar 1991  (SNS/CIT) Minor changes to compile silently on Suns
C     27th Aug 1992  (HME/UoE) No preprocessor. Sun accepts %LOC.
C
C
      IMPLICIT NONE
C
      INTEGER BYTES
      BYTE FROM(BYTES),TO(BYTES)
C
      INTEGER I
      INTEGER*8 DST,SRC
C
      DST=%LOC(TO(1))
      SRC=%LOC(FROM(1))
C
C     The easy case is when dst <= src
C
      IF (DST.LE.SRC) THEN
          DO 10 I=1,BYTES
             TO(I)=FROM(I)
10        CONTINUE
      ELSE
C
C         The hard case is when dst > src
C
          DO 20 I=BYTES,1,-1
             TO(I)=FROM(I)
20        CONTINUE
      ENDIF
      END
