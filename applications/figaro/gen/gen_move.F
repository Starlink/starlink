#if HAVE_CONFIG_H
#  include "config.h"
#endif
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
C     24th Apr 2006  (TIMJ/JAC) Check for NULL pointers
C
      IMPLICIT NONE
C
      INTEGER BYTES
      BYTE FROM(BYTES),TO(BYTES)
C
      INTEGER I
      INTEGER*8 DST,SRC
C

#if HAVE_PERCENTLOC
      DST=%LOC(TO(1))
      SRC=%LOC(FROM(1))
#elif HAVE_INTRINSIC_LOC
      DST=LOC(TO(1))
      SRC=LOC(FROM(1)) 
#else
      Do not have LOC or %LOC
#endif

C
C     Sanity check to prevent SEGV
C
      IF ( DST .EQ. 0) THEN
         PRINT *,'PANIC! NULL pointer given to GEN_MOVE for DST'
         STOP
      END IF

      IF ( SRC .EQ. 0) THEN
         PRINT *,'PANIC! NULL pointer given to GEN_MOVE for SRC'
         STOP
      END IF

C
C     The easy case is when dst <= src
C
      IF (DST.LE.SRC) THEN
          DO I=1,BYTES
             TO(I)=FROM(I)
          END DO
      ELSE
C
C         The hard case is when dst > src
C
          DO I=BYTES,1,-1
             TO(I)=FROM(I)
          END DO
      ENDIF
      END
