C+
      SUBROUTINE FIT_ERROR (STATUS,ERROR)
C
C     F I T _ E R R O R
C
C     Decodes a FIT_ routine error code.  Most of these
C     are MTPCKG error codes, and the actual error message
C     is already decoded in the common variable MTERR.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) STATUS    (Integer) Status code returned by a FIT_
C                   routine.
C     (<) ERROR     (Character) A description of the error.
C
C     Common variables used -
C
C     (>) MTSTAT    (Integer) Last MTPCKG error code.
C     (>) MTERR     (Character) Description of last tape I/O error
C
C     MTSTAT is defined in COMF.INC, MTERR in COMB.INC
C
C     Subroutines / function used -  None
C
C                                        KS / CIT 13th April 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER STATUS
      CHARACTER*(*) ERROR
C
C     Common variables
C
      INCLUDE 'COMB'
      INCLUDE 'COMF'
C
C     Local variables
C
      INTEGER IGNORE
C
C     Assume error code is the last MTPCKG code.  If not, we have
C     no idea what it is, unless it is the disk error flag (-1)
C
      IF ((STATUS.EQ.MTSTAT).OR.(STATUS.EQ.-1)) THEN
         ERROR=MTERR
      ELSE
         WRITE (ERROR,'(A,I5)',IOSTAT=IGNORE)
     :       'In this context cannot understand error code =',
     :       STATUS
      END IF
C
      END
