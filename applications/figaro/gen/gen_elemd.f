C+
      DOUBLE PRECISION FUNCTION GEN_ELEMD(ARRAY,N)
C
C     G E N _ E L E M D
C
C     Returns the value of the Nth element of a given array.
C     This is not quite as daft as it seems, because the array
C     in question may be mapped, and therefore not directly
C     accessible to the calling program.  Therefore most calls
C     to this routine will probably take the form
C     VALUE=GEN_ELEMD(%VAL(POINTER),N)
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY     (Double precision array ARRAY(N)) The given array.
C     (>) N         (Integer) The element of the array whose
C                   value is to be returned.
C
C     Returns
C
C     (<) GEN_ELEMF (Double precision)  The value of ARRAY(N)
C
C                                        KS / AAO 12th Sept 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER N
      DOUBLE PRECISION ARRAY(N)
C
C     Return the value
C
      GEN_ELEMD=ARRAY(N)
C
      END
