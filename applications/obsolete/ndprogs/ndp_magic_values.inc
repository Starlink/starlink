C                                  
C     Magic-values as defined in SGP/38.
C     Modified for Sun's by GOLDJIL on 2/12/92
C
C     Unsigned byte.
C
      BYTE             MAGIC_BYTE
      PARAMETER        (MAGIC_BYTE='FF'X)
C
C     Signed word.
C
      INTEGER*2        MAGIC_SHORT
      PARAMETER        (MAGIC_SHORT='8000'X)
C
C     Signed double word.
C
      INTEGER          MAGIC_INT
      PARAMETER        (MAGIC_INT='80000000'X)
C
C     Single precision floating point.
C
      REAL             MAGIC_FLOAT
      PARAMETER        (MAGIC_FLOAT='FF7FFFFF'X)
C
C     Double precision floating point.
C
      DOUBLE PRECISION MAGIC_DOUBLE
      PARAMETER        (MAGIC_DOUBLE='FFEFFFFFFFFFFFFF'X)
