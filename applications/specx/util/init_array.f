C-----------------------------------------------------------------------

      SUBROUTINE INIT_ARRAY (SIZE, ARRAY, VALUE)

C   Routine to initialize an array to a given value.
C   Written as a subroutine so that we can use virtual memory.

      IMPLICIT  NONE

      INTEGER*4 SIZE
      INTEGER*4 ARRAY(SIZE)
      INTEGER*4 VALUE

      INTEGER*4 I

      DO I = 1, SIZE
        ARRAY(I) = VALUE
      END DO

      RETURN
      END

C-----------------------------------------------------------------------

