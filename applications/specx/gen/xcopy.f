
C-----------------------------------------------------------------
*------------------------------------------------------------------------

      SUBROUTINE XCOPY (NBYTES, ARRAY1, ARRAY2)

C  Routine to move an array of NBYTES from one address to another

      IMPLICIT NONE

C  Formal parameters

      BYTE      ARRAY1(*)
      BYTE      ARRAY2(*)
      INTEGER*4 NBYTES

C  Local variables

      INTEGER*4 J

      DO J = 1,NBYTES
        ARRAY2(J) = ARRAY1(J)
      END DO

      RETURN
      END
