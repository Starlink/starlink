*------------------------------------------------------------------------

      SUBROUTINE SPECX_JSORT (N, IARRAY, ISOURCE)

C  Routine to sort integer array IARRAY into order of increasing
C  size. Sorts ISOURCE in parallel for use later indexing etc.

      INTEGER*4 IARRAY(*), ISOURCE(*)

C  Initialize the source array (on sorting of input array the indices will
C  show where each output point came from in the original array)

      DO J = 1, N
        ISOURCE(J) = J
      END DO

C  Then do the sort: Stop when no values are exchanged in a complete
C  iteration

      NSWAP = 1
      I     = 1
      DO WHILE (NSWAP.NE.0 .AND. I.LE.N-1)
        NSWAP = 0
        DO J = 1, N-1
          IF (IARRAY(J).GT.IARRAY(J+1)) THEN
            NSWAP = NSWAP + 1
            CALL EXCHNGE (J, J+1, IARRAY, 4)
            CALL EXCHNGE (J, J+1, ISOURCE, 4)
          END IF
        END DO
        I = I + 1
      END DO

      RETURN
      END

*------------------------------------------------------------------------

