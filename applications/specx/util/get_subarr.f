C-----------------------------------------------------------------------

      SUBROUTINE GET_SUBARR (ARRAY,NX,NY,SUB_ARRAY,M,N,IOFFX,IOFFY)

C   Routine to extract a subarray from a larger array, given the size
C   of both arrays and the offsets in the main array

      REAL*4 ARRAY(NX,NY)
      REAL*4 SUB_ARRAY(M,N)

      DO I = 1,M
        IF (I+IOFFX-1 .LE. NX) THEN
          DO J = 1,N
            IF (J+IOFFY-1 .LE. NY) THEN
              SUB_ARRAY(I,J) = ARRAY(I+IOFFX-1,J+IOFFY-1)
            END IF
          END DO
        END IF
      END DO

      RETURN
      END

C-----------------------------------------------------------------------

