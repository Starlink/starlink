*------------------------------------------------------------------------

      SUBROUTINE EXCHNGE (N1, N2, ARRAY, NBYTE)

C  Routine to exchange array elements NBYTEs each long from position
C  N1 and N2 in the array.

C  Formal parameters:

      INCLUDE 'CNF_PAR'

      BYTE ARRAY(*)

      ISTAT = IGETVM (NBYTE, .TRUE., 'EXCHNGE', IPTR)

      CALL XCOPY  (NBYTE, ARRAY(1+(N1-1)*NBYTE), %VAL(CNF_PVAL(IPTR)))
      CALL XCOPY  (NBYTE, ARRAY(1+(N2-1)*NBYTE), ARRAY(1+(N1-1)*NBYTE))
      CALL XCOPY  (NBYTE, %VAL(CNF_PVAL(IPTR)),
     :             ARRAY(1+(N2-1)*NBYTE))

      ISTAT = IFREEVM (IPTR)

      RETURN
      END

*------------------------------------------------------------------------

