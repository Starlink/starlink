*+STRIM      Trims character string
      SUBROUTINE STRIM(CHIN, CHOUT, NCHARS)
      IMPLICIT NONE
      CHARACTER*(*) CHIN	! In	String
      CHARACTER*(*) CHOUT	! Out	String
      INTEGER NCHARS		!	Length of output string
*-
* Local Variables
      INTEGER M, N

* Executable code
      N = LEN(CHIN)
      DO WHILE (N .GT.0 .AND. (CHIN(N:N) .EQ. ' ' .OR. CHIN(N:N) .EQ. CHAR(0)))
         N = N - 1
      END DO
      IF (N .EQ. 0) THEN
         NCHARS = 0
      ELSE IF (N.EQ.1) THEN
         NCHARS = 1
         CHOUT = CHIN(:1)
      ELSE
         M = 1
         DO WHILE (M .LT. N .AND. (CHIN(M:M) .EQ. ' '))
            M = M + 1
         END DO
         CHOUT = CHIN(M:N)
         NCHARS = N - M + 1
      END IF

      END
