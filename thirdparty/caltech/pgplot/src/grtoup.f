
C*GRTOUP -- convert character string to upper case
C+
      SUBROUTINE GRTOUP (DST, SRC)
      CHARACTER*(*) DST, SRC
C
C GRPCKG (internal routine): convert character string to upper case.
C
C Arguments:
C  DST    (output) : output string (upper case).
C  SRC    (input)  : input string to be converted.
C--
C 1988-Jan-18 (TJP)
C-----------------------------------------------------------------------
      INTEGER I, N, NCHI, NCHO, NCH
      NCHI = LEN(SRC)
      NCHO = LEN(DST)
      NCH = MIN(NCHI, NCHO)
      DO 10 I=1,NCH
          N = ICHAR(SRC(I:I))
          IF ((N .GE. 97) .AND. (N .LE. 122)) THEN
              DST(I:I) = CHAR(N - 32)
          ELSE
              DST(I:I) = CHAR(N)
          END IF
   10 CONTINUE
      IF (NCHO .GT. NCHI) DST(NCHI+1:NCHO) = ' '
      END
