C*PGQDT -- inquire name of nth available device type
C%void cpgqdt(int n, char *type, int *type_length, char *descr, \
C% int *descr_length, int *inter);
C+
      SUBROUTINE PGQDT(N, TYPE, TLEN, DESCR, DLEN, INTER)
      INTEGER N
      CHARACTER*(*) TYPE, DESCR
      INTEGER TLEN, DLEN, INTER
C
C Return the name of the Nth available device type as a character
C string. The number of available types can be determined by calling
C PGQNDT. If the value of N supplied is outside the range from 1 to
C the number of available types, the routine returns DLEN=TLEN=0.
C
C Arguments:
C  N      (input)  : the number of the device type (1..maximum).
C  TYPE   (output) : receives the character device-type code of the
C                    Nth device type. The argument supplied should be
C                    large enough for at least 8 characters. The first
C                    character in the string is a '/' character.
C  TLEN   (output) : receives the number of characters in TYPE,
C                    excluding trailing blanks.
C  DESCR  (output) : receives a description of the device type. The
C                    argument supplied should be large enough for at
C                    least 64 characters.
C  DLEN   (output) : receives the number of characters in DESCR,
C                    excluding trailing blanks.
C  INTER  (output) : receives 1 if the device type is an interactive
C                    one, 0 otherwise.
C--
C 17-Mar-1997 - new routine [TJP].
C-----------------------------------------------------------------------
      INTEGER NDEV, NBUF, LCHR, L1, L2
      REAL RBUF
      CHARACTER*80 CHR
C
C Initialize PGPLOT if necessary.
C
      CALL PGINIT
C
      TYPE = 'error'
      TLEN = 0
      DESCR = ' '
      DLEN = 0
      INTER = 1
      CALL PGQNDT(NDEV)
      IF (N.GE.1 .AND. N.LE.NDEV) THEN
         NBUF = 0
         CALL GREXEC(N, 1, RBUF, NBUF, CHR, LCHR)
         IF (LCHR.GT.0) THEN
            L1 = INDEX(CHR(1:LCHR), ' ')
            IF (L1.GT.1) THEN
               TYPE(1:1) = '/'
               IF (LEN(TYPE).GT.1) TYPE(2:) = CHR(1:L1-1)
               TLEN = MIN(L1,LEN(TYPE))
            END IF
            L2 = INDEX(CHR(1:LCHR), '(')
            IF (L2.GT.0) DESCR = CHR(L2:LCHR)
            DLEN = MIN(LCHR-L2+1,LEN(DESCR))
            CALL GREXEC(N, 4, RBUF, NBUF, CHR, LCHR)
            IF (CHR(1:1).EQ.'H') INTER = 0
         END IF
      END IF
C
      END
