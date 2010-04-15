C+
      SUBROUTINE DSA_ENCDIM (CHARS,NDIM,DIMS,IPT)
C
C     D S A _ E N C D I M
C
C     Encodes dimension information into a character string.
C
C     Parameters -   (">" input, "!" modified, "<" output)
C
C     (>) CHARS    (String) The string into which to encode the
C                  dimensions.
C     (>) NDIM     (Integer) The number of dimensions in the array.
C     (>) DIMS     (Integer array DIMS(NDIM)) The dimensions of the
C                  array.
C     (!) IPT      (Integer) Character number. Passed as the character
C                  at which to start the encoding (ie where the '['
C                  goes) and returned as the number of the last character
C                  used (the one where the ']' went).
C
C     Common variables used - None
C
C                                            KS / AAO  2nd June 1988
C     History:
C     2nd  June 1988  KS / AAO.  Original version.
C     2nd  Sep  1992  HME / UoE, Starlink.  Change declared length
C                     MAX(...) to *.
C                     Change FIG_ENCDIM to DSA_ENCDIM.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NDIM,DIMS(*),IPT
      CHARACTER CHARS*(*)
C
C     Local variables
C
      INTEGER I, IWPT, LIMPTR, N
      CHARACTER WORK*20
C
      LIMPTR=LEN(CHARS)
      IF (NDIM.EQ.0) THEN
         CHARS(IPT:)='['
         IPT=IPT+1
      ELSE
         DO I=1,NDIM
            IF (I.EQ.1) THEN
               CHARS(IPT:IPT)='['
            ELSE
               CHARS(IPT:IPT)=','
            END IF
            IPT=IPT+1
            N=DIMS(I)
            IWPT=20
            DO WHILE (N.GT.0)
               WORK(IWPT:IWPT)=CHAR(MOD(N,10)+ICHAR('0'))
               IWPT=IWPT-1
               N=N/10
            END DO
            N=IPT+19-IWPT
            IF (N.LE.LIMPTR) THEN
               CHARS(IPT:N)=WORK(IWPT+1:)
               IPT=N+1
            END IF
         END DO
      END IF
      IF (IPT.LE.LIMPTR) CHARS(IPT:IPT)=']'
C
      END
