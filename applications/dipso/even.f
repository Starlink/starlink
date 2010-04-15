      LOGICAL FUNCTION EVEN (N)
C  Is integer N even ?
      INTEGER N, TN
      REAL RN
      RN = 0.5 * REAL(N)
      TN = 2 * INT(RN)
      EVEN = (N.EQ.TN)
      END
