
C
C.PGNPL -- Work out how many numerals there are in an integer
C.
      SUBROUTINE PGNPL (NMAX, N, NPL)
C
      INTEGER NMAX, N, NPL
C
C     Work out how many numerals there are in an integer for use with 
C     format statements.   
C     e.g.  N=280 => NPL=3,   N=-3 => NPL=2
C
C     Input:
C       NMAX   :   If > 0, issue a warning that N is going to
C                  exceed the format statement field size if NPL 
C                  exceeds NMAX
C       N      :   Integer of interest
C     Output:
C       NPL    :   Number of numerals
C
C-
C  20-Apr-1991 -- new routine (Neil Killeen)
C-------------------------------------------------------------------------
      IF (N.EQ.0) THEN
        NPL = 1
      ELSE
        NPL = INT(LOG10(REAL(ABS(N)))) + 1
      END IF
      IF (N.LT.0) NPL = NPL + 1
C
      IF (NMAX.GT.0 .AND. NPL.GT.NMAX) 
     *  CALL GRWARN ('PGNPL: output conversion error likely; '
     *               //'number too big for format')
C
      RETURN
      END
