C*GRSLCT -- select active output device
C+
      SUBROUTINE GRSLCT (IDENT)
C
C GRPCKG: Check that IDENT is a valid plot identifier, and select the
C corresponding plot as the current plot. All subsequent plotting will
C be directed to this device until the assignment is changed by another
C call to GRSLCT.
C
C Argument:
C
C IDENT (input, integer): the identifier of the plot to be selected, as
C       returned by GROPEN.
C--
C (1-Feb-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C  4-Jun-1987 - skip action if no change in ID [TJP].
C 26-Nov-1990 - [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      REAL     RBUF(6)
      INTEGER  IDENT, NBUF,LCHR
      CHARACTER CHR
C
      IF ((IDENT.LE.0) .OR. (IDENT.GT.GRIMAX) .OR.
     1     (GRSTAT(IDENT).EQ.0)) THEN
         CALL GRWARN('GRSLCT - invalid plot identifier.')
      ELSE IF (IDENT.EQ.GRCIDE) THEN
         GRGTYP = GRTYPE(IDENT)
         RETURN
      ELSE
         GRCIDE = IDENT
         GRGTYP = GRTYPE(IDENT)
         RBUF(1)= GRCIDE
         RBUF(2)= GRUNIT(GRCIDE)
         NBUF   = 2
         CALL GREXEC(GRGTYP, 8,RBUF,NBUF,CHR,LCHR)
      END IF
      END
