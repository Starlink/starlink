
C*GRSIZE -- inquire device size and resolution
C+
      SUBROUTINE GRSIZE (IDENT,XSZDEF,YSZDEF,XSZMAX,YSZMAX,
     1                   XPERIN,YPERIN)
C
C GRPCKG : obtain device parameters (user-callable routine).
C--
C (1-Feb-1983)
C  5-Aug-1986 - add GREXEC support [AFT].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      INTEGER IDENT
      REAL XSZDEF, YSZDEF, XSZMAX, YSZMAX, XPERIN, YPERIN
      INTEGER NBUF,LCHR
      REAL    RBUF(6)
      CHARACTER CHR
C
      CALL GRSLCT(IDENT)
      CALL GREXEC(GRGTYP, 6,RBUF,NBUF,CHR,LCHR)
      XSZDEF = RBUF(2)
      YSZDEF = RBUF(4)
      CALL GREXEC(GRGTYP, 2,RBUF,NBUF,CHR,LCHR)
      XSZMAX = RBUF(2)
      YSZMAX = RBUF(4)
      XPERIN = GRPXPI(GRCIDE)
      YPERIN = GRPYPI(GRCIDE)
C
      END
