C*GRDTYP -- decode graphics device type string
C+
      INTEGER FUNCTION GRDTYP (TEXT)
C
C GRPCKG (internal routine): determine graphics device type code from
C type name. It compares the argument with the table of known device
C types in common.
C
C Argument:
C
C TEXT (input, character): device type name, eg 'PRINTRONIX'; the name
C       may be abbreviated to uniqueness.
C
C Returns:
C
C GRDTYP (integer): the device type code, in the range 1 to
C       GRTMAX, zero if the type name is not recognised, or -1
C       if the type name is ambiguous.
C--
C 27-Dec-1984 - rewrite so that is doesn't have to be modified for
C               new devices [TJP].
C  5-Aug-1986 - add GREXEC support [AFT].
C 10-Nov-1995 - ignore drivers that report no device type [TJP].
C 30-Aug-1996 - check for an exact match; indicate if type is
C               ambiguous [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'grpckg1.inc'
      CHARACTER*(*) TEXT
      INTEGER  CODE, I, L, MATCH
      REAL     RBUF(6)
      INTEGER NDEV,NBUF,LCHR
      INTEGER GRTRIM
      CHARACTER*32 CHR
C
      GRDTYP = 0
      L = GRTRIM(TEXT)
      IF (L.LT.1) RETURN
      MATCH = 0
      CODE = 0
      CALL GREXEC(0,0,RBUF,NBUF,CHR,LCHR)
      NDEV=NINT(RBUF(1))
      DO 30 I=1,NDEV
         CALL GREXEC(I, 1,RBUF,NBUF,CHR,LCHR)
         IF (LCHR.GT.0) THEN
            IF(TEXT(1:L).EQ.CHR(1:L)) THEN
               IF (CHR(L+1:L+1).EQ.' ') THEN
C                 -- exact match
                  GRDTYP = I
                  GRGTYP = GRDTYP
                  RETURN
               ELSE
                  MATCH = MATCH+1
                  CODE = I
               END IF
            END IF
         END IF
   30 CONTINUE
      IF (MATCH.EQ.0) THEN
C        -- no match
         GRDTYP = 0
      ELSE IF (MATCH.EQ.1) THEN
         GRDTYP = CODE
         GRGTYP = GRDTYP
      ELSE
         GRDTYP = -1
      END IF
C
      END
