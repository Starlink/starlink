*-----------------------------------------------------------------------

      LOGICAL*4 FUNCTION GEN_HASHDEL (SYMBOL, M, TABLE, HASH)

      IMPLICIT  NONE

      STRUCTURE /SYMTAB/
        CHARACTER NAME*16
        INTEGER*4 VALUE
      END STRUCTURE

*     Formal parameters

      CHARACTER       SYMBOL*(*)
      INTEGER*4       M
      RECORD /SYMTAB/ TABLE(0:M-1)
      INTEGER*4       HASH

*     Local variables

      CHARACTER       EMPTY*4 /'####'/
      INTEGER*4       VALUE

*     Functions

      INTEGER*4       GEN_HASHSRCH

*     OK? Go..

      HASH = GEN_HASHSRCH (SYMBOL, M, TABLE, VALUE)
      IF (HASH.NE.-1) THEN
        TABLE(HASH).NAME  = EMPTY
        GEN_HASHDEL    = .TRUE.
      ELSE
        GEN_HASHDEL    = .FALSE.
        TYPE *,'-- gen_hashdel --'
        TYPE *,'Symbol not defined'
      END IF

      RETURN
      END
