
*-----------------------------------------------------------------------

      INTEGER*4 FUNCTION GEN_HASHSRCH (SYMBOL, M, TABLE, VALUE)

      IMPLICIT  NONE

      STRUCTURE /SYMTAB/
        CHARACTER NAME*16
        INTEGER*4 VALUE
      END STRUCTURE

*     Formal parameters

      CHARACTER       SYMBOL*(*)
      INTEGER*4       M
      RECORD /SYMTAB/ TABLE(0:M-1)
      INTEGER*4       VALUE

*     Local variables

      INTEGER*4       COUNT

*     Functions

      INTEGER*4       GEN_HASH

*     OK? Go..

      COUNT       = 1
      GEN_HASHSRCH = GEN_HASH (SYMBOL, M)

      DO WHILE (      TABLE(GEN_HASHSRCH).NAME .NE. SYMBOL
     &          .AND. TABLE(GEN_HASHSRCH).NAME .NE. ' '
     &          .AND. COUNT.LT.M)
        GEN_HASHSRCH = MOD (GEN_HASHSRCH+1, M)
        COUNT          = COUNT + 1
      END DO

      IF (TABLE(GEN_HASHSRCH).NAME .NE. SYMBOL) THEN
        GEN_HASHSRCH = -1
D       TYPE *,'-- gen_hashsrch --'
D       TYPE *,'Symbol not defined'
      ELSE
        VALUE = TABLE(GEN_HASHSRCH).VALUE
      END IF

      RETURN
      END
