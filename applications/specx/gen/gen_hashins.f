*-----------------------------------------------------------------------

      INTEGER*4 FUNCTION GEN_HASHINS (SYMBOL, M, TABLE, VALUE)

      IMPLICIT  NONE

*     Formal parameters

      STRUCTURE /SYMTAB/
        CHARACTER NAME*16
        INTEGER*4 VALUE
      END STRUCTURE

      CHARACTER       SYMBOL*(*)
      INTEGER*4       M
      RECORD /SYMTAB/ TABLE(0:M-1)
      INTEGER*4       VALUE

*     Local variables

      INTEGER*4 COUNTER
      CHARACTER EMPTY*4   /'####'/     ! placeholder for deleted items

*     Functions

      INTEGER*4 GEN_HASH

*     OK? Go..

      COUNTER        = 1
      GEN_HASHINS = GEN_HASH (SYMBOL, M)

      DO WHILE (      TABLE(GEN_HASHINS).NAME .NE. ' '
     &          .AND. TABLE(GEN_HASHINS).NAME .NE. EMPTY
     &          .AND. COUNTER.LT.M)
        GEN_HASHINS = MOD (GEN_HASHINS+1, M)
        COUNTER = COUNTER + 1
      END DO

      IF (COUNTER.NE.M) THEN
        TABLE(GEN_HASHINS).NAME  = SYMBOL
        TABLE(GEN_HASHINS).VALUE = VALUE
      ELSE
        GEN_HASHINS = -1
        TYPE *,'-- gen_hashins --'
        TYPE *,'Hash table full!'
      END IF

      RETURN
      END
