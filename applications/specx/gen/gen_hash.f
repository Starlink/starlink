*  History:
*     16 Nov 1993 (hme):
*        Replace STR$TRIM with CHR_LEN.
*        Replace CHR_LEN with GEN_ILEN
*-----------------------------------------------------------------------

      INTEGER*4 FUNCTION GEN_HASH (SYMBOL, M)

      IMPLICIT  NONE

*     Formal parameters

      CHARACTER  SYMBOL*(*)
      INTEGER    M

*     Local variables

      INTEGER    J
      INTEGER    LENSYMB

      INTEGER    HASHKEY
      PARAMETER (HASHKEY=128)

*     Functions

      INTEGER GEN_ILEN

*     OK? Go..

      LENSYMB  = GEN_ILEN (SYMBOL)
      GEN_HASH = ICHAR    (SYMBOL(1:1))

      IF (LENSYMB.EQ.1) THEN
        GEN_HASH = MOD (GEN_HASH, HASHKEY)
      ELSE
        DO J = 2, LENSYMB
          GEN_HASH = MOD (GEN_HASH*(HASHKEY) + ICHAR (SYMBOL(J:J)), M)
        END DO
      END IF

      RETURN
      END
