*-----------------------------------------------------------------------
*    symbols.inc
*    common block to hold symbol table for SPECX command language
*-----------------------------------------------------------------------

      INTEGER*4 SZTABLE
      PARAMETER (SZTABLE=100)
      INTEGER*4 LSYMBOL
      PARAMETER (LSYMBOL=64)
      INTEGER*4 LNAMES
      PARAMETER (LNAMES=32)

      INTEGER*4 STMAX
      CHARACTER SYMBOLS*(LSYMBOL)

      COMMON /SYMBS/ STMAX, SYMBOLS(SZTABLE)
      SAVE   /SYMBS/


