*  History:
*     08 Dec 1993 (hme):
*        SYMTAB_INSTALLED must be declared LOGICAL, since it is used as
*        one. If it is declared INTEGER, the Sun (for one) may take
*        wrong decisions.
*-----------------------------------------------------------------------
      INTEGER*4 TABLE_ADDRESS
      INTEGER*4 LENGTH_ADDRESS
      LOGICAL*4 SYMTAB_INSTALLED
      COMMON /GEN_SYMBOLS/ TABLE_ADDRESS, LENGTH_ADDRESS,
     &                     SYMTAB_INSTALLED

      SAVE   /GEN_SYMBOLS/

