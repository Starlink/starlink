
*-----------------------------------------------------------------------

      BLOCK DATA SYMTAB_INIT

      IMPLICIT  NONE

      INTEGER*4 TABLE_ADDRESS
      INTEGER*4 LENGTH_ADDRESS
      LOGICAL*4 SYMTAB_INSTALLED
      COMMON /GEN_SYMBOLS/ TABLE_ADDRESS, LENGTH_ADDRESS,
     &                     SYMTAB_INSTALLED

      DATA TABLE_ADDRESS    /0/
      DATA LENGTH_ADDRESS   /0/
      DATA SYMTAB_INSTALLED /.FALSE./

      END
