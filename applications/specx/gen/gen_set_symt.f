* History:
*    7 Jun 2000 (ajc):
*      Now using C structures for tables - initialised elsewhere
*-----------------------------------------------------------------------

      SUBROUTINE GEN_SET_SYMT (SYMTAB_ADDRESS, NENTRY_ADDRESS)

      IMPLICIT  NONE

*     Formal parameters

      INTEGER*4 SYMTAB_ADDRESS
      INTEGER*4 NENTRY_ADDRESS

*     Hash table
!
!      STRUCTURE /HASH_TAB/
!        CHARACTER NAME*16
!        INTEGER*4 HASHVAL
!      END STRUCTURE
!
!      RECORD /HASH_TAB/   ENTRY(0:502)
!      COMMON /HASH_TABLE/ ENTRY

*     Include files

      INCLUDE  'GEN_SYMBOLS.INC'

*     Local variables

!      INTEGER*4 I

*     Check that variables are initialized

      EXTERNAL SYMTAB_INIT

*  OK? Go..

      TABLE_ADDRESS    = SYMTAB_ADDRESS
      LENGTH_ADDRESS   = NENTRY_ADDRESS
      SYMTAB_INSTALLED = .TRUE.

*     Initialize the hash table
!     C table is initialised automatically
!      DO I = 0, 502
!        ENTRY(I).NAME    = ' '
!        ENTRY(I).HASHVAL = 0
!      END DO

      RETURN
      END
