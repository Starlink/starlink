*-----------------------------------------------------------------------

      SUBROUTINE GEN_SETSYM1 (TABLE, LTAB, SYM_INDEX, IOFF, VALUE, IERR)

      IMPLICIT NONE

*     Formal parameters

      INTEGER*4 LTAB
      INTEGER*4 SYM_INDEX
      INTEGER*4 IOFF
      REAL*4    VALUE
      INTEGER*4 IERR

      STRUCTURE /SYMBOL/
        CHARACTER*16 NAME
        CHARACTER*4  TYPE
        INTEGER*4    LENGTH 
        INTEGER*4    ADDRESS
      END STRUCTURE

      INTEGER*4 MAX_TABLE
      PARAMETER (MAX_TABLE=512)

      RECORD /SYMBOL/ TABLE(MAX_TABLE)

*     Other variables

      INTEGER*4 ADDRESS
      INTEGER*4 ILT
      INTEGER*4 NBYTES

*     Functions

      INTEGER*4 GEN_ILEN

*  Ok, go...

      IERR = 0

*     Type *,'-- gen_setsym1 --'
*     Type *,'  (real) value =', VALUE

      ILT = GEN_ILEN (TABLE(SYM_INDEX).TYPE)
      READ (TABLE(SYM_INDEX).TYPE(2:ILT), '(I)') NBYTES
      ADDRESS = TABLE(SYM_INDEX).ADDRESS + (IOFF-1)*NBYTES

*     Type *,'  Entry # =', NSYMB
*     Type *,'  Length  =', NBYTES
*     Type *,'  Array element # =  ', IOFF
*     Type *,'  Writing to address ', ADDRESS

      CALL XCOPY ( NBYTES, VALUE, %VAL(ADDRESS) )

      RETURN
      END
