*-----------------------------------------------------------------------

      SUBROUTINE GEN_GETSYM1 (TABLE, LTAB, SYM_INDEX, IOFF, VALUE, IERR)

      IMPLICIT NONE

*     Formal parameters

      INTEGER*4 LTAB
      INTEGER*4 SYM_INDEX
      INTEGER*4 IOFF
      REAL*4    VALUE
      INTEGER*4 IERR          ! = 1, symbol not found

      STRUCTURE /SYMBOL/
        CHARACTER*16 NAME
        CHARACTER*4  TYPE
        INTEGER*4    LENGTH 
        INTEGER*4    ADDRESS
      END STRUCTURE

      RECORD /SYMBOL/ TABLE(512)

*     Other variables

      INTEGER*4 I
      INTEGER*4 ILT
      INTEGER*4 NBYTES
      INTEGER*4 ADDRESS

*     Functions

      INTEGER*4 GEN_ILEN

*  Ok, go...

      IERR = 0

      IF (SYM_INDEX.GT.LTAB) THEN
        IERR = 1
        RETURN
      END IF

      ILT = GEN_ILEN (TABLE(SYM_INDEX).TYPE)
      READ (TABLE(SYM_INDEX).TYPE(2:ILT), '(I)') NBYTES
      ADDRESS = TABLE(SYM_INDEX).ADDRESS + (IOFF-1)*NBYTES

      CALL XCOPY (NBYTES, %VAL(ADDRESS), VALUE)

      RETURN
      END
