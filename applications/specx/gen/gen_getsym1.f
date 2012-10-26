*  History:
*      7 Jun 2000 (ajc):
*        Use C structure for port to Linux
*        Use format I3 to read type size
*-----------------------------------------------------------------------

      SUBROUTINE GEN_GETSYM1 (TABLE, LTAB, SYM_INDEX, IOFF, VALUE, IERR)

      IMPLICIT NONE
      INCLUDE 'CNF_PAR'

*     Formal parameters

      INTEGER TABLE           ! Dummy
      INTEGER*4 LTAB
      INTEGER*4 SYM_INDEX
      INTEGER*4 IOFF
      REAL*4    VALUE
      INTEGER*4 IERR          ! = 1, symbol not found

*     Other variables

      INTEGER*4 ILT
      INTEGER*4 NBYTES
      INTEGER*4 ADDRESS
      CHARACTER*4  TYPE

*     Functions

      INTEGER*4 GEN_ILEN

*  Ok, go...

      IERR = 0

      IF (SYM_INDEX.GT.LTAB) THEN
        IERR = 1
        RETURN
      END IF

      CALL GEN_INQSYMTYP( SYM_INDEX, TYPE, IERR )
      CALL GEN_INQSYMADDR( SYM_INDEX, ADDRESS, IERR )

      ILT = GEN_ILEN (TYPE)
      READ (TYPE(2:ILT), '(I3)') NBYTES
      ADDRESS = ADDRESS + (IOFF-1)*NBYTES

      CALL XCOPY (NBYTES, %VAL(CNF_PVAL(ADDRESS)), VALUE)

      RETURN
      END
