*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*      7 Jun 2000 (ajc):
*        Use C structure for port to Linux
*-----------------------------------------------------------------------

      SUBROUTINE GEN_INQSYMB1 (TABLE, LTAB, INSYMBOL, SYM_INDEX,
     &                          TYPE, LENGTH, ADDRESS, READONLY, IERR)

      IMPLICIT NONE

*  Formal parameters

      INTEGER TABLE
      INTEGER*4 LTAB
      CHARACTER INSYMBOL*(*)
      INTEGER*4 SYM_INDEX
      CHARACTER TYPE*(*)
      INTEGER*4 LENGTH
      INTEGER*4 ADDRESS
      LOGICAL*4 READONLY
      INTEGER*4 IERR          ! = 1, symbol not found

*  Hash table

      INTEGER*4           ENTRY
      COMMON /HASH_TABLE/ ENTRY   ! Now a dummy

*  Functions

      INTEGER*4 GEN_HASHSRCH

*  Other variables

      CHARACTER SYMBOL*16
      INTEGER*4 I

      IERR = 0
      SYMBOL = INSYMBOL
      CALL UUCASE (SYMBOL)

      IF (GEN_HASHSRCH (SYMBOL(INDEX(SYMBOL,'*')+1:),
     &                  503, ENTRY, I) .NE. -1) THEN

        SYM_INDEX = I
        CALL GEN_INQSYMTYP( SYM_INDEX, TYPE, IERR )
        CALL GEN_INQSYMLEN( SYM_INDEX, LENGTH, IERR )
        CALL GEN_INQSYMADDR( SYM_INDEX, ADDRESS, IERR )
        CALL GEN_INQSYMNAM( SYM_INDEX, SYMBOL, IERR )
        READONLY  = SYMBOL(1:1) .EQ. '*'

      ELSE
        SYM_INDEX = 0
      END IF

      RETURN
      END
