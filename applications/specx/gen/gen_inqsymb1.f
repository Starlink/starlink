*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*-----------------------------------------------------------------------

      SUBROUTINE GEN_INQSYMB1 (TABLE, LTAB, INSYMBOL, SYM_INDEX,
     &                          TYPE, LENGTH, ADDRESS, READONLY, IERR)

      IMPLICIT NONE

      INTEGER*4 MAX_TABLE
      PARAMETER (MAX_TABLE=512)

*  Formal parameters

      INTEGER*4 LTAB
      CHARACTER INSYMBOL*(*)
      INTEGER*4 SYM_INDEX
      CHARACTER TYPE*(*)
      INTEGER*4 LENGTH
      INTEGER*4 ADDRESS
      LOGICAL*4 READONLY
      INTEGER*4 IERR          ! = 1, symbol not found

      STRUCTURE /SYMBOL/
        CHARACTER*16 NAME
        CHARACTER*4  TYPE
        INTEGER*4    LENGTH 
        INTEGER*4    ADDRESS
      END STRUCTURE

      RECORD /SYMBOL/ TABLE(MAX_TABLE)

*  Hash table

      INTEGER*4           ENTRY
      COMMON /HASH_TABLE/ ENTRY

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
        TYPE      = TABLE(SYM_INDEX).TYPE
        LENGTH    = TABLE(SYM_INDEX).LENGTH
        ADDRESS   = TABLE(SYM_INDEX).ADDRESS
        READONLY  = TABLE(SYM_INDEX).NAME(1:1) .EQ. '*'
      ELSE
        SYM_INDEX = 0
      END IF

      RETURN
      END
