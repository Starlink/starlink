*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*     31 July 2000 (ajc):
*        Type disagreements
*        Change to a dummy routine as it appears never to be used
*        and probably doesn't work anyway
*-----------------------------------------------------------------------

      SUBROUTINE GEN_PUT_SYMB (INSYMBOL, INTYPE, VALUE, IERR)

*  Routine intended to provide easy access to symbol table with
*  straightforward type conversion.

      IMPLICIT   NONE

*     Formal parameters

      CHARACTER INSYMBOL*(*)   ! Symbol name (caseless)
      CHARACTER INTYPE*(*)     ! Type of given VALUE
      REAL*4    VALUE          ! Value to be put (typeless)
      INTEGER*4 IERR           ! Error flag

      PRINT *, 'SUBROUTINE GEN_PUT_SYMB is a dummy routine.'
      PRINT *, 'Contact support if yousee thius message.'

      RETURN
      END
