*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Use format I3 to read type size
*      25 Jul 2004 (timj):
*        Be consistent in capitalisation of include file
*-----------------------------------------------------------------------

      SUBROUTINE gen_eval_str (string1, string2, ierr)

      IMPLICIT  none

*     Formal parameters

      CHARACTER string1*(*)
      CHARACTER string2*(*)
      INTEGER*4 ierr

*     Include files

      INCLUDE  'GEN_SYMBOLS.INC'
      INCLUDE  'CNF_PAR'

*     Local variables

      LOGICAL*4 readonly
      INTEGER*4 sym_index
      INTEGER*4 length
      INTEGER*4 address
      CHARACTER type*4

      INTEGER*4 nbytes
      INTEGER*4 c1, c2
      INTEGER*4 ils

*     Functions:

      INTEGER*4 gen_ilen

*  Ok, go...

CD    Print *,'-- gen_eval_str --'
CD    Print *,'  input string  = ', string1(:60)

      ierr = 0
      ils  = len (string2)

*     Check if it is a symbol (if symbol table installed)

      CALL strip_string (string1, c1, c2)
      IF (symtab_installed) THEN
        CALL gen_inqsymb  (string1, sym_index, type, length,
     &                     address, readonly, ierr)
      ELSE
        ierr = 1
      END IF

*     If returned type is not Cnnn or error set then interpret string
*     as a string constant.

      IF (sym_index.eq.0 .OR. type(1:1).ne.'C' .OR. ierr.ne.0) THEN
        string2 = string1 // ' '
        ierr    = 0
      ELSE
        READ (type(2:gen_ilen(type)), '(I3)') nbytes
        CALL xcopy (MIN (nbytes,ils), %val(cnf_pval(address)), 
     :              %ref(string2))
      END IF

CD    Print *,'  return string = ', string2(:60)

      RETURN
      END
