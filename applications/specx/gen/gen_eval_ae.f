*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*-----------------------------------------------------------------------

      SUBROUTINE gen_eval_ae (string, type, value, ierr)

      IMPLICIT  none

*     Formal parameters

      CHARACTER string*(*)
      CHARACTER type*(*)
      INTEGER*4 value
      INTEGER*4 ierr

*     Local variables

      INTEGER*4 c1, c2
      INTEGER*4 next
      INTEGER*4 nbytes
      INTEGER*4 nbytes2
      CHARACTER outtype*4

*     Stack

      INCLUDE  'eval_ae4.inc'

*     Functions

      INTEGER*4 gen_ilen

*  Ok, go...

D     Type *,'-- gen_eval_ae --'
D     Type *,'  input string =', string(:60)

*     Type of output data requested?

      outtype = type
      CALL uucase (outtype)

*     Initialize the arithmetic-expression stacks.

      DO lev = 1, max_lev
        nopr(lev)  = 0
        nopnd(lev) = 0
      END DO

      ntopnd  = 0
      ntopr   = 0
      lev     = 1
      next_ws = 1
      nsymb   = 0

*     Prepare the expression for parsing

      CALL strip_string (string, c1, c2)

D     Type *,'Stripped string: ', string(c1:c2)
D     Type *,'String length = ', c2-c1+1

*     Actual parsing and evaluation of string - note that the process
*     continues until the string is exhausted, as indicated by a return
*     from gen_factor of ierr=1 or ierr=2

      next = 1

      ierr = 0

D     Type *, 'got past ierr ='
D     Type *, 'string(c1:c2) = ', string(c1:c2)
D     Type *, 'length c2-c1+1 = ', c2-c1+1
D     Type *, 'next, ierr = ', next, ierr

      CALL gen_factor (string(c1:c2), c2-c1+1, next, ierr)
      DO WHILE (ierr.eq.0)
        CALL gen_operator (string(c1:c2), c2-c1+1, next, ierr)
        CALL gen_factor   (string(c1:c2), c2-c1+1, next, ierr)
      END DO

      IF (ierr.eq.5) RETURN      ! undefined symbol in GEN_FACTOR

D     TYPE *,'-- gen_eval_ae --'
D     TYPE *,'   error return from loop: ', ierr

D     TYPE *,'   final call to eval_all: lev =', lev

      CALL gen_eval_all (ierr)
      IF (ierr.ne.0) RETURN

*     Return final value to return address in desired type - give it 
*     its own type and return that if no specific type demanded

      READ (opnd_type(1)(2:gen_ilen(opnd_type(1))), '(I)') nbytes

      IF (type.eq.' ') THEN
        type = opnd_type(1)
      ELSE
        READ (type(2:gen_ilen(type)), '(I)') nbytes2
      END IF

D     TYPE *,'   calling gen_cvt_type with out_type = ', opnd_type(1)
      CALL gen_cvt_type (wksp(1), opnd_type(1), nbytes,
     &                   value,   type,         nbytes2, ierr)

      RETURN
      END
