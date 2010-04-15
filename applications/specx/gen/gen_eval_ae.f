*  History:
*     16 Nov 1993 (hme):
*        Replace STR$UPCASE with CHR_UCASE.
*     15 Jan 1994 (rp):
*        Replace CHR_UCASE with UUCASE
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Use format I3 to read type size
*        Trap expected IERR=1 return from GEN_OPERATOR so
*          GEN_EVAL_ALL can drop through on bad given IERR
*        Report "unevaluable" if expression fails
*      25 Jul 2004 (timj):
*        Be consistent in capitalisation of include file
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

      INCLUDE  'EVAL_AE4.INC'

*     Functions

      INTEGER*4 gen_ilen

*  Ok, go...

CD    Print *,'-- gen_eval_ae --'
CD    Print *,'  input string =', string(:60)

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

CD    Print *,'Stripped string: ', string(c1:c2)
CD    Print *,'String length = ', c2-c1+1

*     Actual parsing and evaluation of string - note that the process
*     continues until the string is exhausted, as indicated by a return
*     from gen_factor of ierr=1 or ierr=2

      next = 1

      ierr = 0

CD    Print *, 'got past ierr ='
CD    Print *, 'string(c1:c2) = ', string(c1:c2)
CD    Print *, 'length c2-c1+1 = ', c2-c1+1
CD    Print *, 'next, ierr = ', next, ierr

      CALL gen_factor (string(c1:c2), c2-c1+1, next, ierr)
      DO WHILE (ierr.eq.0)
        CALL gen_operator (string(c1:c2), c2-c1+1, next, ierr)
        IF ( ierr.eq.1 ) THEN
           ierr = 0
           GOTO 10
        END IF
        CALL gen_factor   (string(c1:c2), c2-c1+1, next, ierr)
      END DO

10    CONTINUE

CD    PRINT *,'-- gen_eval_ae --'
CD    PRINT *,'   error return from loop: ', ierr

CD    PRINT *,'   final call to eval_all: lev =', lev

      CALL gen_eval_all (ierr)
      IF (ierr.ne.0) THEN  ! error evaluating ae
!         PRINT *, '-- gen_eval_ae --'
!         PRINT *, '   expression "', STRING(C1:C2), '" not evaluable'
         RETURN
      END IF

*     Return final value to return address in desired type - give it
*     its own type and return that if no specific type demanded

      READ (opnd_type(1)(2:gen_ilen(opnd_type(1))), '(I3)') nbytes

      IF (type.eq.' ') THEN
        type = opnd_type(1)
      ELSE
        READ (type(2:gen_ilen(type)), '(I3)') nbytes2
      END IF

CD    PRINT *,'   calling gen_cvt_type with out_type = ', opnd_type(1)
      CALL gen_cvt_type (wksp(1), opnd_type(1), nbytes,
     &                   value,   type,         nbytes2, ierr)

      RETURN
      END
