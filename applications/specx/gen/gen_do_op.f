*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*        Use format I3 to read type size
*-----------------------------------------------------------------------

      SUBROUTINE gen_do_op (operator, ierr)

      IMPLICIT  none

*     Formal parameter(s):

      CHARACTER operator*2
      INTEGER*4 ierr

*     Include files

      INCLUDE 'eval_ae4.inc'

*     Local variables:

      INTEGER*4 opnd_length1
      INTEGER*4 opnd_length2
      INTEGER*4 nbytes1
      INTEGER*4 nbytes2

*     Functions:

      INTEGER*4 gen_ilen

*  Ok, go..

      ierr = 0

      IF (operator.eq.'$') THEN
D       Print *,'     resolving function call'
        CALL gen_dofunc (opnd_addr(ntopnd-1),
     &                   opnd_type(ntopnd-1),
     &                   opnd_addr(ntopnd),
     &                   opnd_type(ntopnd),
     &                   sym_address(nsymb),
     &                   ierr )
        ntopnd     = ntopnd     - 1
        nopnd(lev) = nopnd(lev) - 1
        nsymb      = nsymb      - 1

      ELSE IF (operator.eq.'@') THEN
D       Print *,'     resolving symbol array index'
        CALL gen_doarry (opnd_addr(ntopnd-1),
     &                   opnd_type(ntopnd-1),
     &                   opnd_addr(ntopnd),
     &                   opnd_type(ntopnd),
     &                   sym_address(nsymb),
     &                   ierr )
        ntopnd     = ntopnd     - 1
        nopnd(lev) = nopnd(lev) - 1
        nsymb      = nsymb      - 1

      ELSE IF (operator.eq.'%') THEN
D       Print *,'     calling gen_negate on opnd', ntopnd
        CALL gen_negate (%val(opnd_addr(ntopnd)),
     &                   opnd_type(ntopnd), ierr)

      ELSE
D       Print *,'     calling exop on opnds', ntopnd-1, ' and', ntopnd
        opnd_length1 = gen_ilen (opnd_type(ntopnd-1))
        opnd_length2 = gen_ilen (opnd_type(ntopnd))
        READ (opnd_type(ntopnd-1)(2:opnd_length1), '(I3)') nbytes1
        READ (opnd_type(ntopnd)(2:opnd_length1), '(I3)') nbytes2
        CALL gen_exop (%val(opnd_addr(ntopnd-1)),
     &                 opnd_type(ntopnd-1),
     &                 nbytes1,
     &                 %val(opnd_addr(ntopnd)),
     &                 opnd_type(ntopnd),
     &                 nbytes2,
     &                 operator,
     &                 ierr)
        nopnd(lev) = nopnd(lev) - 1
        ntopnd     = ntopnd     - 1

      END IF

      RETURN
      END
