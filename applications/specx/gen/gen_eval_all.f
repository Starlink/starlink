
*-----------------------------------------------------------------------

      SUBROUTINE gen_eval_all (ierr)

*  If we reach the end of the expression or subexpression then we know
*  we can go ahead and execute all operators remaining on the stack

      IMPLICIT none

*     Formal parameters:

      INTEGER*4 value
      INTEGER*4 ierr

*     Operand and operator stacks

      INCLUDE  'eval_ae4.inc'

*     Local variables:

*  Ok, go..

      ierr = 0

D     Type *, '-- gen_eval_all --'
D     TYPE *, '   lev, nopr(lev), ntopr =', lev, nopr(lev), ntopr

      DO WHILE (nopr(lev).gt.0)
D       TYPE *, 'calling do_op for operator ', oper(ntopr)
        CALL gen_do_op (oper(ntopr), ierr)
        IF (ierr.ne.0) RETURN
        nopr(lev)  = nopr(lev)  - 1
        ntopr      = ntopr      - 1
D       TYPE *, 'do_op completed, new value of ntopr =', ntopr
      END DO

      RETURN
      END
