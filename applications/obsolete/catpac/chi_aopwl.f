*+  CHI_AOPWL - Perform work stack operation on L operands
      subroutine chi_aopwl(op, nextype, wptr, wstop, status)
*    Description :
*     Perform simple unary and binary operations on operand(s) in
*     Wival and puts the answer in the appropriate work stack
*    Invocation :
*     CALL CHI_AOPWL ( OP, NEXTYPE, WPTR, WSTOP, STATUS )
*    Parameters :
*     OP = INTEGER(INPUT)
*           Operator id
*     NEXTYPE = INTEGER(INPUT)
*           Final result type
*     WPTR = INTEGER(UPDATE)
*           Pointer into W<t>val arrays
*     WSTOP = INTEGER(UPDATE)
*           Pointer into Wstring
*     STATUS = INTEGER(UPDATE)
*           Status
*    Method :
*     Take top item or top and next_to_top items on Wival and
*     perform the operation, returning the result into the right
*     position in the Wival or Wlval stack.    Then convert the
*     result into the type required for the next operation.
*    Authors :
*     Alan Wood (STADAT::ARW) Esther Gershuny (rlvad::ejg)
*    History :
*     28-Feb-1992 - Original
*    Type definitions :
      implicit none
*    Global constants :
      include 'sae_par'          ! SAI Symbolic constants
      include 'chi_par'
      include 'chipar_par'
      include 'chipar1_par'
      include 'chipar_err'
*    Import :
      integer op
      integer nextype
*    Update :
      integer wptr
      integer wstop
*    Status :
      integer status
*    External :
*    Global variables :
      include 'chiwrk_cmn'
*    Local constants :
      integer no_op
      parameter (no_op=CHI__OPLO)
      integer u_plus
      parameter (u_plus=CHI__UNOP+1)
      integer u_minus
      parameter (u_minus=CHI__UNOP+2)
      integer n_plus
      parameter (n_plus=CHI__OPLO+2)
      integer n_not
      parameter (n_not=CHI__OPLO+2)
*    Local variables :
      logical error                 ! .true. if arithmetic error
      integer resptr                ! index of result in W<t>val
      integer branch                ! code selector
      integer strsize               ! effective length of cval
      character*25 cval             ! string equivalent of result
*-
      if (status.ne.SAI__OK) then
         return
      endif
*
      resptr= wptr
*  conversion only
      if (op.eq.no_op) goto 450
      if (op.lt.CHI__UNOP) goto 200
*
*   unary operator - ought only to be .NOT.
      if (op.ne.n_not) then
         goto 910
      else
         Wlval(resptr)= .not. Wlval(resptr)
      endif
      goto 500
*
*   binary operator
200   resptr= wptr-1
      branch= op - CHI__LOGLO + 1
*   (1:22)   all non-logical ops
      if (branch.le.0) goto 910
*   (23:25)  OR XOR AND
      goto (423,424,425), branch
      goto 900
*
*   logical operators
423   Wlval(resptr)= Wlval(resptr) .OR. Wlval(wptr)
      goto 450
424   Wlval(resptr)= Wlval(resptr) .XOR. Wlval(wptr)
      goto 450
425   Wlval(resptr)= Wlval(resptr) .AND. Wlval(wptr)
*
450   continue
*
*   normal return
*
500   wptr= resptr
*
*
550   continue
*
      return
*
*   ( , )  ie not genuine ops - this would be coding error
900   status= CHI__NTSUP
      goto 550
*
*   op not permissible on this operand type
910   status= CHI__ICMPT
      goto 550
*
      end
