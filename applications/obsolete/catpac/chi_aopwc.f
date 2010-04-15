*+  CHI_AOPWC - Perform work stack operation on C operands
      subroutine chi_aopwc(op, nextype, wptr, wstop, status)
*    Description :
*     Perform simple unary and binary operations on operand(s) in
*     Wival and puts the answer in the appropriate work stack
*    Invocation :
*     CALL CHI_AOPWC ( OP, NEXTYPE, WPTR, WSTOP, STATUS )
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
      integer chr_len
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
*    Local variables :
      logical error                 ! .true. if arithmetic error
      integer resptr                ! index of result in W<t>val
      integer branch                ! code selector
      integer strsize               ! effective length of cval
      character*(chi__szcval) cval             ! string equivalent of result
      integer size
      integer size1
      character*(chi__szcval) cval1
      character*(chi__szcval) cvaltemp
*-
      if (status.ne.SAI__OK) then
         return
      endif
*
      if (op.ge.CHI__UNOP) goto 910
*
*   top-of-stack
      resptr= wptr
      cval = Wcval(resptr)
      size = chr_len(cval)
*
*   conversion only
      if (op.eq.no_op) goto 280
*
*   next-to-top
      resptr= wptr-1
      cval1 = Wcval(resptr)
      size1 = chr_len(cval1)
*   branch on operator number
      branch= op - CHI__RELLO + 1
      if (branch.le.0) goto 910
*   (9:14)  <   {   =   }   >   #
      goto (309,310,311,312,313,314,
*   (15)    **
     :      910,
*   (16)    //
     :      216), branch
*   (23:25) .or. .xor. .and.
      goto 910
*
*   //
216   cval= cval1(:size1)//cval(:size)
      size= size+size1
*
*   convert from C_type
280   continue
      goto 500
*   to C
*285   continue
*      cval= cval // '!'
*      wcval(resptr) = cval
*      goto 500
*
*   relational operators
309   Wlval(resptr)= cval1(:size1) .LT. cval(:size)
      goto 350
310   Wlval(resptr)= cval1(:size1) .LE. cval(:size)
      goto 350
311   Wlval(resptr)= cval1(:size1) .EQ. cval(:size)
      goto 350
312   Wlval(resptr)= cval1(:size1) .GE. cval(:size)
      goto 350
313   Wlval(resptr)= cval1(:size1) .GT. cval(:size)
      goto 350
314   Wlval(resptr)= cval1(:size1) .NE. cval(:size)
*
*   convert from L_type
350   continue
*   normal return
500   wptr= resptr
*
550   continue
*
      return
*
*   ( , )  ie not genuine ops - this would be coding error
*900   status= CHI__NTSUP
*      goto 550
*
*   op not permissible on this operand type
910   status= CHI__ICMPT
      goto 550
*
      end
