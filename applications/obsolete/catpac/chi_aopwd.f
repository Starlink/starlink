
*+  CHI_AOPWD - Perform work stack operation on D operands
      subroutine chi_aopwd(op, nextype, wptr, wstop, status)
*    Description :
*     Perform simple unary and binary operations on operand(s) in
*     Wival and puts the answer in the appropriate work stack
*    Invocation :
*     CALL CHI_AOPWD ( OP, NEXTYPE, WPTR, WSTOP, STATUS )
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
*
      resptr= wptr
*  conversion only
      if (op.eq.no_op) goto 250
      if (op.lt.CHI__UNOP) goto 200
*
*   unary operator - ought only to be + or -
      if (op.lt.u_plus .or. op.gt.u_minus) then
         goto 910
      else
         if (op.eq.u_minus) then
            Wdval(resptr)= -Wdval(resptr)
         endif
      endif
      goto 250
*
*   binary operator
200   resptr= wptr-1
      branch= op - n_plus + 1
*   (1:2)   ? (
      if (branch.le.0) goto 900
*   (3:6)   +   -   *   /
      goto (203,204,205,206,
*   (7:8)   ,   )
     :      900,900,
*   (9:14)  <   {   =   }   >   #
     :      309,310,311,312,313,314,
*   (15)    **
     :      215), branch
*   (16,23:25)  //  .or. .xor. .and.
      goto 910
*
*   arithmetic operators
203   Wdval(resptr)= Wdval(resptr) + Wdval(wptr)
      goto 250
204   Wdval(resptr)= Wdval(resptr) - Wdval(wptr)
      goto 250
205   Wdval(resptr)= Wdval(resptr) * Wdval(wptr)
      goto 250
206   Wdval(resptr)= Wdval(resptr) / Wdval(wptr)
      goto 250
215   Wdval(resptr)= Wdval(resptr) ** Wdval(wptr)
*
*   convert from R_type
250   continue
      goto (251,500,253,254,255), nextype
*   to L
251   continue
      goto 500
*   to I
253   Wival(resptr)= int(Wdval(resptr))
      goto 500
*   to R
254   Wrval(resptr)= Wdval(resptr)
      goto 500
*   to C
255   continue
      goto 500
*
*   relational operators
*
309   Wlval(resptr)= Wdval(resptr) .LT. Wdval(wptr)
      goto 350
310   Wlval(resptr)= Wdval(resptr) .LE. Wdval(wptr)
      goto 350
311   Wlval(resptr)= Wdval(resptr) .EQ. Wdval(wptr)
      goto 350
312   Wlval(resptr)= Wdval(resptr) .GE. Wdval(wptr)
      goto 350
313   Wlval(resptr)= Wdval(resptr) .GT. Wdval(wptr)
      goto 350
314   Wlval(resptr)= Wdval(resptr) .NE. Wdval(wptr)
*
*   convert from L_type
350   continue
*
*   normal return
*
500   wptr= resptr
*
550   continue
*
      return
900   status= CHI__NTSUP
      goto 550
*
*   op not permissible on this operand type
910   status= CHI__ICMPT
      goto 550
*
      end
