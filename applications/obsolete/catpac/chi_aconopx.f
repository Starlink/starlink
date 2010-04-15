*+  CHI_ACONOPX - Operate on constant with unary operator
      subroutine chi_aconopx (op, conptr, typtr, status)
*    Description :
*     Operate on constant in one of the X<t>val arrays with a
*     unary operator, ie NOT + or -.
*    Invocation :
*     CALL CHI_ACONOPX ( OP, CONPTR, TYPTR, STATUS)
*    Parameters :
*     OP = INTEGER(INPUT)
*          Id of the operator
*     CONPTR = INTEGER(INPUT)
*          Pointer to constant within selected array
*     TYPTR = INTEGER(UPDATE)
*          Selector within W<t>val arrays
*     STATUS = INTEGER(UPDATE)
*          Status variable
*    Method :
*     The operator changes the typtr of the operand where necessary
*    Deficiencies :
*     It is assumed that the operand is not of type C (character)
*    Authors :
*     Alan Wood (STADAT::ARW) Esther Gershuny (RLVAD::EJG)
*    History :
*     12-Feb-1992: Original
*    Type definitions :
      implicit none
*    Global constants :
      include 'SAE_PAR'     ! SAI symbolic constants
      include 'CHI_PAR'
      include 'CHIPAR_PAR'
      include 'CHIPAR_ERR'
*    Import:
      integer op
      integer conptr
*    Update :
      integer typtr
*    Status :
      integer status
*    Global variables :
      include 'CHIWRK_CMN'
      integer n_not
      parameter (n_not=CHI__UNOP)
      integer u_plus
      parameter (u_plus=CHI__UNOP+1)
      integer u_minus
      parameter (u_minus=CHI__UNOP+2)
*-
*   begin
*
      if (status.ne.SAI__OK) then
         return
      endif
*
      if (typtr.le.0 .or. typtr.gt.C_type .or. conptr.le.0) then
         status= CHI__PRSER
      elseif (typtr.eq.C_type) then
         status= CHI__ICMPT
      endif
      if (status.ne.SAI__OK) then
         return
      endif
*
*    .NOT.
      if (op.eq.n_not) then
         goto (2100,1200,1300,1400), typtr
*
*   I
1200     Xlval(conptr)= Xival(conptr)
      goto 2100
*
*   R
1300     Xlval(conptr)= Xrval(conptr)
      goto 2100
*
*   D
1400     Xlval(conptr)= Xdval(conptr)
*
*    L and the others
2100     Xlval(conptr)= .NOT. Xlval(conptr)
         typtr= L_type
*
*   unary plus or minus - force L to I
      else
         if (typtr.eq.L_type) then
            typtr= I_type
            Xival(conptr)= Xlval(conptr)
         endif
*
*   unary minus
         if (op.eq.u_minus) then
            goto (4000, 3200, 3300, 3400) typtr
*
*   I and L
3200        Xival(conptr)= -Xival(conptr)
            goto 4000
*
*   R
3300        Xrval(conptr)= -Xrval(conptr)
            goto 4000
*
*   D
3400        Xdval(conptr)= -Xdval(conptr)
*
4000        continue
         elseif (op.ne.u_plus) then
            status= CHI__PRSER
         endif
      endif
*
      return
      end
