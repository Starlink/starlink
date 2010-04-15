*+  CHI_AMATCH - Operand type match for expression parse system
      subroutine chi_amatch (op, type1, type2, optype,
     :                          comnt1, comnt2, opcomnt, status)
*    Description :
*     Matches operand types for binary operators.
*    Invocation :
*     CALL CHI_AMATCH ( OP, TYPE1, TYPE2, OPTYPE,
*    :                     COMNT1, COMNT2, OPCOMNT, STATUS )
*    Parameters :
*     OP = INTEGER(INPUT)
*          Operator id
*     TYPE1 = INTEGER(INPUT)
*          Type of 1st operand
*     TYPE2 = INTEGER(INPUT)
*          Type of 2nd operand
*     OPTYPE = INTEGER(OUTPUT)
*          Type of result
*     COMNT1 = CHARACTER(UPDATE)
*          Comment field of 1st operand
*     COMNT2 = CHARACTER(UPDATE)
*          Comment field of 2nd operand
*     OPCOMNT = CHARACTER(OUTPUT)
*          Comment field of result
*     STATUS = INTEGER(UPDATE)
*          Status variable
*    Method :
*     Routine uses types L I R D C
*     Type of constant is +ve if explicitly typed, else -ve
*     Type of non-constant is +ve
*     if op is + or - or relational then
*       if abs(one type) is O and abs(other) is Q then
*         optype= P
*       else
*         optype= max ( abs(type1), abs(type2))
*         if (optype.eq.C .and. .not.relop) then
*           status= CHI__ICMPT
*         endif
*       endif
*       if both operands are CONSTANT then
*           opcomnt= 'CONSTANT'
*       elseif optype is one of L I R D C then
*         opcomnt= ' '
*       elseif one of operands is constant then
*         opcomnt= comnt of the other
*         comnt of constant operand = comnt of the other
*         if (optype.eq.P .and. opcomnt is blank) then
*           status= CHI__IVCON
*         endif
*       elseif comnt1 is blank then
*         opcomnt= comnt2
*       else
*         opcomnt= comnt1
*       endif
*     else
*       if operator is * and operand 1 is constant then
*         opcomnt= comnt2
*         optype= max ( abs(type1), abs(type2) )
*       elseif operator is * or / and operand 2 is constant then
*         opcomnt= comnt1
*         optype= max ( abs(type1), abs(type2) )
*       else
*         if at least one operand has type one of P O Q then
*           optype= D
*         else
*           optype= max ( abs(type1), abs(type2) )
*         endif
*       endif
*       if (optype.eq.C .xor. op.eq.n_cat) then
*        status= CHI__ICMPT
*       endif
*       if optype is one of L I R D C then
*         if both operands are CONSTANT then
*           opcomnt= 'CONSTANT'
*         else
*           opcomnt= ' '
*         endif
*       endif
*     endif
*    Authors :
*     Alan Wood (STADAT::ARW)
*     Esther Gershuny (RLVAD::EJG)
*    History :
*     9-Feb-1992: Original
*    Deficiencies :
*     L type is not forced from logical operator
*    Type definitions :
      implicit none
*    Global constants :
      include 'sae_par'     ! SAI symbolic constants
      include 'chi_par'
      include 'chipar_par'
      include 'chipar_err'
*    Import:
      integer op
      integer type1
      integer type2
      character*(*) comnt1
      character*(*) comnt2
*    Export :
      integer optype
      character*(*) opcomnt
*    Status :
      integer status
*    External :
      logical chr_simlr
*    Local constants :
      integer n_plus       ! id of + operator
      parameter(n_plus=CHI__OPLO+2)
      integer n_minus      ! id of - operator
      parameter(n_minus=CHI__OPLO+3)
      integer n_mul        ! id of * operator
      parameter(n_mul=CHI__OPLO+4)
      integer n_div        ! id of / operator
      parameter(n_div=CHI__OPLO+5)
      integer n_cat        ! id of // operator
      parameter(n_cat=CHI__OPLO+15)
*    Local variables :
      logical relop       ! .true. if operator relational
      logical radop       ! .true. if operator preserves special units
      integer thi         ! higher of abs(type1), abs(type2)
      integer tlo         ! lower of abs(type1), abs(type2)
*-
      if (status.ne.SAI__OK) then
        return
      endif
*
*   begin
*
      relop= op.ge.CHI__RELLO .and. op.le.CHI__RELHI
      radop= relop .or. op.eq.n_plus .or. op.eq.n_minus
      thi= max(abs(type1),abs(type2))
      tlo= min(abs(type1),abs(type2))
*
*   operator preserving special type, ie + - or relational
*
      if (radop) then
        optype= thi
        if (optype.eq.C_type .and. .not.relop) then
          status= CHI__ICMPT
        endif
        if ( chr_simlr(comnt1, comnt2) .and.
     :       chr_simlr(comnt1(:8), 'CONSTANT') ) then
          opcomnt= 'CONSTANT'
        elseif (optype.le.C_type) then
          opcomnt= ' '
        endif
*
*   other operator, ie * / ** // or logical
*
      else
        if ( chr_simlr(comnt1(:8), 'CONSTANT') .and. op.eq.n_mul) then
          optype= thi
          opcomnt= comnt2
        elseif ( chr_simlr(comnt2(:8), 'CONSTANT') .and.
     :         (op.eq.n_mul .or. op.eq.n_div)) then
          optype= thi
          opcomnt= comnt1
        else
          optype= thi
        endif
        if (optype.eq.C_type .xor. op.eq.n_cat) then
          status= CHI__ICMPT
        endif
        if (optype.le.D_type) then
          if ( chr_simlr(comnt1, comnt2) .and.
     :         chr_simlr(comnt1(:8), 'CONSTANT') ) then
            opcomnt= 'CONSTANT'
          else
            opcomnt= ' '
          endif
        endif
      endif
*
      return
      end
