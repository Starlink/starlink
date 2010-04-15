*+  CHI_APRECOP - Calculates operator precedence
      integer function chi_aprecop (op_num , status)
*    Description :
*     Returns the precedence of the given operator.
*    Invocation
*     INTEGER CHI_APRECOP
*    Parameters :
*     OPNUM = INTEGER(INPUT)
*           Numeric id of operator
*     STATUS = INTEGER(UPDATE)
*           Status value
*    Method :
*      Lookup table indexed by operator number
*    Deficiencies :
*     <description of any deficiencies>
*    Bugs :
*     <description of any "bugs" which have not been fixed>
*    Authors :
*     Esther Gershuny (rlvad::ejg)
*    History :
*     3-Sep-1986: Original (rlvs::ejg)
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      include 'sae_par'
      include 'chipar_par'
      include 'chipar1_par'
      include 'chi_par'
      include 'chipar_err'
*    Import
      integer op_num
*    Status :
      INTEGER STATUS
*-
      chi_aprecop= 0
      if (op_num.ge.chi__oplo .and. op_num.le.chi__ophi) then
          chi_aprecop= op_prec(op_num)
      elseif (op_num.gt.chi__fbase .and.
     :        op_num-chi__fbase.le.chi__mxfun ) then
          chi_aprecop= chi__fprec
      else
          status= chi__ivopr
      endif
*
      return
      end
