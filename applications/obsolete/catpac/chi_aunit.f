
*+  CHI_AUNIT Calculate the units of a derived quantity
      subroutine chi_aunit(op, unit_1, unit_2, unit_r, status)
*    Description :
*     Derive the units of the result of a binary operation from
*     those of the operands and the operator itself
*    Invocation :
*     CALL CHI_AUNIT(OP, UNIT_1, UNIT_2, UNIT_R, STATUS)
*    Parameters :
*     OP=INTEGER(INPUT)
*          The operator
*     UNIT_1=CHARACTER(*)(INPUT)
*          Units of operand 1
*     UNIT_2=CHARACTER(*)(INPUT)
*          Units of operand 2
*     UNIT_R=CHARACTER(*)(OUTPUT)
*           Units of the result
*     STATUS=INTEGER(UPDATE)
*           Status variable
*    Method :
*     UC ** U1 -> UC
*     U? ** U? -> UK
*
*     UC + U?  =  U? + UC  -> U?
*     UK + U?  =  U? + UK  -> UK
*     U1 + U1  -> U1
*     U1 + U2  -> UK
*
*     - is as for +
*
*     UC * U?  =  U? * UC  -> U?
*     UK * U?  =  U? * UK  -> UK
*     U1 * U1  -> U1**2
*     U1 * U2  -> U1*U2
*
*     U? / UC  -> U?
*     U? / UK  =  UK / U?  -> UK
*     UC / U?  -> 1/U?
*     U1 / U1  -> UC
*     U1 / U2  -> U1 / U2
*    Author :
*     Esther Gershuny (RAL::EJG)
*    History :
*     2-Jul-1987: Original (RLVAD::EJG)
*    Type definitions :
      implicit none
*    Global constants :
      include 'sae_par'         ! SAI symbolic constants
      include 'chi_par'
      include 'chipar_par'
*    Import
      integer op
      character*(*) unit_1
      character*(*) unit_2
*    Export :
      character*(*) unit_r
*    Status :
      integer status
*    External references :
      logical chr_simlr
*      integer str$trim
*    Local constants :
      character*4 u_none
      parameter (u_none='NONE')
      character*7 u_nk
      parameter (u_nk='UNKNOWN')
      integer n_plus
      parameter (n_plus=CHI__OPLO+2)
      integer n_minus
      parameter (n_minus=n_plus+1)
      integer n_mul
      parameter (n_mul=n_plus+2)
      integer n_div
      parameter (n_div=n_plus+3)
      integer n_pwr
      parameter (n_pwr=n_plus+12)
*    Local variables :
      character*(CHI__SZCUNIT) unit1    ! copy of unit_1
      character*(CHI__SZCUNIT) unit2    ! copy of unit_2
      character*(CHI__SZCUNIT*2) unitx  ! copy of unit_r
      integer lu1                     ! effective length of unit_1
      integer lu2                     ! effective length of unit_2
      integer lx                      ! effective length of unitx
      integer istat                   ! temporary status value
*-
      if (status.ne.SAI__OK) then
         return
      endif
*
*   trim unit_1, unit_2
*      istat= str$trim(unit1, unit_1, lu1)
*      istat= str$trim(unit2, unit_2, lu2)
      lu1 = 0
      lu2 = 0
      if (lu1.eq.0) then
         lu1= len(u_nk)
         unit1= u_nk
      endif
      if (lu2.eq.0) then
         lu2= len(u_nk)
         unit2= u_nk
      endif
*
*   relational and logical operators
      if ( (op.ge.CHI__RELLO .and. op.le.CHI__RELHI) .or.
     :     (op.ge.CHI__LOGLO .and. op.le.CHI__LOGHI)) then
         unitx= u_none
*
*   power
      elseif (op.eq.n_pwr) then
         if (chr_simlr(unit1,u_none)) then
            unitx= u_none
         else
            unitx= u_nk
         endif
*
*   plus or minus
      elseif (op.eq.n_plus .or. op.eq.n_minus) then
         if (chr_simlr(unit1,u_none)) then
            unitx= unit2
         elseif (chr_simlr(unit2,u_none) .or.
     :           chr_simlr(unit1,unit2)) then
            unitx= unit1
         else
            unitx= u_nk
         endif
*
*   multiply
      elseif (op.eq.n_mul) then
         if (chr_simlr(unit1,u_nk) .or. chr_simlr(unit2,u_nk)) then
            unitx= u_nk
         elseif (chr_simlr(unit1,u_none)) then
            unitx= unit2
         elseif (chr_simlr(unit2,u_none)) then
            unitx= unit1
         else
            if (index(unit1,'*').eq.0 .and. index(unit1,'/').eq.0 .and.
     :          index(unit2,'*').eq.0 .and. index(unit2,'/').eq.0) then
               if (chr_simlr(unit1,unit2)) then
                  unitx= unit1 // '**2'
               else
                  unitx= unit1 // '*' // unit2
               endif
            else
               unitx= u_nk
            endif
         endif
*
*   divide
      elseif (op.eq.n_div) then
         if (chr_simlr(unit2,u_none)) then
            unitx= unit1
         elseif (chr_simlr(unit1,u_nk).or.chr_simlr(unit2,u_nk)) then
            unitx= u_nk
         elseif (chr_simlr(unit1,u_none)) then
            unitx= '1/' // unit2
         elseif (chr_simlr(unit1,unit2)) then
            unitx= u_none
         else
            if (index(unit1,'*').eq.0 .and. index(unit1,'/').eq.0 .and.
     :          index(unit2,'*').eq.0 .and. index(unit2,'/').eq.0) then
               unitx= unit1 // '/' // unit2
            else
               unitx= u_nk
            endif
         endif
      endif
*
*      istat= str$trim(unitx,unitx,lx)
       lx = 1000
      if (len(unit_r).ge.lx) then
         unit_r= unitx
      else
         unit_r= u_nk
      endif
*
      return
      end
