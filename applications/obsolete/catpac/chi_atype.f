*+  CHI_ATYPE - Convert type code to numeric
      integer function chi_atype(typecode)
*    Description :
*     Convert character typecodes L,I,R,D,C,P,O,Q,T,A to
*             numeric             1,2,3,4,5,6,7,8,9,10.
*    Invocation :
*     INTEGER CHI_ATYPE
*    Parameters :
*     TYPECODE = CHAR(INPUT)
*              Type as 'L','I','R','D','C','P','O','Q','T','A'
*    Method :
*     Use ASCII value
*    Limitations :
*     Will not work with non-ASCII character coding
*    Author :
*     Alan Wood (STADAT::ARW) Esther Gershuny (RAL::EJG)
*    History :
*     25-Feb-1992: Original
*    Type definitions :
      implicit none
*    Global constants :
      include 'chipar_par'
*    Import :
      character*(CHI__SZTYP) typecode
*    Local constants :
      integer typarr(0:9)
      data typarr/9,           ! T = time
     : 10,                     ! A = angle
     :  1,                     ! L = logical
     :  5,                     ! C = character
     :  4,                     ! D = double
     :  7,                     ! O = RA-like quantity in radians
     :  6,                     ! P = radians
     :  8,                     ! Q = DEC-like quantity in radians
     :  3,                     ! R = real
     :  2/                     ! I = integer
*    Local variables :
      integer icode            ! position in alphabet
      integer ntype            ! result
*-
      icode = 0
      ntype = 0

      icode = ichar(typecode) - ichar('A') + 1
      ntype = typarr(mod(icode,10))
      chi_atype = ntype
      return
      end
