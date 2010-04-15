*+  CHI_APELM - Parse expression as an element or a constant.
      subroutine chi_apelm( expression, ed, status)
*    Description :
*     Parse an expression as an element or a constant.
*    Invocation
*     CALL CHI_APELM( EXPRESSION, ED, STATUS)
*    Parameters :
*     EXPRESSION=CHARACTER(INPUT)
*           The element expression (fieldname)
*     ED=INTEGER(OUTPUT)
*           The element descriptor
*     STATUS=INTEGER(UPDATE)
*           Status variable
*    Method :
*     Look up the field name in the catalogue.
*     If it does not exist then
*        Look up the element in the other catalogues that are open
*        assuming that the fieldname is prefixed
*        If it does not exist then
*           Get a free element descriptor from the top of table
*           Remove any units (delimited in square brackets [])
*           Decode the string as a FORTRAN constant
*           Define the expression as a temporary parameter
*        Endif
*     Endif
*    Authors :
*     Jon Fairclough (RAL::IPMAF)
*     Esther Gurney (IPMAF::EJG)
*     Alan Wood (STADAT::ARW)
*    History :
*     2-Feb-1992: Original
*    Type Definitions :
      implicit none
*    Global constants :
      include 'sae_par'                 ! SAI Symbolic Constants
      include 'chi_par'
      include 'chipar_par'
      include 'chipar1_par'
      include 'chipar_err'
*    Import
      character*(*) expression
*    Export
      integer ed
*    Status :
      integer status
*    External references
      integer chr_len
      integer chi_atype
*    Global variables
      include 'chiwrk_cmn'
*    Local
      integer width
      integer l1, l2
      logical not_found
      integer i
      integer Enamelen
      integer Cname_len
      character*(CHI__SZTYP) ctype
      character*(CHI__SZCNAME) fname1, fname2
*-
*    Begin
*
      if (status .ne. SAI__OK) then
         return
      endif
*
*    Look up the element name in the common area initialized by CHI_AINITPAR
*
      ed = 0
      do i = 1, enitems
        Enamelen = chr_len(Ename(i))
        if (expression(1:Enamelen) .eq. Ename(i)) then
          ed = i
        endif
      enddo
*
      if (ed .eq. 0) then
*    Define the string as a constant
*             Initialise element
      Etype(ed) = 0
*
      Ename(ed) = ' '
      Eformt(ed) = ' '
      Eunit(ed) = ' '
      Eformt(ed) = ' '
      Enull(ed) = ' '
      Ecomnt(ed) = ' '
*
         enitems = enitems + 1
         ed = enitems
         l1 = chr_len (expression)
         Eunit(ed) = ' '
*             Decode the string
         call chi_adecode(expression(:l1), ctype, width,
     :                         Eformt(ed), Enull(ed), status)
         Etype(ed)= chi_atype(ctype,status)
         Ename(ed) = expression
*         Eformt(ed) = Enfrmt(ed)
         Ecomnt(ed) = 'CONSTANT'  ! Unique tag ?
      endif
*
      end
