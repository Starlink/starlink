*+  CHI_ACONVAL - Evaluate constants in expression parse system
      subroutine chi_aconval(ed,item_type,comnt,typtr,cvalue,status)
*    Description :
*     Evaluation of constant into bottom of W<t>val array or cvalue
*    Invocation :
*     CALL CHI_ACONVAL ( ED, ITEM_TYPE, COMNT, TYPTR, CVALUE, STATUS )
*    Parameters :
*     ED = INTEGER(INPUT)
*          Element desriptor
*     ITEM_TYPE = INTEGER(INPUT)
*          Final type of constant
*     COMNT = CHARACTER(INPUT)
*          Decoding template of constant
*     TYPTR = INTEGER(OUTPUT)
*          Selector within W<t>val arrays
*     CVALUE = CHARACTER*(*)(OUTPUT)
*          Evaluated constant if a string
*     STATUS = INTEGER(UPDATE)
*          Status variable
*    Method :
*    Authors :
*     Alan Wood (STADAT::ARW) Esther Gershuny (RLVAD::EJG)
*    History :
*     4-Feb-1992: Original
*    Type definitions :
      implicit none
*    Global constants :
      include 'sae_par'     ! SAI symbolic constants
      include 'chi_par'
      include 'chipar_par'
      include 'chipar_err'
*    Import:
      integer ed
      integer item_type
      character*(*) comnt
*    Export :
      integer typtr
      character*(*) cvalue
*    Status :
      integer status
*    Global variables :
      include 'chiwrk_cmn'
*    External:
      integer chr_len
*    Local variables :
      character*40 ctest   ! temporary string constant
      integer l2           ! effective length of string constant
      integer restype      ! abs value of item_type
      integer jj,kk        ! counters
*-
*   begin
*
      if (status.ne.SAI__OK) then
         return
      endif
*
      restype= abs(item_type)
*
      typtr = item_type
*
*    simple constant with type 1 of L I R D C
      ctest = enull(ed)
      goto (1100,1200,1300,1400, 1500), typtr
      status= CHI__PRSER
      goto 3000
*
*   L
1100  call chr_ctol(ctest, Wlval(1), status)
      goto 3000
*
*   I
1200  call chr_ctoi(ctest, Wival(1), status)
      goto 3000
*
*   R
1300  call chr_ctor(ctest, Wrval(1), status)
      goto 3000
*
*   D
1400  call chr_ctod(ctest, Wdval(1), status)
      goto 3000
*
*   C
1500   continue
*      l2= index(ctest,'!')
*       print *,'ctest = ',ctest
*       print *,'l2 = ',l2
*      if (l2.gt.0) then
*         cvalue(l2:)= ' '
*      endif
*
*      cvalue = ctest(:l2) // '!'
*      print *,'cvalue = ',cvalue
      l2 = chr_len(ctest)
      Wcval(1) = ctest(:l2)
      l2 = chr_len(Wcval(1))
*
*
3000  continue
      return
      end
