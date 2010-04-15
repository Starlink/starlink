*+  CHI_AWTOX - Move value from work table to expression table
      subroutine chi_awtox( typtr, conptr, status)
*    Description :
*     Move numeric from the bottom of W<t>val or string from
*     Wstring onto the top of the appropriate X<t>val or Xstring
*    Invocation :
*     CALL CHI_AWTOX ( TYPTR, CONPTR, STATUS)
*    Parameters :
*     TYPTR = INTEGER(INPUT)
*          Selector of type within X<t>val & W<t>val
*     CONPTR = INTEGER(UPDATE)
*          Pointer to destination of constant
*     STATUS = INTEGER(UPDATE)
*          Status variable
*    Authors :
*     Alan Wood (STADAT::ARW) Esther Gershuny (RLVAD::EJG)
*    History :
*     12-Feb-1992: Original
*    Type definitions :
      implicit none
*    Global constants :
      include 'sae_par'
      include 'chi_par'
      include 'chipar_par'
      include 'chipar_err'
*    Import :
      integer typtr
*    Update :
      integer conptr
*    Status :
      integer status
*    Global variables :
      include 'chiwrk_cmn'
*    Local variables :
      integer l2            ! string lengths
*-
*   begin
      if (status.ne.SAI__OK) then
         return
      else
         if (conptr.lt.0) then
            status= CHI__PRSER
            return
         endif
      endif
*
*   constant
      conptr= conptr+1
      goto (101, 102, 103, 104, 105), typtr
*
*   L
101   Xlval(conptr)= Wlval(1)
      goto 200
*
*   I
102   Xival(conptr)= Wival(1)
      goto 200
*
*   R
103   Xrval(conptr)= Wrval(1)
      goto 200
*
*   D
104   Xdval(conptr)= Wdval(1)
      goto 200
*
*   C
105   Xcval(conptr)= Wcval(1)
      goto 200
*
*
200   continue
      return
      end
