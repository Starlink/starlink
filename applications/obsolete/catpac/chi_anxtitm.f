*+  CHI_ANXTITM - Stores the new item in the item lists
      subroutine chi_anxtitm (str_item, id_item, wsptr, item, status)
*    Description :
*     Adds the new item to the string and numeric item lists.
*    Invocation
*     CALL CHI_ANXTITM (STR_ITEM, ID_ITEM, WSPTR, ITEM, STATUS)
*    Parameters :
*     STR_ITEM = CHAR(INPUT)
*           New item
*     ID_ITEM = INTEGER(INPUT)
*           Id of new item
*     WSPTR = INTEGER(UPDATE)
*           Pointer into concatenated string list
*     ITEM = INTEGER(UPDATE)
*           Pointer into id & string-pointer lists
*     STATUS = INTEGER(UPDATE)
*           Status variable
*    Method :
*     Increment the pointers, checking for string & list overflow.
*     Set new item in working common string and id & pointer lists.
*    Authors :
*     Alan Wood (STADAT::ARW) and Esther Gershuny (rlvad::ejg)
*    History :
*     30-Jan-1992: Original
*    Type Definitions :
      implicit none
*    Global constants :
      include 'sae_par'                 ! SAI Symbolic Constants
      include 'chi_par'
      include 'chipar_par'
      include 'chipar_err'
*    Import :
      character*(*) str_item
      integer id_item
*    Update :
      integer wsptr
      integer item
*    Status :
      integer status
*    Global variables :
      include 'chiwrk_cmn'
*    Local variables :
      integer wsold                   ! entry value of wsptr
*-
*    Begin
*
      if (status .eq. SAI__OK) then
         wsold= wsptr
         call chi_apushc(str_item,Wstring,wsptr,CHI__SZSTK,
     :                  CHI__WSOFL,status)
            if (item .ge. CHI__MXITM) then
               status= CHI__TOOIT
            else
               item= item+1
               Worigin(item)= wsold+1
               Wqual(item)= wsold+1
               Wlist(item)= id_item
            endif
      endif
*
      return
      end
