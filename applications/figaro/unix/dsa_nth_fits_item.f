*+
*   D S A _ N T H _ F I T S _ I T E M
*
      subroutine dsa_nth_fits_item(refnam,nth,exist,item,access,el,slen
     :  ,status)
*
*  Description:
*
*
*  Purpose:
*
*
*  Arguments:
*      REFNAM = CHARACTER*4 (Given)
*        ref name
*      NTH = INTEGER (Given)
*        item
*      STATUS = INTEGER (Given and returned)
*        status
*      EXIST = LOGICAL (Returned)
*        if exists
*      ITEM = CHARACTER*(*) (Returned)
*        item name
*      ACCESS = CHARACTER*(*) (Returned)
*        access
*      EL = INTEGER (Returned)
*        element
*      SLEN = INTEGER (Returned)
*        string length
*  Subroutines/functions referenced:
*
*  Author:
*    T.N.Wilkins, Cambridge, 25-JUL-1991
*    A.J.Holloway, Manchester, 4-Dec-1997
*  History:
*    AJH Set refnam to be 4 characters
*-
      implicit none
      character*4 refnam
      integer nth
      logical exist
      character*(*) item
      character*(*) access
      integer el
      integer slen
      integer status,dstatus

*

      call dta_nmvar(refnam//'.fits',nth,item,dstatus)
      exist = dstatus.eq.0
      if(exist) then
        call dsa_seek_fits(refnam,item,exist,access,el,slen,status)
      end if
      end
