      subroutine gr_list
*+
* Name:
*    GR_LIST

* Invocation:
*    CALL GR_LIST
*
* Description:
*    To list the available graphics devices. Version for GNS.
*
* Purpose:
*    To list the available graphics devices. Version for GNS.
*
* Arguments:
*    Given:
*    Altered:
*    Returned:
*    Workspace:
*  Subroutines/functions referenced:
*
* Author:
*    T.N.Wilkins, Cambridge, 25-SEP-1991, extracted from GR_OPDEV.
* History:
*-
      implicit none

*

      integer contxt
      include 'GNS_PAR'
      character*(gns__sznam) chars1
      character*(gns__szdes) chars2
      logical gns_filtg,go
      external gns_filtg
      integer len1,status

      call par_wruser('Available graphics devices',status)
      contxt = 0
      go = .true.
      do while(go)
        call gns_gwng(gns_filtg,contxt,chars1,chars2,len1,status)
        go = contxt.ne.0
        call par_wruser(chars1//chars2(:len1),status)
      end do
      end
