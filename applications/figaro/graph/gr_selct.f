      subroutine gr_selct(soft,status)
*+
* Name:
*    GR_SELCT

* Invocation:
*    CALL GR_SELCT(SOFT,STATUS)
* Purpose:
*   Utility routine to select graphics device.

* Description:
*   Utility routine to select graphics device.

* History:
*    T.N.Wilkins  Manchester 24/3/88
*        "        Cambridge 22/6/90 Name changed
*        "           "      25/1/91 Made a subroutine
*
* Arguments:
*   Given:
*     SOFT     (l) : If softcopy device to be opened
*   Altered
*     STATUS   (l) : Error status, 0=ok
*
*-
      implicit none
      logical soft
      integer status

      if(soft)then
        call gr_soft(status)
      else
        call gr_hard(status)
      end if
      end
