      subroutine gr_soft(status)
*+
* Name:
*    GR_SOFT

* Invocation:
*    CALL GR_SOFT(STATUS)
*
* Description:
*    To open a softcopy graphics device. The display surface is
*    cleared.
*
* Purpose:
*    To open a softcopy graphics device.
*
* Arguments:
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=okay
* Global variables:
*      HARDCOPY = LOGICAL (Given and returned)
*        If hardcopy device open (gr_inc)
*      GREYSCALE = LOGICAL (Given and returned)
*        If greyscale device open (gr_inc)
*      TERMINAL = LOGICAL (Given and returned)
*        If softcopy device open (gr_inc)
*      BLANK_PEN = LOGICAL (Given and returned)
*        If a blank pen is defined (gr_inc)
* Subroutines/functions referenced:
*    CLGRAP, GR_OPDEV, GR_SPEN, PGPAGE, PGQCOL
* Author:
*    T.N.Wilkins 3/3/87 Manchester.
* History:
*    ERASE parameter introduced 8/4/87
*    Return of error status 10/8/87
*    Removal of DIATP 9/11/87
*    Changed to call OPENSOFT 6/4/88
*    PGPLOT version 3/90
*    ERASE parameter removed, 12/9/90
*    Renamed GR_SOFT, changed to use STATUS argument-made a subroutine
*                             T.N.Wilkins, Cambridge, 11-JAN-1991
*-
      implicit none
      integer status
      include 'SAE_PAR'
      include 'gr_inc'
      integer li,hi

      if(status.ne.SAI__OK) return
      if(terminal) then
        call pgpage
      else

*     Close graphics-another device might be open. This doesn't do
*     anything if no device is open

        call clgrap
        status=0
        call gr_opdev(.false.,.true.,prname,status)

*   Check if opened ok

        terminal = status.eq.SAI__OK

*   See if pen 0 is available

        call pgqcol(li,hi)
        blank_pen = li.eq.0
      end if
      if(.not.terminal) status = SAI__ERROR
      call gr_spen(1)
      end
