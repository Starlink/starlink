      subroutine gr_hard(status)
*+
* Name:
*    GR_HARD

* Invocation:
*    CALL GR_HARD(STATUS)

* Purpose:
*    To open a hardcopy graphics device.

* Description:
*    To open a hardcopy graphics device. If 6 or more plots are
*    in the current file, the device is closed, the plot file
*    spooled, and the device re-opened. All variables needed are
*    passed in common. This opening and closing of files is done to
*    avoid problems with filling disks, since the files can be quiet
*    large. NHPLOT is also incremented each time this is called. This
*    also performs a frame advance.
* Arguments:
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=okay
* Global variables:
*      TERMINAL = LOGICAL (Given And Returned)
*        If softcopy workstation open
*      GREYSCALE = LOGICAL (Given And Returned)
*        If greyscale device open
*      HARDCOPY = LOGICAL (Given And Returned)
*        If hardcopy workstation open
*      NHPLOT = INTEGER (Given And Returned)
*        Number of plots in file (incremented when this
*                    routine is called, just prior to plotting.
*  Subroutines/functions referenced:
*    CLGRAP, GR_OPDEV, GR_SPEN, PGPAGE, PAR_BATCH
* Authors:
*    TNW: T.N.Wilkins 15/12/86 Manchester.
* History:
*    Modified to return error status 10/8/87 TNW
*    Call to tnw_agrest added TNW 1/12/87
*    Altered to call openhard 6/4/88 TNW
*    Change to call of OPENHARD TNW 30/6/88
*    PGPLOT version TNW Cambridge 3/90
*    Renamed GR_HARD, changed to use STATUS argument-made a subroutine
*                             T.N.Wilkins, Cambridge, 11-JAN-1991
*-
      implicit none
      integer status
      include 'SAE_PAR'
      include 'gr_inc'
      logical par_batch
*
      if(status.ne.SAI__OK) return
      if((.not.hardcopy).or.(nhplot.ge.max_hplot)) then
        call clgrap
        call gr_opdev(par_batch(),.false.,prname,status)
        nhplot = 0
        hardcopy = status.eq.SAI__OK
      else
        call pgpage
      end if
      if(.not.hardcopy) status = SAI__OK
      call gr_spen(1)
      nhplot = nhplot + 1
      end
