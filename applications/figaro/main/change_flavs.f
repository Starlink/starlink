      subroutine change_flavs(cur_flav,cur_plot,status)
*+
* Name:
*    CHANGE_FLAVS

* Invocation:
*    CALL CHANGE_FLAVS(CUR_FLAV,CUR_PLOT,STATUS)

* Purpose:
*   Alter the current flavours.

* Description:
*   A table giving the current flavour settings is
*   printed on entry. Specifing a flavour then
*   flips its setting i.e if ON then it is set to OFF
*   and vice versa.
*   On exit the revised table is printed.
*
* Arguments:
*    CUR_FLAV(MAX_FLAVS) = LOGICAL ARRAY (Given and returned)
*        flavour status
*    CUR_PLOT(MAX_PLOTOPT) = LOGICAL ARRAY (Given and returned)
*        plot flavour status
*    STATUS = INTEGER (Given and returned)
*        Error status

* Authors:
*   TNW: T.N.Wilkins, Cambridge until 9/92, then Durham
* History:
* TNW: 16/11/89 Call to QMENU moved from flavours_menu
* TNW: 16/8/91 All info output via QMENU
* TNW: 4/2/94 Set ifsoft and showvel here
* TNW: 17/3/94 Use qcheck
* JWP: March 97 Removed SHAPE and STATIC options
*-
      implicit none
      integer DIM_FLAVS, MAX_PLOTOPT
      parameter (DIM_FLAVS = 11, MAX_PLOTOPT = 5)
      logical cur_flav(DIM_FLAVS),cur_plot(MAX_PLOTOPT)
      integer status

* menu pick

      integer iopt

* do loop

      integer OPT_PLOT
      parameter (OPT_PLOT = 4)
      real dumr
      character dumc
      integer NDICT
      parameter (NDICT = DIM_FLAVS+1)
      character*46 dict(NDICT)
      character*33 dictp(MAX_PLOTOPT+1)
      data dict/
     :     'L HARDCOPY : Plots of profile fits +',
     :     'L TABLE    : Table of profile parameters',
     :     'L PLOT     : Plot rotation curves *+',
     :     'L PRINT    : Print rotation curves',
     :     'L RATIO    : Output line ratios (with plots)',
     :     'L GREY     : Greyscale plot of velocity',
     :     'L CONTOUR  : Contour plot of velocity *',
     :     'L FULL     : Large table',
     :     'L CHECK    : Profile array *',
     :     'L SOFT     : Plot in softcopy (* above)',
     :     'L VELOCITY : Use velocity scale (+ above)',
     :     'Q EXIT     : Exit menu'/
      data dictp/
     :     'L VELOCITY : Radial velocities',
     :     'L WIDTH    : Widths',
     :     'L FLUX     : Fluxes',
     :     'L AVERAGE  : Average velocities',
     :     'L ALL      : Velocities on 1 plot',
     :     'Q EXIT     : Exit menu'/
* ------------------------------------------------------------------

* output the menu

      call qcheck('Output Options',dict,NDICT,dumr,dumc,cur_flav,iopt
     :     ,status)

* If the user selects the PLOT option, then the various "sub-options"
* may require changing

      if(cur_flav(OPT_PLOT)) then

* output the menu

         call qcheck('Plot Options',dictp,MAX_PLOTOPT+1,dumr,dumc
     :        ,cur_plot,iopt,status)
      endif
      end
