      subroutine plot_teeth(nwindows,window_pos,tram1,tram2)
*+
* Name:
*    PLOT_TEETH

* Invocation:
*    CALL PLOT_TEETH(NWINDOWS,WINDOW_POS,TRAM1,TRAM2)

* Purpose:
*  Plot locations of Dirac comb teeth as found by COMB_SPACING.

* Description:
*  Plot locations of Dirac comb teeth as found by COMB_SPACING,
*  using teeth seperation and width supplied by user, on top
*  of the plot of the current channel cut through the comb.
*
* Arguments:
*    NWINDOWS = INTEGER (Given)
*        number of windows
*    WINDOW_POS(NWINDOWS) = REAL ARRAY (Given)
*        centre of windows
*    TRAM1(NWINDOWS) = REAL ARRAY (Given)
*        left hand tram
*    TRAM2(NWINDOWS) = REAL ARRAY (Given)
*        right hand tram
*
* Subroutines referenced:
*     GR_VLINE   : Draw vertical line
*
*     GR_SPEN   : Select graphics pen
* History:
*   WINDOW_POS now real, TNW 16/6/92
*-
      implicit none
      integer nwindows
      real window_pos(nwindows)
      real tram1(nwindows)
      real tram2(nwindows)
      integer i
*
      do i =1,nwindows
        call gr_spen(2)
        call gr_vline(window_pos(i))
        call gr_spen(3)
        call gr_vline(tram1(i))
        call gr_vline(tram2(i))
      end do
      call gr_spen(1)
      end
