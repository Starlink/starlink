      subroutine gr_init
*+
* Name:
*    GR_INIT

* Invocation:
*    CALL GR_INIT
* Purpose:
*  To perform initialisation for graphics common blocks

* Description:
*  To perform initialisation for graphics common blocks

* Subroutine called:
*     CHR_FILL  : Fill character string with one character

* Authors:
*   T.N.Wilkins Manchester 10/11/88
*-
      implicit none
      include 'gr_inc'
      include 'gr_inc2'
      if_old = .false.
      nhplot = 0
      call zero_int(gr_slot,10)
      terminal = .false.
      hardcopy = .false.
      call chr_fill(' ',prname)
      end
