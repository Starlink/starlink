      subroutine line_show(tram,segbounds,line_count)
*+
* Name:
*    LINE_SHOW

* Invocation:
*    CALL LINE_SHOW(TRAM,SEGBOUNDS,LINE_COUNT)

* Purpose:
*   Show tram lines of located lines

* Description:
*      This plots the tram line positions already found onto the current
*    plot as appropriate.
*
* Subroutines referenced:
*     GR_VLINE    : Draw virtical line on plot
*     GR_SPEN     : Select graphics pen
* Arguments:
*    TRAM(LINE_COUNT) = REAL ARRAY (Given)
*        Tram to plot
*    SEGBOUNDS(2) = REAL ARRAY (Given)
*        Boundaries of segment
*    LINE_COUNT = INTEGER (Given)
*        Number of lines
*-
      implicit none
      integer i
      integer line_count
      real segbounds(2)
      real tram(line_count)

      call gr_spen(3)
      do i=1,line_count
         if ((tram(i).gt.segbounds(1)).and.(tram(i).lt.segbounds(2)))
     :        then
            call gr_vline(tram(i))
         end if
      end do
      call gr_spen(1)
      end
