      subroutine plot_curpos(dx, dy, nbut, buttons, cursor_present)

*   Get cursor position when a key is pressed

      integer dx, dy, nbut, buttons(nbut), i
      integer pgcurse, cursor_present
      character ch
      real x,y
      save x,y

      do i =1,nbut
         buttons(i) = 0
      end do

      cursor_present = pgcurse(x,y,ch)
      if (cursor_present.eq.1) then
*  these are the 3 buttons on the mouse on a SPARCstation under Xwindows
         if (ch .eq. 'A') buttons(1) = 1
         if (ch .eq. 'D') buttons(2) = 1
         if (ch .eq. 'X') buttons(3) = 1
*  use the keys 1..3 to simulate the 3 mouse buttons
         if ((ch .ge. '1') .and. (ch .le. '3'))
     :     buttons(ichar(ch) - 48) = 1

         dx = nint(x)
         dy = nint(y)
      endif

      end
