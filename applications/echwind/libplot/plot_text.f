      subroutine plot_text(text, x, y, colind)

      integer colind, x, y
      character*(*) text

      call pgsci(colind)
      call pgtext(float(x),float(y),text)

      return
      end
