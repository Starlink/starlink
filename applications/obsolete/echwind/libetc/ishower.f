*+  ISHOWER  -  Set up a linearly varying colour index range

      subroutine ishower(ns,ne,rs,re,gs,ge,bs,be)
c
      integer ns, ne, np, i
      real col(3), frac, rs, re, gs, ge, bs, be
c
      do np=ns,ne
         frac=float(np-ns)/(ne-ns)
         col(1)=rs+(re-rs)*frac
         col(2)=gs+(ge-gs)*frac
         col(3)=bs+(be-bs)*frac
         do i=1,3
            col(i)=max(0.3,col(i))
         enddo
         call PGSCR(np, col(1), col(2), col(3))
      enddo
c
      return
      end
