      subroutine nlines (im, nlin, err)
      logical Memb(1)
      integer*2 Memc(1)
      integer*2 Mems(1)
      integer Memi(1)
      integer*4 Meml(1)
      real Memr(1)
      double precision Memd(1)
      complex Memx(1)
      equivalence (Memb, Memc, Mems, Memi, Meml, Memr, Memd, Memx)
      common /Mem/ Memd
      integer im
      integer nlin
      integer err
      integer rp
      integer ua
      save
         ua = ((((im+713 )-1)*2+1))
         nlin = 0
         rp=ua
110      if (.not.(memc(rp) .ne. 0)) goto 112
            if (.not.(memc(rp) .eq. 10)) goto 120
               nlin = nlin + 1
120         continue
111         rp=rp+1
            goto 110
112      continue
100      return
      end
