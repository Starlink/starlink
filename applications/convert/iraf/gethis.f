      subroutine gethis (im, hlin, hislin)
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
      integer hlin
      character*(*)  hislin
      save
         call getent( memc((((im+200 +485)-1)*2+1)) , hlin, hislin )
100      return
      end
