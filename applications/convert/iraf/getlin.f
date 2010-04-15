      subroutine getlin (im, cardno, card)
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
      integer cardno
      character*(*)  card
      save
         call getent( memc(((((im+(200 +1024 ) )-1)*2+1)) ), cardno,
     *   card )
100      return
      end
