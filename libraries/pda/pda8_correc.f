c
c
      double precision function pda8_correc(i,n)
c
c     calculates correction for tail area of the i-th largest of n
c     order statistics.
c
      implicit none
      double precision c1(7),c2(7),c3(7), mic, c14
      integer*8 i
      integer*8 n, an
      data c1/9.5d0, 28.7d0, 1.9d0, 0.0d0, -7.0d0, -6.2d0, -1.6d0/,
     :     c2/-6195.0d0, -9569.0d0, -6728.0d0, -17614.0d0, -8278.0d0,
     :     -3570.0d0, 1075.0d0/,
     :     c3/9.338d4, 1.7516d5, 4.1040d5, 2.1576d6, 2.376d6, 2.065d6,
     :     2.065d6/,
     :     mic/1.d-6/, c14/1.9d-5/
c
      pda8_correc = c14
      if(i*n.eq.4) return
      pda8_correc = 0.0d0
      if(i.lt.1.or.i.gt.7) return
      if(i.ne.4.and.n.gt.20) return
      if(i.eq.4.and.n.gt.40) return
      an = n
      an = 1.0d0/(an*an)
      pda8_correc = (c1(i) + an*(c2(i) + an*c3(i)))*mic
      return
      end
