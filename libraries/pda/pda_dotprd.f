      double precision function pda_dotprd(p, x, y)
c
c  ***  return the inner product of the p-vectors x and y.  ***
c
      integer p
      double precision x(p), y(p)
c
      integer i
      double precision one, sqteta, t, zero
c/+
      double precision dmax1, dabs
c/
      external pda_rmdcon
      double precision pda_rmdcon
c
c  ***  pda_rmdcon(2) returns a machine-dependent constant, sqteta, which
c  ***  is slightly larger than the smallest positive number that
c  ***  can be squared without underflowing.
c
c/6
      data one/1.d+0/, sqteta/0.d+0/, zero/0.d+0/
c/7
c     parameter (one=1.d+0, zero=0.d+0)
c     data sqteta/0.d+0/
c/
c
      pda_dotprd = zero
      if (p .le. 0) go to 999
      if (sqteta .eq. zero) sqteta = pda_rmdcon(2)
      do 20 i = 1, p
         t = dmax1(dabs(x(i)), dabs(y(i)))
         if (t .gt. one) go to 10
         if (t .lt. sqteta) go to 20
         t = (x(i)/sqteta)*y(i)
         if (dabs(t) .lt. sqteta) go to 20
 10      pda_dotprd = pda_dotprd + x(i)*y(i)
 20   continue
c
 999  return
c  ***  last card of pda_dotprd follows  ***
      end
