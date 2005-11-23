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
      do 20 i = 1, p
         pda_dotprd = pda_dotprd + x(i)*y(i)
 20   continue



c      pda_dotprd = zero
c      if (p .le. 0) go to 999
c      if (sqteta .eq. zero) sqteta = pda_rmdcon(2)
c      do 20 i = 1, p
c         t = dmax1(dabs(x(i)), dabs(y(i)))
c         if (t .gt. one) go to 10
c         if (t .lt. sqteta) go to 20
c         t = (x(i)/sqteta)*y(i)
c         if (dabs(t) .lt. sqteta) go to 20
c 10      pda_dotprd = pda_dotprd + x(i)*y(i)
c 20   continue
c
c 999  return

c  ***  last card of pda_dotprd follows  ***
      end
