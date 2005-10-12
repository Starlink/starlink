      double precision function pda_v2norm(p, x)
c
c  ***  return the 2-norm of the p-vector x, taking  ***
c  ***  care to avoid the most likely underflows.    ***
c
      integer p
      double precision x(p)
c
      integer i, j
      double precision one, r, scale, sqteta, t, xi, zero
c/+
      double precision dabs, dsqrt
c/
      external pda_rmdcon
      double precision pda_rmdcon
c
c/6
      data one/1.d+0/, zero/0.d+0/
c/7
c     parameter (one=1.d+0, zero=0.d+0)
c     save sqteta
c/
      data sqteta/0.d+0/
c
      if (p .gt. 0) go to 10
         pda_v2norm = zero
         go to 999
 10   do 20 i = 1, p
         if (x(i) .ne. zero) go to 30
 20      continue
      pda_v2norm = zero
      go to 999
c
 30   scale = dabs(x(i))
      if (i .lt. p) go to 40
         pda_v2norm = scale
         go to 999
 40   t = one
      if (sqteta .eq. zero) sqteta = pda_rmdcon(2)
c
c     ***  sqteta is (slightly larger than) the square root of the
c     ***  smallest positive floating point number on the machine.
c     ***  the tests involving sqteta are done to prevent underflows.
c
      j = i + 1
      do 60 i = j, p
         xi = dabs(x(i))
         if (xi .gt. scale) go to 50
              r = xi / scale
              if (r .gt. sqteta) t = t + r*r
              go to 60
 50           r = scale / xi
              if (r .le. sqteta) r = zero
              t = one  +  t * r*r
              scale = xi
 60      continue
c
      pda_v2norm = scale * dsqrt(t)
 999  return
c  ***  last card of pda_v2norm follows  ***
      end
