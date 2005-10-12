      double precision function pda_reldst(p, d, x, x0)
c
c  ***  compute and return relative difference between x and x0  ***
c  ***  nl2sol version 2.2  ***
c
      integer p
      double precision d(p), x(p), x0(p)
c/+
      double precision dabs
c/
      integer i
      double precision emax, t, xmax, zero
c/6
      data zero/0.d+0/
c/7
c     parameter (zero=0.d+0)
c/
c
      emax = zero
      xmax = zero
      do 10 i = 1, p
         t = dabs(d(i) * (x(i) - x0(i)))
         if (emax .lt. t) emax = t
         t = d(i) * (dabs(x(i)) + dabs(x0(i)))
         if (xmax .lt. t) xmax = t
 10      continue
      pda_reldst = zero
      if (xmax .gt. zero) pda_reldst = emax / xmax
 999  return
c  ***  last card of pda_reldst follows  ***
      end
