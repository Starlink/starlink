      subroutine pda_dupdu(d, hdiag, iv, liv, lv, n, v)
c
c  ***  update scale vector d for pda_humsl  ***
c
c  ***  parameter declarations  ***
c
      integer liv, lv, n
      integer iv(liv)
      double precision d(n), hdiag(n), v(lv)
c
c  ***  local variables  ***
c
      integer dtoli, d0i, i
      double precision t, vdfac
c
c  ***  intrinsic functions  ***
c/+
      double precision dabs, dmax1, dsqrt
c/
c  ***  subscripts for iv and v  ***
c
      integer dfac, dtol, dtype, niter
c/6
      data dfac/41/, dtol/59/, dtype/16/, niter/31/
c/7
c     parameter (dfac=41, dtol=59, dtype=16, niter=31)
c/
c
c-------------------------------  body  --------------------------------
c
      i = iv(dtype)
      if (i .eq. 1) go to 10
         if (iv(niter) .gt. 0) go to 999
c
 10   dtoli = iv(dtol)
      d0i = dtoli + n
      vdfac = v(dfac)
      do 20 i = 1, n
         t = dmax1(dsqrt(dabs(hdiag(i))), vdfac*d(i))
         if (t .lt. v(dtoli)) t = dmax1(v(dtoli), v(d0i))
         d(i) = t
         dtoli = dtoli + 1
         d0i = d0i + 1
 20      continue
c
 999  return
c  ***  last card of pda_dupdu follows  ***
      end
