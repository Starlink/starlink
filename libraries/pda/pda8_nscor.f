      subroutine pda8_nscor(s,n,n2,ier)
c
c     algorithm as 177.3, applied statistics, v.31, 161-165, 1982.
c
c     calculates approximate expected values of normal order statistics.
c     claimed accuracy is 0.0001, though usually accurate to 5-6 dec.
c
c     arguments:
c     s(n2)   = output, the first n2 expected values (double precision).
c     n       = input, the sample size.
c     n2      = input, the number of order statistics required; must
c                      be <= n/2.
c     ier     = output, error indicator
c                   = 0 if no error detected
c                   = 1 if n <= 1.
c                   = 2 if n > 2000, in which case the order statistics
c                          are still calculated, but may be inaccurate.
c                   = 3 if n2 > n/2 (n.b. this differs from the
c                          published algorithm which returns an error
c                          if n2 is not equal to n/2.)
c
c     calls pda_ppnd16 = a variation of ppnd7 in algorithm AS 241.
c
c   author: royston, j.p
c
      implicit none
      integer*8 n2, n
      integer ier
      double precision s(n2)
      integer*8 i, k
      double precision eps(4), dl1(4), dl2(4), gam(4), lam(4),
     *     bb, d, b1, an, ai, e1, e2, l1
      double precision pda_ppnd16, pda_correc
      external pda_ppnd16, pda_correc
      data eps/0.419885d0, 0.450536d0, 0.456936d0, 0.468488d0/,
     1 dl1/0.112063d0, 0.121770d0, 0.239299d0, 0.215159d0/,
     2 dl2/0.080122d0, 0.111348d0, -0.211867d0, -0.115049d0/,
     3 gam/0.474798d0, 0.469051d0, 0.208597d0, 0.259784d0/,
     4 lam/0.282765d0, 0.304856d0, 0.407708d0, 0.414093d0/,
     5 bb/-0.283833d0/, d/-0.106136d0/, b1/0.5641896d0/
c
c     input parameter checks.
c
      ier = 3
      if(n2.gt.n/2) return
      ier = 1
      if(n.le.1) return
      ier = 0
      if(n.gt.2000) ier = 2
      s(1) = b1
      if(n.eq.2) return
c
c     calculate normal tail areas for first 3 order statistics.
c
      an = n
      k = 3
      if(n2.lt.k) k = n2
      do 5 i = 1,k
        ai = i
        e1 = (ai - eps(i))/(an + gam(i))
        e2 = e1**lam(i)
        s(i) = e1 + e2*(dl1(i) + e2*dl2(i))/an - pda_correc(i,n)
    5 continue
      if(n2.eq.k) go to 20
c
c     calculate normal areas for other cases.
c
      do 10 i = 4,n2
        ai = i
        l1 = lam(4) + bb/(ai + d)
        e1 = (ai - eps(4))/(an + gam(4))
        e2 = e1**l1
        s(i) = e1 + e2*(dl1(4) + e2*dl2(4))/an - pda_correc(i,n)
   10 continue
c
c     convert tail areas to normal deviates.
c
 20   do 30 i = 1,n2
         s(i) = -pda_ppnd16(s(i),ier)
 30   continue
      return
      end
