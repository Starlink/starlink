      subroutine pda_fpbspl(t,n,k,x,l,h)
c  subroutine pda_fpbspl evaluates the (k+1) non-zero b-splines of
c  degree k at t(l) <= x < t(l+1) using the stable recurrence
c  relation of de boor and cox.
c  ..
c  ..scalar arguments..
      real x
      integer n,k,l
c  ..array arguments..
      real t(n),h(6)
c  ..local scalars..
      real f,one
      integer i,j,li,lj
c  ..local arrays..
      real hh(5)
c  ..
      one = 0.1e+01
      h(1) = one
      do 20 j=1,k
        do 10 i=1,j
          hh(i) = h(i)
  10    continue
        h(1) = 0.
        do 20 i=1,j
          li = l+i
          lj = li-j
          f = hh(i)/(t(li)-t(lj))
          h(i) = h(i)+f*(t(li)-x)
          h(i+1) = f*(x-t(lj))
  20  continue
      return
      end
