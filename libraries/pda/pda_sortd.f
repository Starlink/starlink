      subroutine pda_sortd (n,xkey,ix)
c
      integer n
      integer ix(n)
      double precision xkey(n)
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_sortd uses the shakersort method to sort an array of keys
c in decreasing order. The sort is performed implicitly by
c modifying a vector of indices.
c
c For nearly sorted arrays, pda_sortd requires O(n) comparisons.
c for completely unsorted arrays, pda_sortd requires O(n**2)
c comparisons and will be inefficient unless n is small.
c
c input
c
c   n      - number of components
c
c   xkey   - double precision vector of keys
c
c   ix     - integer vector of indices
c
c output
c
c   ix     - indices satisfy xkey(ix(i)) .ge. xkey(ix(i+1))
c            for i = 1,...,n-1
c
c local variables
c
      integer i,ifirst,ilast,iswap,ixi,ixip1
c
c-----------------------------------------------------------
c
      ifirst = 1
      iswap = 1
      ilast = n-1
   10 continue
      if (ifirst .le. ilast) then
        do 20 i = ifirst,ilast
          ixi = ix(i)
          ixip1 = ix(i+1)
          if (xkey(ixi) .lt. xkey(ixip1)) then
            ix(i) = ixip1
            ix(i+1) = ixi
            iswap = i
          end if
   20   continue
        ilast = iswap-1
        do 30 i = ilast,ifirst,-1
          ixi = ix(i)
          ixip1 = ix(i+1)
          if (xkey(ixi) .lt. xkey(ixip1)) then
            ix(i) = ixip1
            ix(i+1) = ixi
            iswap = i
          end if
   30   continue
        ifirst = iswap+1
        go to 10
      end if
      return
      end
