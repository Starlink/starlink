      double precision function pda_dist (n,x,y)
c
      integer n
      double precision x(n),y(n)
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_dist calculates the distance between the points x,y.
c
c input
c
c   n      - number of components
c
c   x      - point in n-space
c
c   y      - point in n-space
c
c local variables
c
      integer i
      double precision absxmy,scale,sum
c
c subroutines and functions
c
c   fortran
      intrinsic abs,sqrt
c
c-----------------------------------------------------------
c
      absxmy = abs(x(1)-y(1))
      if (absxmy .le. 1.d0) then
        sum = absxmy*absxmy
        scale = 1.d0
      else
        sum = 1.d0
        scale = absxmy
      end if
      do 10 i = 2,n
        absxmy = abs(x(i)-y(i))
        if (absxmy .le. scale) then
          sum = sum+(absxmy/scale)**2
        else
          sum = 1.d0+sum*(scale/absxmy)**2
          scale = absxmy
        end if
   10 continue
      pda_dist = scale*sqrt(sum)
      return
      end
