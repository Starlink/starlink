      subroutine pda_start (n,x,step,ns,ips,s,small)
c
      integer n,ns
      integer ips(n)
      double precision x(n),step(n),s(ns,ns+3)
      logical small
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_start creates the initial simplex for pda_simplx minimization.
c
c input
c
c   n      - problem dimension
c
c   x      - current best point
c
c   step   - stepsizes for corresponding components of x
c
c   ns     - subspace dimension
c
c   ips    - permutation vector
c
c
c output
c
c   s      - first ns+1 columns contain initial simplex
c
c   small  - logical flag
c            = .true.  : coincident points
c            = .false. : otherwise
c
c local variables
c
      integer i,j
c
c subroutines and functions
c
c   blas
      external pda_dcopy
c   fortran
      intrinsic dble
c
c-----------------------------------------------------------
c
      do 10 i = 1,ns
        s(i,1) = x(ips(i))
   10 continue
      do 20 j = 2,ns+1
        call pda_dcopy (ns,s(1,1),1,s(1,j),1)
        s(j-1,j) = s(j-1,1)+step(ips(j-1))
   20 continue
c
c check for coincident points
c
      do 30 j = 2,ns+1
        if (dble(s(j-1,j)) .eq. dble(s(j-1,1))) go to 40
   30 continue
      small = .false.
      return
c
c coincident points
c
   40 continue
      small = .true.
      return
      end
