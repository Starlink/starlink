      subroutine pda_calcc (ns,s,ih,inew,updatc,c)
c
      integer ns,ih,inew
      double precision s(ns,ns+3),c(ns)
      logical updatc
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_calcc calculates the centroid of the simplex without the
c vertex with highest function value.
c
c input
c
c   ns     - subspace dimension
c
c   s      - double precision work space of dimension .ge.
c            ns*(ns+3) used to store simplex
c
c   ih     - index to vertex with highest function value
c
c   inew   - index to new point
c
c   updatc - logical switch
c            = .true.  : update centroid
c            = .false. : calculate centroid from scratch
c
c   c      - centroid of the simplex without vertex with
c            highest function value
c
c output
c
c   c      - new centroid
c
c local variables
c
      integer i,j
c
c subroutines and functions
c
c   blas
      external pda_daxpy,pda_dcopy,pda_dscal
c
c-----------------------------------------------------------
c
      if (updatc) then
        if (ih .eq. inew) return
        do 10 i = 1,ns
          c(i) = c(i)+(s(i,inew)-s(i,ih))/ns
   10   continue
      else
        call pda_dcopy (ns,0.d0,0,c,1)
        do 20 j = 1,ns+1
          if (j .ne. ih) call pda_daxpy (ns,1.d0,s(1,j),1,c,1)
   20   continue
        call pda_dscal (ns,1.d0/ns,c,1)
      end if
      return
      end
