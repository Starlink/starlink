      subroutine pda_order (npts,fs,il,is,ih)
c
      integer npts,il,is,ih
      double precision fs(npts)
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_order determines the indices of the vertices with the
c lowest, second highest, and highest function values.
c
c input
c
c   npts   - number of points in simplex
c
c   fs     - double precision vector of function values of
c            simplex
c
c   il     - index to vertex with lowest function value
c
c output
c
c   il     - new index to vertex with lowest function value
c
c   is     - new index to vertex with second highest
c            function value
c
c   ih     - new index to vertex with highest function value
c
c local variables
c
      integer i,il0,j
c
c subroutines and functions
c
c   fortran
      intrinsic mod
c
c-----------------------------------------------------------
c
      il0 = il
      j = mod(il0,npts)+1
      if (fs(j) .ge. fs(il)) then
        ih = j
        is = il0
      else
        ih = il0
        is = j
        il = j
      end if
      do 10 i = il0+1,il0+npts-2
        j = mod(i,npts)+1
        if (fs(j) .ge. fs(ih)) then
          is = ih
          ih = j
        else if (fs(j) .gt. fs(is)) then
          is = j
        else if (fs(j) .lt. fs(il)) then
          il = j
        end if
   10 continue
      return
      end
