      subroutine pda_newpt (ns,coef,xbase,xold,new,xnew,small)
c
      integer ns
      double precision coef,xbase(ns),xold(ns),xnew(*)
      logical new,small
c
c                                         Coded by Tom Rowan
c                            Department of Computer Sciences
c                              University of Texas at Austin
c
c pda_newpt performs reflections, expansions, contractions, and
c shrinkages (massive contractions) by computing:
c
c xbase + coef * (xbase - xold)
c
c The result is stored in xnew if new .eq. .true.,
c in xold otherwise.
c
c use :  coef .gt. 0 to reflect
c        coef .lt. 0 to expand, contract, or shrink
c
c input
c
c   ns     - number of components (subspace dimension)
c
c   coef   - one of four simplex method coefficients
c
c   xbase  - double precision ns-vector representing base
c            point
c
c   xold   - double precision ns-vector representing old
c            point
c
c   new    - logical switch
c            = .true.  : store result in xnew
c            = .false. : store result in xold, xnew is not
c                        referenced
c
c output
c
c   xold   - unchanged if new .eq. .true., contains new
c            point otherwise
c
c   xnew   - double precision ns-vector representing new
c            point if  new .eq. .true., not referenced
c            otherwise
c
c   small  - logical flag
c            = .true.  : coincident points
c            = .false. : otherwise
c
c local variables
c
      integer i
      double precision xoldi
      logical eqbase,eqold
c
c subroutines and functions
c
c   fortran
      intrinsic dble
c
c-----------------------------------------------------------
c
      eqbase = .true.
      eqold = .true.
      if (new) then
        do 10 i = 1,ns
          xnew(i) = xbase(i)+coef*(xbase(i)-xold(i))
          eqbase = eqbase .and.
     *             (dble(xnew(i)) .eq. dble(xbase(i)))
          eqold = eqold .and.
     *            (dble(xnew(i)) .eq. dble(xold(i)))
   10   continue
      else
        do 20 i = 1,ns
          xoldi = xold(i)
          xold(i) = xbase(i)+coef*(xbase(i)-xold(i))
          eqbase = eqbase .and.
     *             (dble(xold(i)) .eq. dble(xbase(i)))
          eqold = eqold .and.
     *            (dble(xold(i)) .eq. dble(xoldi))
   20   continue
      end if
      small = eqbase .or. eqold
      return
      end
