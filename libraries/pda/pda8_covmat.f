      SUBROUTINE PDA8_COVMAT( V, N, MDIM, V11, EX1, EX2, SUMM2, IFAULT )
*+
*  Name:
*     PDA8_COVMAT

*  Purpose:
*     Approximates the covariance matrix of normal order statistics.

*  Language:
*     Fortran-77

*  Invocation:
*     CALL PDA8_COVMAT( V, N, MDIM, V11, EX1, EX2, SUMM2, IFAULT )

*  Description:
*     This routine computes and normalises the David-Johnson
*     approximation for the covariance matrix of normal order
*     statistics. The value V11 can be calculated using the PDA_V11
*     routine and the values of EX1, EX2 and SUMM2 using PDA_NSCOR.

*  Arguments:
*     V( MDIM, N ) = DOUBLE PRECISION (Returned)
*        The covariance approximation.
*     N = INTEGER*8 (Given)
*        The sample size.
*     MDIM = INTEGER*8 (Given)
*        First dimension of V in calling routine.
*     V11 = DOUBLE PRECISION (Given)
*        Exact value of the extreme variance V(1,1).
*     EX1 = DOUBLE PRECISION (Given)
*        Absolute expected value of the smallest order statistic from a
*        size N sample.
*     EX2 = DOUBLE PRECISION (Given)
*        Absolute expected value of the second smallest order statistic
*        from a size N sample.
*     SUMM2 = DOUBLE PRECISION (Given)
*        Sum of squares of expected values order statistics for a sample
*        of size N.
*     IFAULT = INTEGER (Returned)
*        Failure indicator. Zero for success, otherwise N is out of
*        bounds.

*  Algorithm:
*     Appl. Statist. algorithm as 128 (1978), vol. 27

*  Origin:
*     Applied Statistics / Statlib Archive

*  Copyright:
*     The Royal Statistical Society.

*  Authors:
*     ORIGINAL: Davis C.S. and Stephens M.A.
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     1978 (ORIGINAL):
*        Original version.
*     20-FEB-1997 (PDRAPER):
*        Added prologue, changed to use PDA_PPND16 and slight
*        modifications to make output correspond more closely to
*        that of equivalent NAG routine.
*     15-JAN-2020 (DSB):
*        Add support for huge arrays.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
c
      implicit none
      integer*8 n,mdim
      integer ifault
      double precision v(mdim,n),v11,ex1,ex2,summ2
c
c      local integer variables
c
      integer*8 i,j,k,ni,nj,nhalf1,njm1,im1,nsub1
c
c      local real variables
c
      double precision cnst,dxr,d2xr,d3xr,d4xr,d5xr,dxs,d2xs,
     :       d3xs,d4xs,d5xs,pr,ps,qr,rn,rn1,rn2,rn22,rn23,sum,
     :       two,xr,xs,zero,half,one
      double precision pda_ppnd16, pda_var, pda_cov
      external pda_ppnd16, pda_var, pda_cov
c
      common /cons/ rn2,rn22,rn23
c
c     initialise constants
c
      data zero /0.0d0/,half /0.5d0/, one /1.0d0/ ,two/2.0d0/
c
      ifault = 1
      if(n .gt. mdim .or. n. lt. 2) return
      ifault = 0
      rn=n
      rn1=rn+one
      rn2=rn+two
      rn22=rn2*rn2
      rn23=rn22*rn2
      nhalf1=(n+1) / 2
c
c     the elements of the upper triangle
c     are first computed
c
      ni=n
      do 50 i=1,nhalf1
         pr=dble(i)/rn1
         qr=one-pr
c
c        in function pda_ppnd16, xr is computed to satisfy
c        prob(z.lt.pr) = pr, where z is n(0,1)
c
         xr = pda_ppnd16(pr, ifault)
         call pda_der(xr,dxr,d2xr,d3xr,d4xr,d5xr)
         do 40 j=i,ni
            if (i .ne. j) goto 30
c
c     if i is equal to j, var(xr) is calculated
c
            v(i,j)=pda_var(dxr,d2xr,d3xr,d4xr,d5xr,pr,qr)
            go to 40
c
c     if i is not equal to j, cov(xr,xs) is calculated
c
   30       ps=dble(j)/rn1
            xs=pda_ppnd16(ps,ifault)
            call pda_der(xs,dxs,d2xs,d3xs,d4xs,d5xs)
            v(i,j)=pda_cov(dxr,d2xr,d3xr,d4xr,d5xr,pr,qr,
     1                    dxs,d2xs,d3xs,d4xs,d5xs,ps)
            v(j,i)=v(i,j)
   40    continue
         ni=ni-1
   50 continue
c
c        By symmetry the other elements of v will now be filled
c
      nj = n
      do 70 i = 2,n
      njm1 = nj - 1
      im1 = i - 1
      do 60 j = nj, n
      v(i,j) = v(im1, njm1)
      im1 = im1 - 1
   60 continue
      nj = nj - 1
   70 continue
c
c     insert exact values of v(1,1) and v(1,2)
c
      v(1,1)=v11
      v(n,n)=v(1,1)
      v(1,2)=v(1,1)+ex1*(ex1-ex2)-one
      v(2,1)=v(1,2)
      nsub1=n-1
      v(n,nsub1)=v(1,2)
      v(nsub1,n)=v(1,2)
c
c     normalise the first row of v, leaving
c     v(1,1) and v(1,2) fixed
c
      if(n.eq.2) return
      sum=zero
      do 80 j=3,n
         sum=sum+v(1,j)
   80 continue
      cnst=(one-v(1,1)-v(1,2))/sum
      nj=n-2
      do 90 j=3,n
         v(1,j)=v(1,j)*cnst
         v(j,1)=v(1,j)
	 v(n,nj)=v(1,j)
	 v(nj,n)=v(1,j)
         nj=nj-1
   90 continue
c
c     normalise rows 2 through n-1 of v
c
      call pda8_rwnorm(v,n,mdim,0)

c
c     modify v(2,2) and its equal
c     v(n-1,n-1) so the trace identity is satisfied
c
      sum=zero
      do 100 k=1,n
         if(k.eq.2.or.k.eq.nsub1) go to 100
         sum=sum+v(k,k)
 100  continue
      v(2,2)=half*(dble(n)-summ2-sum)
*     PWD: added next line as it makes the output correspond with
*          equivalent NAG routine for n=3.
      if ( n .eq. 3 ) v(2,2) = 2.0d0 * v(2,2)
      v(nsub1,nsub1)=v(2,2)

c
c     renormalise rows 2 through n-1 of v,
c     leaving diagonal elements fixed,
c
      call pda8_rwnorm(v,n,mdim,1)
      return
      end
