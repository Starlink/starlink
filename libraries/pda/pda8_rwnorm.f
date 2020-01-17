c
      subroutine pda8_rwnorm(v,n,mdim,id)
c
c      Appl. Statist algorithm as 128.4 (1978), vol. 27
c      Davis C.S. and Stephens M.A.
c
c      Normalises rows of covariance matrix of normal order statistics
c      so that sum of row elements equals one.
c
c      arguments :      v - array (mdim,mdim) containing the covariance
c                           matrix approximation.
c                       n - sample size.
c                    mdim - row dimension of v in the calling program.
c                      id -
c
      implicit none
      integer id
      integer*8 n,mdim
      double precision v(mdim,n)
c
c      local integer variables
c
      integer*8 i,j,k,l,m,ni,nj,nhalf1
c
c      local real variables
c
      double precision cnst,one,small,sum,term,zero
c
      data zero /0.0d0/, small /1.0d-12/, one /1.0d0/
c
      nhalf1 = (n+1)/2
      ni=n-1
      do 75 i=2,nhalf1
c
c     find sums of computed terms in each row
c
         sum=zero
         do 55 j=i,ni
            sum=sum+v(i,j)
   55    continue
         if(id .ne.0) sum = sum - v(i,i)
         if(abs(sum).lt.small) go to 75
c
c     normalise rows leaving appropriate elements fixed
c
         k=i-1
         if(id .ne. 0) k = i
         term=zero
         do 60 j=1,k
   60       term=term+v(i,j)
         l=ni+1
         do 65 j=l,n
   65       term=term+v(i,j)
         cnst=(one-term)/sum
         m=i
         if(id.ne.0) m=i+1
         nj=n-m+1
         do 70 j=m,ni
            v(i,j)=v(i,j)*cnst
            v(j,i)=v(i,j)
            v(ni,nj)=v(i,j)
            v(nj,ni)=v(i,j)
            nj=nj-1
   70    continue
         ni=ni-1
   75 continue
      return
      end
