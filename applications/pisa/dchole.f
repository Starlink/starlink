      subroutine dchole(a,b,n)
      real*8 a(201,201),b(201)
      real*8 sum,l(201,201),y(201)
c *** CHOLEsky decomposition of positive definite symmetric matrix
c ***          to solve Ax = b.
   50 l(1,1)=dsqrt(a(1,1))
      do 300 k=2,n
      do 200 j=1,k-1
      sum=a(k,j)
      if(j.eq.1)goto 200
      do 100 i=1,j-1
  100 sum=sum-l(k,i)*l(j,i)
  200 l(k,j)=sum/l(j,j)
      sum=a(k,k)
      do 250 i=1,k-1
  250 sum=sum-l(k,i)*l(k,i)
      if(sum.le.0.d0)then
	write (*,*) ' **** warning matrix ill-conditioned ****'
	aveigv=a(1,1)
	do 275 i=2,n
  275	aveigv=aveigv+a(i,i)
c *** max eigenvalue < trace
	offset=aveigv*1.0e-15
        do 280 i=1,n
  280	a(i,i)=a(i,i)+offset
	write(*,*)' Offset added to diagonal =',offset
	goto 50
      endif
  300 l(k,k)=dsqrt(sum)
c *** solve Ly = b
      y(1)=b(1)/l(1,1)
      do 500 i=2,n
      sum=b(i)
      do 400 k=1,i-1
  400 sum=sum-l(i,k)*y(k)
  500 y(i)=sum/l(i,i)
c *** solve L(T)x = y
      b(n)=y(n)/l(n,n)
      do 700 i=n-1,1,-1
      sum=y(i)
      do 600 k=i+1,n
  600 sum=sum-l(k,i)*b(k)
  700 b(i)=sum/l(i,i)
      return
      end
* $Id$
