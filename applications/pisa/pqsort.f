      subroutine pqsort(x,point,l,nfilt)
*
*changed name to pqsort from qsort to resolve naming clash
*
      real*4 x(nfilt)
      integer*2 it,point(nfilt)
      test=x(l)
      do 100 i=1,nfilt
      if(i.eq.l)goto 100
      if(test.gt.x(i))goto 100
      j=i
      goto 200
  100 continue
      j=nfilt+1
  200 if(j-1.eq.l)return
      if(j-l)300,500,400
  300 temp=x(l)
      it=point(l)
      npt=l-j
      do 350 i=1,npt
      ii=l-i
      x(ii+1)=x(ii)
  350 point(ii+1)=point(ii)
      x(j)=temp
      point(j)=it
      return
  400 temp=x(l)
      it=point(l)
      j=j-1
      npt=j-l
      if(npt.eq.0)goto 475
      do 450 i=1,npt
      ii=l+i
      x(ii-1)=x(ii)
  450 point(ii-1)=point(ii)
  475 x(j)=temp
      point(j)=it
  500 return
      end
* $Id$
