      subroutine median(xbuf,npt,nfilt)
c *** MEDIAN  performs median filtering on array xbuf
*
* history : changed call to qsort to pqsort (DUVAD::PDRAPER; 6-11-1990)
*
      dimension xbuf(npt),ybuf(8704),array(512)
      integer*2 point(512)
*
*
      if(nfilt.gt.511)stop ' too large a filter'
      if(npt.gt.8192)stop ' too many points in data array'
      if((nfilt/2)*2.eq.nfilt)nfilt=nfilt+1
      nfo2p1=nfilt/2+1
c *** set first and last edges equal
      il=nfilt/2
      ilow=max0(3,nfilt/4)
      ilow=(ilow/2)*2+1
      do 100 i=1,ilow
  100 array(i)=xbuf(i)
      call sortm(array,point,ilow)
      xmns=array(ilow/2+1)
      do 150 i=1,ilow
  150 array(i)=xbuf(npt+1-i)
      call sortm(array,point,ilow)
      xmnf=array(ilow/2+1)
c *** reflect edges before filtering
      do 200 i=1,il
      ybuf(i)=2.0*xmns-xbuf(il+ilow+1-i)
  200 ybuf(npt+i+il)=2.0*xmnf-xbuf(npt-i-ilow+1)
      do 250 i=1,npt
  250 ybuf(i+il)=xbuf(i)
c *** do median filtering on rest
      do 260 i=1,nfilt
      array(i)=ybuf(i)
  260 point(i)=i
      call sortm(array,point,nfilt)
      xbuf(1)=array(nfo2p1)
      jl=nfilt+1
      jh=nfilt+npt-1
      do 400 j=jl,jh
      do 300 i=1,nfilt
      if(point(i).eq.1)goto 290
      point(i)=point(i)-1
      goto 300
  290 point(i)=nfilt
      array(i)=ybuf(j)
      l=i
  300 continue
      call pqsort(array,point,l,nfilt)
  400 xbuf(j-jl+2)=array(nfo2p1)
      return
      end
* $Id$
