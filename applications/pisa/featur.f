      subroutine featur(xmodel,ims,jcount,mpx,sigma,range,iextra,edge)
      integer*2 ilist(75000),jlist(75000),klist(75000)
      real*4 parm(16,200),xcords(200),ycords(200)
      real*4 xcordl(200),ycordl(200)
      real*4 xext(50),yext(50),sext(50)
      integer*2 icext(50)
      real*4 xmodel(10000)
      integer*2 ijist(10000),jjist(10000),kjist(10000),njist(10000)
      integer*2 silist(10000),sjlist(10000),sklist(10000)
      common /ov/ ilist,jlist,klist,phresh,ipix,parm,nbit,const,
     1  offset,ianal
      common /st/ xcords,ycords,xcordl,ycordl
      common /an/ ijist,jjist,kjist,njist,silist,sjlist,sklist,npt
      common /fil/ istart,istop,nword,ixl,ixh
c *** FEATUR  looks at difference map for blend and sticks in more
c ***         images if necessary.
      iextra=0
      thresh=2.0*sigma
      imlim=10000
      sigsq=sigma**2
      il=ims
      ih=ims+jcount-1
c *** grab pixels for analysis
   50 npt=0
      do 100 i=il,ih
      signif=sqrt(1.0+max(0,klist(i))/sigsq)
      if(klist(i)-xmodel(i-il+1).le.thresh*signif)goto 100
      npt=npt+1
      if(npt.gt.imlim)then
	thresh=thresh+sigma
	goto 50
      endif
      silist(npt)=ilist(i)
      sjlist(npt)=jlist(i)
      sklist(npt)=klist(i)-nint(xmodel(i-il+1))
  100 continue
c      type *,' No. of pixels above threshold =',npt
      if(npt.lt.mpx)then
	iextra=0
	return
      endif
      call analys
c *** find separate images their coordinates intensities etc.
  125 mobj=njist(1)
      icount=1
      tt=kjist(1)
      xx=ijist(1)*tt
      yy=jjist(1)*tt
      pk=tt
      ss=tt
      numim=0
      do 200 i=2,npt
      if(mobj.eq.njist(i))then
	icount=icount+1
	tt=kjist(i)
	xx=xx+ijist(i)*tt
	yy=yy+jjist(i)*tt
	pk=amax1(pk,tt)
	ss=ss+tt
	if(i.eq.npt)goto 175
      else
c *** reject small bits
  175 	if(icount.lt.mpx)goto 185
	numim=numim+1
	if(numim+nbit.gt.200)then
	mpx=mpx+1
	goto 125
	endif
  	xext(numim)=xx/ss
	yext(numim)=yy/ss
	sext(numim)=ss/(1.0-thresh/pk)
	icext(numim)=icount
  185	if(i.eq.npt)goto 200
     	icount=1
	tt=kjist(i)
	xx=ijist(i)*tt
	yy=jjist(i)*tt
	pk=tt
	ss=tt
	mobj=njist(i)
      endif
  200 continue
      if(numim.eq.0)then
	iextra=0
	return
      endif
c ***
c *** Now check thro list and see if any can be removed
c ***
      do 300 i=1,numim
      if(xext(i).lt.istart+edge.or.xext(i).gt.istop-edge.
     1 or.yext(i).lt.ixl+edge.or.yext(i).gt.ixh-edge)then
	sext(i)=-1.0
	goto 300
      endif
      do 250 j=1,nbit
      if(parm(1,j).lt.0.5)goto 250
      xydis=(parm(4,j)-xext(i))**2+(parm(2,j)-yext(i))**2
      if(xydis.lt.range)then
	sext(i)=-1.0
	goto 300
      endif
  250 continue
  300 continue
c *** store in master object lists
      do 500 i=1,numim
      if(sext(i).lt.0.0)goto 500
      nbit=nbit+1
      iextra=iextra+1
c      write(6,1000) xext(i),yext(i),sext(i),iextra,icext(i)
c 1000 format(3f10.3,2i6)
      parm(1,nbit)=sext(i)
      parm(2,nbit)=yext(i)
      parm(4,nbit)=xext(i)
      xcords(nbit)=xext(i)
      ycords(nbit)=yext(i)
      xcordl(nbit)=xext(i)
      ycordl(nbit)=yext(i)
  500 continue
      return
      end
* $Id$
