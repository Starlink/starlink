      subroutine overlp(xbar,ybar,pmax,ims,jcount,mm,total,iflag)
      integer*2 ilist(75000),jlist(75000),klist(75000)
      integer*2 ijist(10000),jjist(10000),kjist(10000),njist(10000)
      integer*2 silist(10000),sjlist(10000),sklist(10000)
      integer*2 milist(10000),mjlist(10000),mklist(10000)
      integer*4 iap(8)
      real*4 xdat(9),xcor(9),polycf(3)
      dimension ipoint(200),npoint(200),mpoint(200)
      real*4 parm(16,200)
      integer*2 ibitx(200),ibitl(200),ibity(200)
      common /ov/ ilist,jlist,klist,thresh,ipix,parm,nbit,const,offset,
     1            ianal
      common /an/ ijist,jjist,kjist,njist,silist,sjlist,sklist,npt
c *** OVERLP  looks for overlapping images using multiple isophotes
      isoph=iflag
      ioffs=nint(offset)
      algthr=alog(thresh)
      pi=4.0*atan(1.0)
ccccccwrite(6,5000)cmm
c5000cformat(//'cImagecno.c',i5/)
      il=ims
      ih=ims+jcount-1
      radmax=sqrt(float(jcount)/pi)
c *** 1/2 mag
      tmul=1.585
c *** 1/4 mag
      tmul=sqrt(tmul)
      ithres=nint(thresh)
c *** limits for analysis in main lists
      imnum=200
      imlim=10000
c *** use higher intitial threshold if not smoothed
      smul=2.0
      if(ianal.ne.1)smul=2.5
      newthr=nint(smul*thresh)
      ipixo2=(ipix+1)/2
      ipixo2=max(2,ipixo2)
c *** check no. of points above threshold and store
   50 npt=0
      do 100 i=il,ih
      if(klist(i).le.newthr)goto 100
      npt=npt+1
      if(npt.gt.imlim)then
	newthr=newthr+nint(thresh)
    	goto 50
      endif
      silist(npt)=ilist(i)
      sjlist(npt)=jlist(i)
      sklist(npt)=klist(i)
  100 continue
      if(npt.lt.ipix)then
	iflag=0
	nbit=1
	return
      endif
c ***
c *** main analysis loop at new thresholds
c ***
      nbit=0
      lpt=0
      mimno=0
      isplit=0
  150 call analys
cccccctypec*,'cNo.cofcpointscabovecthresh',npt,newthr
c *** find image boundaries
      nobj=njist(1)
      icount=1
      numim=0
      do 200 i=2,npt
      if(nobj.eq.njist(i))then
	icount=icount+1
	ll=i
   	if(i.eq.npt)goto 175
      else
c *** reject small pixel fragments
  175	if(icount.lt.ipixo2)goto 185
        if(numim.eq.imnum)goto 185
   	numim=numim+1
	ipoint(numim)=ll+1-icount
	if(icount.lt.ipix)icount=-icount
	npoint(numim)=icount
  185	icount=1
 	if(i.eq.npt)goto 200
	nobj=njist(i)
      endif
  200 continue
c *** test to see if lost it altogther
      if(numim.eq.0)then
	numim=1
	ipoint(numim)=1
	npoint(numim)=-npt
      endif
c *** flag for first splitting
      if(numim.gt.1.and.isplit.eq.0)isplit=1
cccccctypec*,'cNo.cofcfragmentsc',numim
c *** for each image check no. of points above next threshold and flag
      isc=0
      nexthr=nint(newthr*tmul)
      nexthr=max(newthr+ithres,nexthr)
      do 300 i=1,numim
      jl=ipoint(i)
      jh=jl+iabs(npoint(i))-1
      mpt=0
      xb=0.0
      yb=0.0
      xsq=0.0
      ysq=0.0
      xysq=0.0
      xoff=ijist(jl)
      yoff=jjist(jl)
      do 220 j=1,8
  220 iap(j)=0
      ijmax=0
      sum=0.0
      do 250 j=jl,jh
      if(kjist(j).lt.nexthr)goto 240
      mpt=mpt+1
  240 it=kjist(j)-newthr
      t=it
      x=ijist(j)-xoff
      y=jjist(j)-yoff
      sum=sum+t
      xb=xb+t*x
      yb=yb+t*y
c *** + 1/12 to allow for finite pixel size
      xsq=xsq+(x*x+1.0/12.0)*t
      ysq=ysq+(y*y+1.0/12.0)*t
      xysq=xysq+x*y*t
      call update(iap,t,0.0,const,offset)
      if(it.gt.ijmax)then
	lx=ijist(j)
	ly=jjist(j)
	ijmax=it
      endif
  250 continue
c *** compute image parameters
      xb=xb/sum
      yb=yb/sum
      sxx=amax1(0.0,xsq/sum-xb**2)
      syy=amax1(0.0,ysq/sum-yb**2)
      sxy=xysq/sum-xb*yb
      xb=xb+xoff
      yb=yb+yoff
      if(sxy.gt.0.0)then
	sxy=amin1(sxy,sqrt(sxx*syy))
 	sxy=amax1(1.0e-4,sxy)
      else
     	sxy=amax1(sxy,-sqrt(sxx*syy))
	sxy=amin1(-1.0e-4,sxy)
      endif
c *** see if image already in list
      if(nbit.eq.0)goto 265
      do 260 k=1,nbit
      if(ibitx(k).ne.lx)goto 260
      if(ibity(k).ne.ly)goto 260
c *** same keep old info - if cog changed by lots overwrite
      if((xb-parm(4,k))**2+(yb-parm(2,k))**2.gt.1.0
     1   .or.isplit.eq.1)then
	parm(1,k)=sum
	parm(4,k)=xb
	parm(2,k)=yb
	parm(3,k)=newthr
	parm(5,k)=sxx
	parm(6,k)=sxy
	parm(7,k)=syy
	parm(8,k)=ijmax
	do j=1,8
	parm(8+j,k)=iap(j)
 	enddo
      endif
      goto 270
  260 continue
c *** store new one
  265 nbit=nbit+1
      if(nbit.gt.imnum)then
	nbit=imnum
	write(*,*) ' '
        write(*,*) ' Warning there are more than',imnum,
     &             ' images in fragment'
	write(*,*) ' No. of images truncated to',imnum
	write(*,*) ' '
	goto 275
      endif
      ibitx(nbit)=lx
      ibity(nbit)=ly
      parm(1,nbit)=sum
      parm(4,nbit)=xb
      parm(2,nbit)=yb
      parm(3,nbit)=newthr
      parm(5,nbit)=sxx
      parm(6,nbit)=sxy
      parm(7,nbit)=syy
      parm(8,nbit)=ijmax
      do j=1,8
      parm(8+j,nbit)=iap(j)
      enddo
  270 if(mpt.lt.ipix.or.npoint(i).lt.0)goto 275
      goto 290
c *** update master storage list for terminated images
  275 mimno=mimno+1
      if(mimno.gt.imnum)then
	mimno=imnum
	write(*,*)' *** Warning *** more than',imnum,' images found'
	goto 400
      endif
      mpoint(mimno)=lpt+1
      do 280 j=jl,jh
      lpt=lpt+1
      milist(lpt)=ijist(j)
      mjlist(lpt)=jjist(j)
  280 mklist(lpt)=kjist(j)
      goto 300
c *** copy analysis lists to storage lists for next threshold analysis
  290 continue
      do 295 j=jl,jh
      if(kjist(j).le.nexthr)goto 295
      isc=isc+1
      silist(isc)=ijist(j)
      sjlist(isc)=jjist(j)
      sklist(isc)=kjist(j)
  295 continue
  300 continue
      if(isplit.eq.1)isplit=2
cccccctypec*,'cBitscanalysis'
ccccccdoc350cj=1,nbit
ccccccwrite(6,1222)cibitx(j),ibity(j),parm(4,j),parm(2,j),parm(3,j),
ccccc1ccccccccccccccparm(1,j)
c1222cformat(2i5,4f10.2)
cc350ccontinue
c *** check for any thing left
  400 if(isc.eq.0.or.mimno.eq.imnum)goto 500
c *** set up next round
      npt=isc
      newthr=nexthr
      goto 150
c ***
c *** Now have master lists of tops of separate images
c ***
  500 continue
cc500ctypec*,'cNo.cofcseparatecimagescfoundc=',mimno
      if(mimno.eq.1)then
	iflag=0
	nbit=1
	return
      endif
c *** calculate image coordinates and peak height
      do 800 k=1,mimno
      jl=mpoint(k)
      if(k.eq.mimno)then
	jh=lpt
      else
	jh=mpoint(k+1)-1
      endif
      itmax=0
      do 700 i=jl,jh
      it=mklist(i)
      if(it.gt.itmax)then
	lx=milist(i)
	ly=mjlist(i)
	itmax=it
      endif
  700 continue
c *** match up tops with bits
      l=0
      idx=10000
      do 750 i=1,nbit
      if(ibitx(i).lt.0)goto 750
      idis=(ibitx(i)-lx)**2+(ibity(i)-ly)**2
      if(idis.gt.idx)goto 750
      l=i
      idx=idis
  750 continue
      if(l.ne.0)then
c *** flag parm list
	parm(1,l)=-parm(1,l)
	ibitx(l)=-1
      endif
  800 continue
      iflag=1
cccccctypec*,'cInitialcimagecparameters'
      do 900 i=1,nbit
      if(parm(1,i).lt.0.0)then
	ibitx(i)=0
	ibitl(i)=0
	parm(1,i)=-parm(1,i)
c	write(6,1000)c(parm(j,i),j=1,8)
c1000	format(2x,8f8.1)
c	write(6,1000) (parm(j,i),j=9,16)
      else
	parm(1,i)=-parm(1,i)
      endif
  900 continue
c ***
c *** for each image find true areal profile levels
c ***
c *** iterate to find local continuum
c ***
      temp=thresh
      if(isoph.eq.1)temp=0.0
      iter=0
      sumint=0.0
      iglag=0
  905 iter=iter+1
      do 950 k=1,nbit
      if(parm(1,k).lt.0.0)goto 950
      xlevol=alog(parm(8,k)+parm(3,k)-ibitl(k))
      radold=0.0
      ttt=0.0
      slope=1.0
      ic=0
      do 935 i=1,8
      jj=17-i
      ii=9-i
      if(parm(jj,k).lt.0.5)goto 935
      if(ii.eq.1)then
	xlevel=alog(parm(3,k)-ibitl(k)+0.5)
      else
	xlevel=alog(2.0**(ii+ioffs-1)+parm(3,k)-ibitl(k)-0.5)
      endif
      radius=sqrt(parm(jj,k)/pi)
      ic=ic+1
      xdat(ic)=xlevel
      xcor(ic)=radius
      dlbydr=(xlevol-xlevel)/amax1(0.01,radius-radold)
      wt=amax1((radius-radold)*5.0,0.1)
      wt=amin1(wt,1.0)
      slope=(1.0-0.5*wt)*slope+0.5*wt*amin1(5.0,dlbydr)
      radold=radius
      xlevol=xlevel
  935 continue
      if(iglag.eq.1)goto 945
c *** estimate effect on local continuum from each image
      do 940 i=1,nbit
      if(parm(1,i).lt.0.0.or.i.eq.k)goto 940
      dist=sqrt((parm(4,k)-parm(4,i))**2+(parm(2,k)-parm(2,i))**2)
      xeff=xlevel-amax1(0.0,slope*(dist-radius))
      ibitx(i)=ibitx(i)+nint(exp(xeff))
  940 continue
      goto 950
c *** now update parameters
  945 if(ic.gt.2)then
	call polynm(xdat,xcor,ic,polycf,3,0)
	ttt=polycf(2)+2.0*polycf(3)*radius
      endif
      slope=amax1(-ttt,slope)
      radthr=radius+(xlevel-algthr)/slope
      if(radthr.gt.radmax)then
	slope=(xlevel-algthr)/(radmax-radius)
	radthr=radmax
      endif
c *** pixel area
      delb=parm(9,k)*(parm(3,k)-ibitl(k))
      parm(9,k)=pi*radthr**2
c *** peak height
      parm(8,k)=parm(8,k)+(parm(3,k)-ibitl(k))
c *** intensity
      deli=2.0*pi*((parm(3,k)-ibitl(k))*(1.0+slope*radius)-
     1           temp*(1.0+slope*radthr))/slope**2
      parm(1,k)=parm(1,k)+delb+amax1(0.0,deli)
cccccctypec*,'cSlopecetc.',slope,radthr,radius,deli,delb
      do i=1,7
      parm(i+9,k)=-1.0
      enddo
      sumint=sumint+parm(1,k)
  950 continue
      if(iglag.eq.1)goto 959
c *** check changes in continuum
      iglag=1
ccccccwrite(6,1989)c((ibitl(i),ibitx(i)),i=1,nbit)
c1989cformat(2i8)
      do 955 i=1,nbit
      if(parm(1,i).lt.0.0)goto 955
      if(abs(ibitx(i)-ibitl(i)).gt.3)iglag=0
      ibitl(i)=ibitx(i)
      ibitx(i)=0
      ibitl(i)=min(ibitl(i),nint(parm(3,i)-thresh))
  955 continue
      if(iter.eq.5)iglag=1
      goto 905
  959 if(sumint.eq.0.0)then
	iflag=0
	nbit=1
	return
      else
	ratio=total/sumint
      endif
cccccctypec*,'cTotalcintensityc=',total,ratio
cccccctypec*,'cFinalcimagecparameters'
c *** remove surplus images
      ii=0
      do 980 k=1,nbit
      if(parm(1,k).gt.0.0)then
	ii=ii+1
      else
        goto 980
      endif
      if(ii.eq.k)goto 980
      do 970 i=1,16
  970 parm(i,ii)=parm(i,k)
  980 continue
      nbit=ii
      do 990 i=1,nbit
	parm(1,i)=ratio*parm(1,i)
c	write(6,1000) (parm(j,i),j=1,8)
c	write(6,1000) (parm(j,i),j=9,16)
  990 continue
      return
      end
* @(#)overlp.f	1.2     3/1/95     1
* $Id$
