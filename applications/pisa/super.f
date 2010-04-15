      subroutine super(ims,jcount,avchi,numchi,sigma,psf,ifsm)
c *** SUPER  does pseudo ls refinement using initial coord list
      integer*2 ilist(75000),jlist(75000),klist(75000)
      real*4 xmodel(10000),parm(16,200)
      real*4 xcords(200),ycords(200)
      real*4 xcordl(200),ycordl(200)
      real*4 xmn(200),tmn(200),ymn(200),sxdel(200),sydel(200),sint(200)
      integer*2 iref(200)
      real*4 erri(200),errc(200),xdamp(200),ydamp(200)
      common /ov/ ilist,jlist,klist,phresh,ipix,parm,nbit,konst,
     1   offset,ianal
      common /pm/  parm1,parm2,parmn,parsq,change,parrad,iupp,skycor,ib
      common /pmn/  parmn1,parmn2,parmnn,q,imodel
      common /st/ xcords,ycords,xcordl,ycordl
      pi=4.0*atan(1.0)
      lt=6
      ill=ims
      ihh=ims+jcount-1
      imlim=10000
      if(jcount.gt.imlim)then
        write(*,*) '**** warning too many pixels for surface modelling'
        return
      endif
c *** save starting coordinates
      do 100 i=1,nbit
      xcordl(i)=parm(4,i)
      ycordl(i)=parm(2,i)
      xcords(i)=parm(4,i)
  100 ycords(i)=parm(2,i)
      sigsq=sigma**2
      psfsq=psf**2
      const=4.0*pi*sigsq*psfsq
c *** limit for edge effects
      edge=psf
c *** saturation limit
      fupp=float(iupp)
c *** minimum intensity for theoretical bit
      xintmn=sigma/parmn
c *** minimum intensity during refinement
      yintmn=pi*psf**2*sigma*0.5
c *** after
      zintmn=phresh/parmn
      zintmn=amax1(zintmn,2.0*yintmn)
c *** minimum pixel size for extra images
      minpix=ipix
      minpix=max0(4,minpix)
      iextra=0
      irem=0
c *** look for saturated pixels
      nsat=0
      do 400 i=ill,ihh
      if(klist(i).ge.iupp)nsat=nsat+1
  400 continue
c      type *,' '
c      type *,' No. of pixels for blend =',jcount,nsat
c *** limit for rejecting spurious images
      dislim=psf
      rsat=sqrt(float(nsat)/pi)
      dissq=(dislim+rsat)**2
c *** limit for introducing new ones
      range=dissq/16.0
c *** model fitting threshold
      xmthr=amax1(1.0,0.5*sigma)
c
c *** now do refinement of position and intensity
c
  445 chisqo=1.0e10
      nter=0
      skycor=0.0
  450 nter=nter+1
      iflag=0
c *** if already removed images this cycle skip this bit
      if(irem.ne.0)goto 470
c *** if any images are within 1.0*psf use larger only
      do 460 i=1,nbit
      if(parm(1,i).lt.0.5)goto 460
      do 455 j=1,nbit
      if(j.eq.i)goto 455
      if(parm(1,j).lt.0.5)goto 455
      xydis=(parm(4,i)-parm(4,j))**2+(parm(2,i)-parm(2,j))**2
      xyold=(xcordl(i)-xcordl(j))**2+(ycordl(i)-ycordl(j))**2
      if(xydis.gt.dissq)goto 455
      if(parm(1,i).gt.parm(1,j))then
c *** must have moved 1/4 psf sigma
c *** if nearer than 1/4 psf throw out anyway
c *** must be moving toward image
      if(xydis.lt.range)goto 453
      xyref=(parm(4,j)-xcords(j))**2+(parm(2,j)-ycords(j))**2
	if(xyref.lt.range.or.xydis.gt.xyold)goto 455
  453	parm(1,j)=0.0
	iflag=1
c	type *,' TYPE A'
      else
      if(xydis.lt.range)goto 454
      xyref=(parm(4,i)-xcords(i))**2+(parm(2,i)-ycords(i))**2
	if(xyref.lt.range.or.xydis.gt.xyold)goto 455
  454 	parm(1,i)=0.0
	iflag=1
c	type *,' TYPE A'
      endif
  455 continue
  460 continue
  470 if(iflag.eq.1)nter=nter-1
c *** approx theoretical parameter errors
      do 480 i=1,nbit
      sxdel(i)=0.0
      sydel(i)=0.0
      xdamp(i)=1.0
      ydamp(i)=1.0
      xinttp=amax1(xintmn,parm(1,i))
      erri(i)=sqrt(xinttp*(1.0+0.5*const/xinttp))
      errc(i)=sqrt(psfsq*(1.0+const/xinttp)/(2.0*xinttp))
      if(nter.gt.1)errc(i)=0.5*errc(i)
c *** at least 1/100 th of a pixel
      errc(i)=amax1(0.01,errc(i))
  480 continue
c      type *,' Coordinate refinement'
      iter=0
  500 iter=iter+1
c *** compute model function for all pixels
      do 550 i=ill,ihh
      sum=0.0
      do 525 k=1,nbit
      if(parm(1,k).lt.0.5)goto 525
      x=parm(4,k)-ilist(i)
      y=parm(2,k)-jlist(i)
c *** replaced FUNC between here
      radsq=x*x+y*y
      rad=sqrt(radsq)
      if(rad.lt.parrad)then
	arg=parm1*radsq
	t=parmn*q/(1.0+parmn2*radsq)
	if(arg.gt.-16.0)t=t+(1.0-q)*parmn*exp(arg)
      else
	arg=change+(parrad-rad)*parm2
	t=parmn*q/(1.0+parmn2*radsq)
	if(arg.gt.-16.0)t=t+(1.0-q)*parmn*exp(arg)
      endif
c *** and here
      sum=sum+parm(1,k)*t
  525 continue
      xmodel(i-ill+1)=sum
  550 continue
c *** global convergence statistic
      if(iter.eq.1)then
       	do 580 k=1,nbit
  580	iref(k)=1
	chisqu=0.0
	icc=0
	do 590 i=ill,ihh
c *** miss out saturated pixels
 	if(klist(i).ge.iupp)goto 590
	icc=icc+1
	chisqu=chisqu+(xmodel(i-ill+1)-klist(i)+skycor)**2
  590	continue
	chisqu=chisqu/(sigsq*float(icc))
c       type *,nter,chisqu
c	if(chisqu.gt.0.99*chisqo)goto 780
	chisqo=chisqu
      endif
c *** now do x and y positions
      do 600 i=1,nbit
c *** save old coords
      xcordl(i)=parm(4,i)
      ycordl(i)=parm(2,i)
      xmn(i)=0.0
      tmn(i)=0.0
  600 ymn(i)=0.0
      do 700 i=ill,ihh
      if(xmodel(i-ill+1).lt.xmthr)goto 700
      do 650 k=1,nbit
      if(iref(k).eq.0)goto 650
      if(parm(1,k).lt.0.5)goto 650
      x=parm(4,k)-ilist(i)
      y=parm(2,k)-jlist(i)
      radsq=x*x+y*y
      arg=parm1*radsq
      if(arg.lt.-16.0)then
	dmodel=0.0
      else
        dmodel=exp(arg)
      endif
      temp=klist(i)*dmodel/xmodel(i-ill+1)
      xmn(k)=xmn(k)+temp*ilist(i)
      ymn(k)=ymn(k)+temp*jlist(i)
      tmn(k)=tmn(k)+temp
  650 continue
  700 continue
c *** check on errors and update
      iflag=0
      do 720 k=1,nbit
      if(tmn(k).eq.0.0)goto 720
      iref(k)=1
      if(parm(1,k).lt.0.5)goto 720
      xmn(k)=xmn(k)/tmn(k)
      ymn(k)=ymn(k)/tmn(k)
      xdel=xmn(k)-parm(4,k)
      ydel=ymn(k)-parm(2,k)
      if(xdel.gt.1.0)xdel=1.0
      if(xdel.lt.-1.0)xdel=-1.0
      if(ydel.gt.1.0)ydel=1.0
      if(ydel.lt.-1.0)ydel=-1.0
      if(abs(xdel)+abs(ydel).lt.0.5*errc(k))iref(k)=0
c *** test to stop oscillation
      if(xdel*sxdel(k).lt.0.0)xdamp(k)=xdamp(k)-0.25
      if(ydel*sydel(k).lt.0.0)ydamp(k)=ydamp(k)-0.25
      xdel=xdel*xdamp(k)
      ydel=ydel*ydamp(k)
      parm(4,k)=parm(4,k)+xdel
      parm(2,k)=parm(2,k)+ydel
      if(abs(xdel).gt.errc(k).or.abs(ydel).gt.errc(k))iflag=1
c      write(lt,7500) iter,parm(4,k),parm(2,k),xdel,ydel,errc(k),
c     1  iref(k)
c 7500 format(i5,5f10.2,i5)
      sxdel(k)=xdel
      sydel(k)=ydel
  720 continue
c *** convergence test for coordinates
      if(iflag.eq.1.and.iter.lt.5)goto 500
c *** check if can remove any images
      if(iextra.gt.0.and.nter.gt.1)then
	irem=0
	irlim=max0(1,nint(0.1*nbit))
	do 723 i=1,nbit
	if(parm(1,i).lt.0.5)goto 723
	do 722 j=1,nbit
	if(i.eq.j)goto 722
 	if(parm(1,j).lt.0.5)goto 722
	xydis=(parm(4,i)-parm(4,j))**2+(parm(2,i)-parm(2,j))**2
 	xyold=(xcordl(i)-xcordl(j))**2+(ycordl(i)-ycordl(j))**2
 	if(xydis.gt.psfsq.or.xydis.gt.xyold)goto 722
	if(parm(1,i).gt.parm(1,j))then
	  parm(1,j)=0.0
	else
	  parm(1,i)=0.0
	endif
c	type *,' TYPE B'
     	nter=nter-1
	irem=irem+1
c *** don't remove more than 10% at a time
	if(irem.eq.irlim)goto 724
  722	continue
  723	continue
      endif
  724 ii=0
      do 730 i=1,nbit
      if(parm(1,i).gt.0.5)then
	ii=ii+1
      else
	goto 730
      endif
      if(ii.eq.i)goto 730
      xcords(ii)=xcords(i)
      ycords(ii)=ycords(i)
      xcordl(ii)=xcordl(i)
      ycordl(ii)=ycordl(i)
      do 725 k=1,16
  725 parm(k,ii)=parm(k,i)
  730 continue
      nbit=ii
c *** now do intensities
c      type *,' Intensity refinement'
      do 750 i=1,nbit
  750 sint(i)=parm(1,i)
      call phopt2(ims,jcount)
c      write(lt,4600) skycor
c 4600 format(' Sky correction =',f8.2)
c *** check for convergence
      iflag=0
      do 770 i=1,nbit
      parm(1,i)=amax1(0.0,parm(1,i))
      if(sint(i).lt.yintmn)then
	parm(1,i)=0.0
	goto 770
      endif
      tdel=parm(1,i)-sint(i)
      if(abs(tdel).gt.erri(i))iflag=1
c      write(lt,7500) nter,parm(1,i),tdel,erri(i),sint(i)
  770 continue
      if(iflag.eq.1.and.nter.lt.10)goto 450
c *** check if any features left in diff map if doing full surface modelling
      if(iextra.eq.0.and.ifsm.eq.1)then
        call featur(xmodel,ims,jcount,minpix,sigma,range,iextra,edge)
c	if(iextra.gt.0)type *,' No. of extra images =',iextra
      else
  	goto 780
      endif
      if(iextra.gt.0)then
	nter=0
	skycor=0.0
	goto 450
      endif
c *** finish for blend
  780 avchi=avchi+float(jcount)*chisqu
      numchi=numchi+jcount
c *** see how many images and fill in parm array
      ii=0
      do 900 k=1,nbit
      if(parm(1,k).gt.zintmn)then
	ii=ii+1
      else
	goto 900
      endif
      if(ii.eq.k)goto 900
      do 800 i=1,16
  800 parm(i,ii)=parm(i,k)
  900 continue
      nbit=ii
      do 950 k=1,nbit
      parm(5,k)=0.5*psf
      parm(6,k)=0.0
      parm(7,k)=0.5*psf
      parm(8,k)=parm(1,k)*parmn
      parm(9,k)=pi*psfsq*alog(parm(8,k)/phresh)
      parm(9,k)=amax1(1.0,parm(9,k))
      parm(10,k)=-1.0
      parm(11,k)=-1.0
      parm(12,k)=-1.0
      parm(13,k)=-1.0
      parm(14,k)=-1.0
      parm(15,k)=-1.0
  950 parm(16,k)=-1.0
c      type *,' '
      return
      end
* $Id$
