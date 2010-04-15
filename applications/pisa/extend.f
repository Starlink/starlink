      subroutine extend(map,parm,xpeak,r,sigsq,icirc,rcirc)
      integer*2 map(*)
      real*4 xdat(10),polycf(4),xxxx(10)
      real*4 parm(16),accum(10)
      common /fil/ istart,istop,nword,ixl,ixh
c *** EXTEND  does aperture integration a la Kron except using
c ***         matched ellipses.
      pi=4.0*atan(1.0)
      pio2=pi/2.0
      radeg=180.0/pi
      lt=6
      nyout=ixh-ixl+1
c *** get image parms
      xniso=parm(1)
      xcor=parm(4)
      ycor=parm(2)
      sxx=parm(5)
      sxy=parm(6)
      syy=parm(7)
      srr=amax1(0.5,sxx+syy)
      ecc=sqrt((sxx-syy)**2+4.0*sxy**2)/srr
      ecc=amin1(0.9,ecc)
      xx=0.5*(1.0+ecc)*srr-syy
      if(sxy.eq.0.0)theta=0.0
      if(xx.eq.0.0)then
	theta=pio2
      else
        theta=atan(sxy/xx)
      endif
      ctheta=cos(theta)
      stheta=sin(theta)
c *** ecc modified by noise effect
      eccold=ecc
c *** 50 approx 16*pi
      ecc=sqrt(amax1((sxx-syy)**2-50.0*sigsq*srr**3/(xniso**2)
     1  +4.0*sxy**2,0.0))/srr
      ecc=amin1(0.9,ecc)
c *** set initial aperture to be isophotal area
      a=sqrt(srr*(1.0+ecc))
      b=sqrt(srr*(1.0-ecc))
      strech=sqrt(parm(9)/(pi*a*b))
c *** no. of isophotal radii to extend
      sfac=3.0/sqrt(alog(r))
      sfac=amax1(2.0,sfac)
      sfac=amin1(5.0,sfac)
      a=sfac*a*strech
      b=sfac*b*strech
      if(icirc.eq.1)then
	a=rcirc
	b=rcirc
      endif
c *** clear accumulators
      temp=radeg*theta
      do 100 i=1,10
  100 accum(i)=0.0
c *** generate images boundaries
      climsq=(a*ctheta)**2+(b*stheta)**2
      climsq=amax1(1.0,climsq)
      clim=sqrt(climsq)
      pt1=sin(2.0*theta)*(b**2-a**2)
      pt2=(b*ctheta)**2+(a*stheta)**2
      pt3=(a*b)**2
      irecl=max0(istart,int(xcor-clim))
      irech=min0(istop,int(xcor+clim+1.0))
      do 500 ii=irecl,irech
      kk=(ii-istart)*nyout
      c=ii-xcor
      pa=climsq
      pb=pt1*c
      pc=pt2*c**2-pt3
      arg1=pb**2-4.0*pa*pc
      arg1=sqrt(amax1(arg1,0.0))
      yliml=(-pb-arg1)/(2.0*pa)
      ylimu=(-pb+arg1)/(2.0*pa)
      iyliml=max0(ixl,int(ycor+yliml))
      iylimu=min0(ixh,int(ycor+ylimu+1.0))
      x=c
      do 400 i=iyliml,iylimu
      t=map(kk+i-ixl+1)-xpeak
      y=i-ycor
c *** find ellipse radius of pt
      xnew=x*ctheta+y*stheta
      ynew=-x*stheta+y*ctheta
      ellrad=2.0*sqrt((xnew/a)**2+(ynew/b)**2)
      iupd=int((2.0-ellrad)*10.0)+1
      iupd=max0(1,iupd)
      iupd=min0(10,iupd)
      do 350 j=1,iupd
  350 accum(11-j)=accum(11-j)+t
  400 continue
  500 continue
c *** now find limiting intensity
      if(icirc.eq.1)then
	parm(1)=accum(10)
	return
      endif
      if(parm(1).lt.0.0)then
	do 520 i=1,10
  520 	accum(i)=-accum(i)
      endif
ccccccwrite(lt,1000)caccum
c1000cformat(10f8.1/)
      call median(accum,10,3)
ccccccwrite(lt,1000)caccum
      xmax=0.0
      xlim1=-1.0
      xlim2=-1.0
      do 600 i=1,10
      xxxx(i)=i
      xmax=amax1(xmax,accum(i))
  600 xdat(i)=accum(i)
      call polynm(xdat,xxxx,10,polycf,4,0)
      pa=polycf(2)
      pb=polycf(3)*2.0
      pc=polycf(4)*3.0
      arg=sqrt(amax1(0.0,pb**2-4.0*pa*pc))
      if(pc.eq.0.0)goto 950
      rt1=(-pb+arg)/(2.0*pc)
      rt2=(-pb-arg)/(2.0*pc)
      if(rt1.lt.10.0.and.rt1.gt.1.0)then
	ir=int(rt1)
	t1=rt1-ir
	xlim1=(1.0-t1)*accum(ir)+t1*accum(ir+1)
      endif
      if(rt2.lt.10.0.and.rt2.gt.1.0)then
	ir=int(rt2)
	t1=rt2-ir
	xlim2=(1.0-t1)*accum(ir)+t1*accum(ir+1)
      endif
  950 xlimit=amax1(xlim1,xlim2)
      if(xlimit.lt.0.0)xlimit=xmax
cccccctypec*,xlimit,xmax,xlim1,xlim2
c *** update buffers
      if(parm(1).lt.0.0)xlimit=-xlimit
      parm(1)=xlimit
      return
      end
* $Id$
