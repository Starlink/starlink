      subroutine phopt(map,parm,nbit,rec,xpeak,r,sigsq)
      integer*2 map(*)
      real*4 parm(16,200),rec(16)
      real*8 aa(201,201),bb(201)
      byte cc(200,200)
      common /fil/ istart,istop,nword,ixl,ixh
      common /pm/ c1,c2,cn,parsq,change,parrad,iupp,skycor,ib
      common /pmn/ parmn1,parmn2,parmnn,q,imodel
c *** PHOPT  does multiple profile fitting to determine intensities
      pi=4.0*atan(1.0)
      pio2=pi/2.0
      lt=6
      fupp=iupp
      nyout=ixh-ixl+1
c *** set up flag array matrix
      cc(1,1)=1
      if(nbit.gt.1)then
	do 20 i=1,nbit
	xi=parm(4,i)
	yi=parm(2,i)
	jl=i+1
	cc(i,i)=1
	do 20 j=jl,nbit
	if((xi-parm(4,j))**2+(yi-parm(2,j))**2.gt.parsq)then
	cc(j,i)=0
	cc(i,j)=0
	else
	cc(j,i)=1
	cc(i,j)=1
	endif
   20 	continue
      endif
c *** get image parms
      xniso=rec(1)
      xcor=rec(4)
      ycor=rec(2)
      sxx=rec(5)
      sxy=rec(6)
      syy=rec(7)
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
c *** set initial aperture to be isophotal area
      a=sqrt(srr*(1.0+ecc))
      b=sqrt(srr*(1.0-ecc))
      strech=sqrt(rec(9)/(pi*a*b))
c *** no. of isophotal radii to extend
      sfac=2.5/sqrt(alog(r))
      sfac=amax1(2.0,sfac)
      sfac=amin1(3.0,sfac)
      a=sfac*a*strech
      b=sfac*b*strech
c *** find max elliptical radius of input points
c *** and alter a,b if necessary
      if(nbit.gt.1)then
	ratio=b/a
	rnew=0.0
	do 50 i=1,nbit
	call checkp(parm(4,i),parm(2,i),xcor,ycor,ratio,theta,rsq)
   50	rnew=amax1(rnew,(sqrt(rsq)+1.0)/a)
	if(rnew.gt.1.0)then
	a=a*rnew
	b=b*rnew
	endif
      endif
c *** clear accumulators
      do 100 i=1,nbit
      bb(i)=0.d0
      do 100 j=1,nbit
  100 aa(j,i)=0.d0
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
      do 400 i=iyliml,iylimu
      t=map(kk+i-ixl+1)-xpeak
      if(t.ge.fupp)goto 400
      do 300 j=1,nbit
      xj=ii-parm(4,j)
      yj=i-parm(2,j)
c *** replace FUNC directly between here
      radsq=xj*xj+yj*yj
      rad=sqrt(radsq)
      if(rad.lt.parrad)then
	arg=c1*radsq
	tj=cn*q/(1.0+parmn2*radsq)
	if(arg.gt.-16.0)tj=tj+(1.0-q)*cn*exp(arg)
      else
	arg=change+(parrad-rad)*c2
	tj=cn*q/(1.0+parmn2*radsq)
	if(arg.gt.-16.0)tj=tj+(1.0-q)*cn*exp(arg)
      endif
c *** and here
      bb(j)=bb(j)+tj*t
      do 300 k=j,nbit
      if(cc(k,j).eq.0)goto 300
      if(k.eq.j)then
	tk=tj
      else
	xk=ii-parm(4,k)
	yk=i-parm(2,k)
c *** replaced FUNC directly between here
	radsq=xk*xk+yk*yk
	rad=sqrt(radsq)
	if(rad.lt.parrad)then
	  arg=c1*radsq
	  tk=cn*q/(1.0+parmn2*radsq)
	  if(arg.gt.-16.0)tk=tk+(1.0-q)*cn*exp(arg)
 	else
	  arg=change+(parrad-rad)*c2
	  tk=cn*q/(1.0+parmn2*radsq)
	  if(arg.gt.-16.0)tk=tk+(1.0-q)*cn*exp(arg)
	endif
c *** and here
      endif
      aa(k,j)=aa(k,j)+tk*tj
  300 continue
  400 continue
  500 continue
c *** pad out rest of matrix
      if(nbit.gt.1)then
	do 550 j=2,nbit
	ju=j-1
	do 550 k=1,ju
  550 	aa(k,j)=aa(j,k)
      endif
c *** solve for profile intensities
      call dchole(aa,bb,nbit)
      do 600 i=1,nbit
  600 parm(1,i)=bb(i)
      return
      end
* $Id$
