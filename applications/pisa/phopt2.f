      subroutine phopt2(ims,jcount)
      integer*2 ilist(75000),jlist(75000),klist(75000)
      real*4 parm(16,200)
      real*8 aa(201,201),bb(201)
      byte cc(200,200)
      common /ov/ ilist,jlist,klist,thresh,ipix,parm,nbit,const,offset,
     1            ianal
      common /fil/ istart,istop,nword,ixl,ixh
      common /pm/ c1,c2,cn,parsq,change,parrad,iupp,skycor,ib
      common /pmn/ parmn1,parmn2,parmnn,q,imodel
c *** PHOPT2  does multiple profile fitting to determine intensities
c ***         using only pixels within isophote
      lt=6
      jl=ims
      jh=ims+jcount-1
      nparm=nbit+1
c *** set up flag array matrix
      cc(1,1)=1
      if(nbit.gt.1)then
	do 20 i=1,nbit
	xi=parm(4,i)
	yi=parm(2,i)
	jjl=i+1
	cc(i,i)=1
	do 20 j=jjl,nbit
	if((xi-parm(4,j))**2+(yi-parm(2,j))**2.gt.parsq)then
	cc(j,i)=0
	cc(i,j)=0
	else
	cc(j,i)=1
	cc(i,j)=1
	endif
   20 	continue
      endif
c *** clear accumulators
      do 100 i=1,nparm
      bb(i)=0.d0
      do 100 j=1,nparm
  100 aa(j,i)=0.d0
c *** main pixel loop
      do 400 nn=jl,jh
      if(klist(nn).ge.iupp)goto 400
      ii=ilist(nn)
      i=jlist(nn)
      t=klist(nn)
      aa(nparm,nparm)=aa(nparm,nparm)+1.0
      bb(nparm)=bb(nparm)+t
      do 300 j=1,nbit
      xj=ii-parm(4,j)
      yj=i-parm(2,j)
c *** replaced FUNC between here
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
      aa(j,nparm)=aa(j,nparm)+tj
      do 300 k=j,nbit
      if(cc(k,j).eq.0)goto 300
      if(k.eq.j)then
	tk=tj
      else
	xk=ii-parm(4,k)
	yk=i-parm(2,k)
c *** replace FUNC between here
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
c *** pad out rest of matrix
      if(nbit.gt.1)then
	do 550 j=2,nbit
	ju=j-1
	do 550 k=1,ju
  550 	aa(k,j)=aa(j,k)
      endif
      do 575 j=1,nbit
  575 aa(nparm,j)=aa(j,nparm)
c *** solve for profile intensities
      if(ib.eq.1)then
	call dchole(aa,bb,nparm)
	skycor=bb(nparm)
      else
	call dchole(aa,bb,nbit)
	skycor=0.0
      endif
      do 600 i=1,nbit
  600 parm(1,i)=bb(i)
      return
      end
* $Id$
