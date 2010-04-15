      subroutine analys
      integer*2 ijist(10000),jjist(10000),kjist(10000),njist(10000)
      integer*2 silist(10000),sjlist(10000),sklist(10000)
      common /an/ ijist,jjist,kjist,njist,silist,sjlist,sklist,npt
c *** ANALYS  does isophotal analysis on lists from OVERLP
c ***         these lists only contain pixels above threshold.
      ijc=0
      ijcold=0
      ijcpre=0
c *** sort pixel lists on i
      call sorti3(silist,sjlist,sklist,npt)
      iold=silist(1)
      nobj=0
      do 400 n=1,npt
c *** is i j neighbour to any in ijlist
      i=silist(n)
      j=sjlist(n)
      if(i.ne.iold)then
	iold=i
	ijcold=ijcpre
	ijcpre=ijc
      endif
      if(ijc.eq.0)goto 380
      kl=ijcold+1
      kref=kl
      ku=ijc
      if(kl.gt.ku)goto 380
  275 continue
      do 300 k=kl,ku
      idel1=ijist(k)-i
      idel2=jjist(k)-j
      if(idel1**2+idel2**2.gt.1)goto 300
      goto 320
  300 continue
      if(kl.eq.kref)goto 380
      goto 400
c *** addition to present group
  320 if(kl.eq.kref)goto 360
      numb=njist(k)
      newnum=njist(ijc)
      if(numb.eq.newnum)goto 350
c *** check for branched objects
      do 340 l=1,ku
      if(njist(l).eq.numb)njist(l)=newnum
  340 continue
  350 kl=k+1
      if(kl.gt.ku)goto 400
      goto 275
  360 ijc=ijc+1
      ijist(ijc)=i
      jjist(ijc)=j
      kjist(ijc)=sklist(n)
      njist(ijc)=njist(k)
      kl=k+1
      if(kl.gt.ku)goto 400
      goto 275
c *** new object
  380 nobj=nobj+1
      ijc=ijc+1
      ijist(ijc)=i
      jjist(ijc)=j
      kjist(ijc)=sklist(n)
      njist(ijc)=nobj
  400 continue
c *** sort objects in ascending order on njist
      call sortin(njist,ijist,jjist,kjist,ijc)
      return
      end
* $Id$
