      subroutine sortin(ia,ib,ic,id,n)
      integer*2 ia(n),ib(n),ic(n),id(n)
      int=2
   10 int=2*int
      if(int.lt.n)goto 10
      int=min0(n,(3*int)/4-1)
   20 int=int/2
      ifin=n-int
      do 70 ii=1,ifin
      i=ii
      j=i+int
      if(ia(i).le.ia(j))goto 70
      it=ia(j)
      iu=ib(j)
      iv=ic(j)
      iw=id(j)
   40 ia(j)=ia(i)
      ib(j)=ib(i)
      ic(j)=ic(i)
      id(j)=id(i)
      j=i
      i=i-int
      if(i.le.0)goto 60
      if(ia(i).gt.it)goto 40
   60 ia(j)=it
      ib(j)=iu
      ic(j)=iv
      id(j)=iw
   70 continue
      if(int.gt.1)goto 20
      return
      end
* $Id$
