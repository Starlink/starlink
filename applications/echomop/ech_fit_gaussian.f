      subroutine ech_fit_gaussian(data,error,nd,parm,parerr,np,idiag)

      INCLUDE 'ECH_REPORT.INC'

      integer nd
      real data(nd),error(nd),parm(np),parerr(np)
      real sgrad(25),grad(25),hessian(25,25)
      real bb(25,25)
      real xdat(11),polycf(3),xcor(11)
c *** UCOPT  unconstrained n dimensional optimisation using Gauss
c ***        Newton method with parameter search.
c ***        data    - data array continuum corrected.
c ***        error   - estimate of 1/var for each data point, routine returns
c ***                  model fit in this array.
c ***        nd      - no. of data points.
c ***        parm    - parameters for model (in this case 3 per Gaussian
c ***                  in order: peak height,position,sigma etc..). Initial
c ***                  guess required, routine returns refined values.
c ***        parerr  - routine returns estimates of parameter variances.
c ***        np      - no. of parameters must be 3m where m no. of Gaussians
c ***                  and not more than 8 Gaussians ie. 24 parameters.
      iter=-1

      itlim=25
      chisqo=1.0e6
      nfunc=np/3
c *** set up initial guess parameters
C! 1000 format(//'  Iteration   Statistic          Parameters'/)
      istart=1
      if(istart.ne.0)istart=1
C!      write(lt,1000)
   50 iter=iter+1
      iflag=0
c *** test for convergence
      sum=0.0
      alpha=0.0
      do 100 k=1,nd
      yk=k
      phi=0.0
      do 80 n=1,nfunc
      nn=3*(n-1)
   80 phi=phi+func(parm(nn+1),parm(nn+2),parm(nn+3),yk)
  100 sum=sum+(phi-data(k))**2*error(k)
      chisq=sum/float(nd)
C!      write(lt,2000) iter,chisq,(parm(i),i=1,np)
C! 2000 format(i6,8x,e10.3,5x,3f10.3,8(/27x,3f10.3))
      if(iter.ge.itlim)goto 500
      if(chisq.le.0.998*chisqo)goto 110
      if(istart.eq.1)goto 500
      istart=1
  110 chisqo=chisq
c *** compute search direction
      call deriv(data,error,nd,parm,np,grad,hessian,idiag)
c *** store gradient for later use if necessary
      do 125 i=1,np
      if(istart.eq.0.and.(i/3)*3.eq.i)then
        sgrad(i)=0.0
      else
        sgrad(i)=grad(i)
      endif
  125 continue
c *** returns in grad as -ve direction
      call usolve(hessian,grad,np,istart)
      if(idiag.eq.1)then
C!      write(lt,3000)
C! 3000 format(/' Newton gradient vector')
C!      write(lt,3500) (grad(i),i=1,np)
C! 3500 format(6f10.3)
C!      write(lt,4500)
C! 4500 format(/' Steepest descent gradient vector')
C!      write(lt,3500) (sgrad(i),i=1,np)
      endif
c *** compute step length  -  alpha = 1.0 is quadratic model version
      step=0.125
  130 if(iflag.eq.1)step=defstp
      stpmin=0.0
      sumold=sum
      xdat(1)=sum
      xcor(1)=0.0
      if(idiag.eq.1) THEN
         WRITE ( report_string , 1003 ) sum
         CALL ECH_REPORT ( 0 , report_string )
      ENDIF
      do 250 jj=1,10
      xcor(jj+1)=step
      sumnew=0.0
      do 200 k=1,nd
      yk=k
      phi=0.0
      do 150 n=1,nfunc
      nn=3*(n-1)
      p1=parm(nn+1)-step*grad(nn+1)
      p2=parm(nn+2)-step*grad(nn+2)
      p3=parm(nn+3)-step*grad(nn+3)
  150 phi=phi+func(p1,p2,p3,yk)
  200 sumnew=sumnew+(phi-data(k))**2*error(k)
      if(idiag.eq.1) THEN
         WRITE ( report_string , 1001 ) sumnew
         CALL ECH_REPORT ( 0 , report_string )
      ENDIF
      xdat(jj+1)=sumnew
      if(sumnew.gt.sumold)goto 275
      sumold=sumnew
      stpmin=step
      step=step*2.0
  250 continue
  275 ic=jj+1
      if(iflag.eq.1)then
        alpha=stpmin
        goto 295
      endif
      if(ic.gt.2)then
c *** if chiqu decrease in Newton direction ok
      call polynm(xdat,xcor,ic,polycf,3,0)
      if(polycf(3).eq.0.0)then
        alpha=1.0
      else
        peak=-polycf(2)/(2.0*polycf(3))
        alpha=peak
      endif
      if(idiag.eq.1) THEN
         WRITE ( report_string , 1002 ) alpha
         CALL ECH_REPORT ( 0 , report_string )
      ENDIF
c *** otherwise use the steepest descent direction
      else
      do 290 i=1,np
  290 grad(i)=sgrad(i)
      iflag=1
c *** find a suitable step length
      ratio=0.0
      nratio=0
      do 285 i=1,np
      if(parm(i).eq.0.0)goto 285
      nratio=nratio+1
      ratio=ratio+abs(grad(i))/abs(parm(i))
  285 continue
      ratio=ratio/float(nratio)
c *** set def step to be 0.1% of parms
      defstp=0.001/ratio
      goto 130
      endif
c *** update parm
  295 continue
      do 300 k=1,np
  300 parm(k)=parm(k)-alpha*grad(k)
c *** constrain sigmas to be >0.5
      do 400 kk=3,np,3
  400 parm(kk)=amax1(0.5,parm(kk))
      goto 50
  500 continue
c *** compute error function
      chisq=chisq*float(nd)/float(nd-np)
      call deriv(data,error,nd,parm,np,grad,hessian,idiag)
      call matinv(hessian,bb,np)
      do 600 k=1,np
  600 parerr(k)=chisq*bb(k,k)
c *** put fitted profile into error array
      do 950 k=1,nd
      yk=k
      phi=0.0
      do 925 n=1,nfunc
      nn=3*(n-1)
  925 phi=phi+func(parm(nn+1),parm(nn+2),parm(nn+3),yk)
  950 error(k)=phi

 1003  FORMAT ( 1X , 'Gaussian sum = ',F12.2 )
 1001  FORMAT ( 1X , 'Gaussian sumnew = ',F12.2 )
 1002  FORMAT ( 1X , 'Gaussian alpha = ',F12.2 )


      end


      function func(p1,p2,p3,y)
c *** FUNC computes individual functions for 3 parameters
      arg=(y-p2)**2/(2.0*p3)
      if(arg.le.0.0)then
        func=0.0
        return
      endif
      arg=amin1(arg,50.0)
      func=p1*exp(-arg)
      return
      end


      subroutine usolve(a,b,m,istart)
      dimension a(25,25),b(25)
      dimension aa(25,25),bb(25)
c *** gauss elimination to solve ax=b
      if(istart.eq.0)then
c *** if istart=0 shrink down equations to miss out 3rd parameters
      do 10 i=1,m
      bb(i)=b(i)
      do 10 j=1,m
   10 aa(i,j)=a(i,j)
      ii=0
      do 20 i=1,m
      if((i/3)*3.eq.i)goto 20
      ii=ii+1
      jj=0
      do 15 j=1,m
      if((j/3)*3.eq.j)goto 15
      jj=jj+1
      a(ii,jj)=aa(i,j)
   15 continue
      b(ii)=b(i)
   20 continue
      mm=(2*m)/3
      else
      mm=m
      endif
      iu=mm-1
      do 200 i=1,iu
c *** find largest remaining term in ith column for pivot
      big=0.0
      do 50 k=i,mm
      rmax=abs(a(k,i))
      if(rmax-big)50,50,30
   30 big=rmax
      l=k
   50 continue
c *** check for non-zero term
      if(big.eq.0.0)then
        do kk=1,m
        b(kk)=0.0
        enddo
        return
      endif
      if(i.eq.l)goto 120
c *** switch rows
      do 100 j=1,mm
      temp=a(i,j)
      a(i,j)=a(l,j)
  100 a(l,j)=temp
      temp=b(i)
      b(i)=b(l)
      b(l)=temp
c *** pivotal reduction
  120 pivot=a(i,i)
      jl=i+1
      do 200 j=jl,mm
      temp=a(j,i)/pivot
      b(j)=b(j)-temp*b(i)
      do 200 k=i,mm
  200 a(j,k)=a(j,k)-temp*a(i,k)
c *** back substitution for solution
      do 500 i=1,mm
      ir=mm+1-i
      if(a(ir,ir).eq.0.0)goto 490
      temp=b(ir)
      if(ir.eq.mm)goto 480
      do 450 j=2,i
      k=mm+2-j
  450 temp=temp-a(ir,k)*b(k)
  480 b(ir)=temp/a(ir,ir)
      goto 500
  490 b(ir)=0.0
  500 continue
c *** if istart=0 readjust b array
      if(istart.ne.0)return
      do 600 i=1,mm
  600 bb(i)=b(i)
      ii=0
      do 700 i=1,m
      if((i/3)*3.ne.i)then
      ii=ii+1
      b(i)=bb(ii)
      else
      b(i)=0.0
      endif
  700 continue
      return
      end


      subroutine deriv(data,error,nd,parm,np,grad,hessian,idiag)
      real data(nd),error(nd),parm(nd),grad(np),hessian(25,25)
      real f(25),bb(625),aa(625)
c *** DERIV  computes gradient vector and Hessian matrix for ls
c ***        Gaussian fitting  max. of 8 functions
      nfunc=np/3
c *** clear arrays
      do 100 j=1,np
      grad(j)=0.0
      do 100 i=1,np
  100 hessian(i,j)=0.0
c *** main loop for summing in data points
      do 500 k=1,nd
      yk=k
      pphi=0.0
      do 125 n=1,nfunc
      nn=3*(n-1)
  125 pphi=pphi+func(parm(nn+1),parm(nn+2),parm(nn+3),yk)
      do 150 n=1,nfunc
      nn=(n-1)*3
      ymx=yk-parm(nn+2)
      if ( parm(nn+3) .NE. 0.0 ) ymxox=ymx/parm(nn+3)
      arg=ymx**2/(2.0*parm(nn+3))
      phi=parm(nn+1)*exp(-arg)
      if ( parm(nn+1) .NE. 0.0 ) f(nn+1)=phi/parm(nn+1)
      f(nn+2)=ymxox*phi
      if ( parm(nn+3) .NE. 0.0 ) f(nn+3)=arg*phi/parm(nn+3)
  150 continue
      diff=error(k)*(pphi-data(k))
      do 200 j=1,np
      grad(j)=grad(j)+f(j)*diff
      do 200 i=j,np
  200 hessian(i,j)=hessian(i,j)+f(i)*f(j)*error(k)
  500 continue
c *** pad out rest of Hessian
      do 600 j=2,np
      jj=j-1
      do 600 i=1,jj
  600 hessian(i,j)=hessian(j,i)
      if(idiag.lt.2)return
c!      write(6,1000)
c! 1000 format(/' Hessian matrix'/)
      do 700 i=1,np
c!      write(6,2000) (hessian(i,j),j=1,np)
c! 2000 format(6f10.3)
  700 continue
c *** compute eigenvalues and eigenvectors
      do 725 i=1,np
      ii=(i-1)*np
      do 725 j=1,np
  725 aa(ii+j)=hessian(i,j)
      call eigen(aa,bb,np)
      do 710 i=1,np
  710 f(i)=aa(i+(i-1)*np)
c!      write(6,1300)
c! 1300 format(/' Eigenvalues'/)
c!      write(6,2000) (f(i),i=1,np)
c!      write(6,1500)
c! 1500 format(/' Eigenvectors'/)
      do 750 k=1,np
      kk=(k-1)*np
C!      write(6,2000) (bb(i+kk),i=1,np)
  750 continue
      return
      end


      subroutine polynm(xdat,xcor,n,polycf,m,ilim)
      dimension xdat(n),xcor(n),polycf(m),a(25,25),b(25)
c *** least-squares fit of order m polynomial to n data points
      if(n.lt.m)stop ' too few data points'
      if(m.gt.25)stop ' order of polynomial too large'
c *** clear arrays
      do 50 i=1,25
      b(i)=0.0
      do 50 j=1,25
   50 a(i,j)=0.0
c *** cumulate sums
      do 200 i=1,n
      do 100 k=1,m
      temp=1.0
      if(k+ilim-1.eq.0)goto 80
      temp=xcor(i)**(k-1+ilim)
   80 b(k)=b(k)+xdat(i)*temp
      do 100 j=1,k
      temp=1.0
      if(k+j-2+2*ilim.eq.0)goto 90
      temp=xcor(i)**(k+j-2+2*ilim)
   90 a(k,j)=a(k,j)+temp
  100 continue
  200 continue
      do 300 k=2,m
      ju=k-1
      do 300 j=1,ju
  300 a(j,k)=a(k,j)
c *** solve linear equations
      call solve(a,b,m)
      do 400 i=1,m
  400 polycf(i)=b(i)
      return
      end



      subroutine matinv(a,b,m)
      real a(25,25),b(25,25),pivot,temp,det
c *** MATINV  inverts matrix A and returns inverse in B
c ***         uses a modified Gauss elimination process
c ***         on Ax=I
      iu=m-1
      l=1

c *** initialise B as unit matrix
      do 20 i=1,m
      do 10 j=1,m
   10 b(i,j)=0.0
   20 b(i,i)=1.0
      do 200 i=1,iu
c *** find largest remaining term in ith column for pivot
      big=0.0
      do 50 k=i,m
      pivot=a(k,i)
      rmax=abs(pivot)
      if(rmax-big)50,50,30
   30 big=rmax
      l=k
   50 continue
c *** check for non-zero term
      if(big.eq.0.0)then
        do ik=1,m
        b(ik,ik)=0.0
        enddo
      endif
      if(i.eq.l)goto 120
c *** switch rows
      do 100 j=1,m
      temp=a(i,j)
      a(i,j)=a(l,j)
      a(l,j)=temp
      temp=b(i,j)
      b(i,j)=b(l,j)
  100 b(l,j)=temp
c *** pivotal reduction
  120 pivot=a(i,i)
      jl=i+1
      do 180 j=jl,m
      if ( pivot .NE. 0.0 ) temp=a(j,i)/pivot
      do 140 k=i,m
  140 a(j,k)=a(j,k)-temp*a(i,k)
      do 160 k=1,m
  160 b(j,k)=b(j,k)-temp*b(i,k)
  180 continue
  200 continue
c *** compute determinant
      det=1.0
      do 250 i=1,m
  250 det=det*a(i,i)
ccccccwrite(li,1200)cdet
c1200cformat(/,'cDeterminantcofcmatrix',e15.5/)
c *** back substitution for solution
      do 500 j=1,m
      do 400 i=1,m
      ir=m+1-i
      temp=b(ir,j)
      if(ir.eq.m)goto 400
      do 300 k=2,i
      kk=m+2-k
  300 temp=temp-a(ir,kk)*b(kk,j)
  400 if ( a(ir,ir) .NE. 0.0 ) b(ir,j)=temp/a(ir,ir)
  500 continue
      return
      end


      subroutine eigen(aa,vv,m)
      dimension a(100,100),v(100,100),aa(1),vv(1)
c *** Jacobi method for calculating eigenvalues and eigenvectors
c *** of a real symmetric matrix. (Serial-threshold variant)
      do 25 i=1,m
      do 25 j=1,m
      ii=(i-1)*m+j
   25 a(i,j)=aa(ii)
      n=m-1
      tol=1.0e-6
      icnt=0
c *** initialise arrays
      do 100 i=1,m
      do 50 j=1,m
   50 v(i,j)=0.0
  100 v(i,i)=1.0
c *** find maximum diagonal and off-diagonal elements
  200 rmaxd=abs(a(1,1))
      rmaxo=0.0
      do 250 i=2,m
      rmaxd=amax1(rmaxd,abs(a(i,i)))
      iu=i-1
      do 250 j=1,iu
  250 rmaxo=amax1(rmaxo,abs(a(i,j)))
      if(rmaxo.lt.tol*rmaxd)goto 900
C!      if ( rmaxd .NE. 0.0 ) ratio=rmaxo/rmaxd
      if(icnt.lt.30)goto 275
C!      write(6,1000) icnt,ratio
C! 1000 format(' Iteration failed to converge in',i5,' cycles',/,
C!     1 ' Ratio of maximum off-diagonal to diagonal term',e15.5/)
      return
  275 continue
c *** apply rotation matrix to minimise off-diagonal terms
      do 500 i=1,n
      jl=i+1
      aii=a(i,i)
      do 400 j=jl,m
      aij=a(i,j)
      if(abs(aij).lt.0.2*rmaxo)goto 400
      ajj=a(j,j)
      if(ajj.eq.aii)goto 300
      t=aij/(ajj-aii)
      ta=abs(t)
      if(ta.gt.1.0)t=t/ta
      goto 310
  300 t=1.0
      if(aij.lt.0.0)t=-t
  310 g=t/(2.0*(1.0+t*t))
      sn=2.0*g/(1.0+g*g)
      cs=1.0-g*sn
      do 320 k=1,m
      akj=a(k,j)
      aki=a(k,i)
      a(k,i)=cs*aki-sn*akj
  320 a(k,j)=sn*aki+cs*akj
      do 340 k=1,m
      aik=a(i,k)
      ajk=a(j,k)
      a(i,k)=cs*aik-sn*ajk
      a(j,k)=sn*aik+cs*ajk
      vki=v(k,i)
      vkj=v(k,j)
      v(k,i)=cs*vki-sn*vkj
  340 v(k,j)=sn*vki+cs*vkj
  400 continue
  500 continue
      icnt=icnt+1
      goto 200
  900 continue
c *** put answers in output arrays
      do 950 i=1,m
      do 950 j=1,m
      ii=(i-1)*m+j
      aa(ii)=a(i,j)
  950 vv(ii)=v(i,j)
      return
      end



      subroutine solve(a,b,m)
      dimension a(25,25),b(25)
c *** gauss elimination to solve ax=b
      iu=m-1
      do 200 i=1,iu
c *** find largest remaining term in ith column for pivot
      big=0.0
      do 50 k=i,m
      rmax=abs(a(k,i))
      if(rmax-big)50,50,30
   30 big=rmax
      l=k
   50 continue
c *** check for non-zero term
      if(big.eq.0.0)then
        do ib=1,m
        b(ib)=0.0
        enddo
        return
      endif
      if(i.eq.l)goto 120
c *** switch rows
      do 100 j=1,m
      temp=a(i,j)
      a(i,j)=a(l,j)
  100 a(l,j)=temp
      temp=b(i)
      b(i)=b(l)
      b(l)=temp
c *** pivotal reduction
  120 pivot=a(i,i)
      jl=i+1
      do 200 j=jl,m
      if ( pivot .NE. 0.0 ) temp=a(j,i)/pivot
      b(j)=b(j)-temp*b(i)
      do 200 k=i,m
  200 a(j,k)=a(j,k)-temp*a(i,k)
c *** back substitution for solution
      do 500 i=1,m
      ir=m+1-i
      if(a(ir,ir).eq.0.0)goto 490
      temp=b(ir)
      if(ir.eq.m)goto 480
      do 450 j=2,i
      k=m+2-j
  450 temp=temp-a(ir,k)*b(k)
  480 if ( a(ir,ir) .NE. 0.0 ) b(ir)=temp/a(ir,ir)
      goto 500
  490 b(ir)=0.0
  500 continue
      return
      end
