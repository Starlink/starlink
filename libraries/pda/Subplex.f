      program demo
c
c
c coded by
c
c Tom Rowan
c Oak Ridge National Laboratory
c Mathematical Sciences Section
c P.O. Box 2008, Bldg. 6012
c Oak Ridge, TN 37831-6367
c
c Phone: (615) 574-3131
c Email: na.rowan@na-net.ornl.gov
c
c
c This program uses pda_subplx to minimize the function fun.
c
c constants
c     See pda_subplx comments for storage requirements.
c
      integer niwmax,nwmax,nxmax
c
      parameter (niwmax=300,nwmax=300,nxmax=300)
c
c local variables
c
      integer i,iflag,iwork(niwmax),
     *        maxnfe,mdcont,mdsing,mduser,mode,
     *        nf1,nf2,nfe,nfinc,n
      double precision    fx,scale(nxmax),scl,
     *        tol,tol1,tol2,tolfac,work(nwmax),x(nwmax)
      character*26 flgdsc(-2:2)
c
c subroutines and functions
c
      double precision fun
      external fun,pda_subplx
c
c data
c
      data flgdsc /'invalid input',
     *             'maxnfe exceeded',
     *             'tol satisfied',
     *             'limit of machine precision',
     *             'fstop reached'/
c
c-----------------------------------------------------------
c
      print *,'********************************************'
      print *,'****  pda_subplx minimization of fun  ******'
      print *,'********************************************'
c
c For descriptions of pda_subplx arguments see pda_subplx comments.
c
      read *, n
c
c The following two read statements determine when pda_subplx
c is interrupted so the user can examine intermediate
c results.  pda_subplx can be interrupted and then continued as
c if no interrupt had occured when an optimization tolerance
c is satisfied and/or when the maximum number of objective
c function evaluations is reached.
c
c Variables that define the sequence of tolerances.
c If tol = 0 is used, pda_subplx will optimize to the limits
c of machine precision.
c See pda_subplx comments for description of tol.
c
      read *, tol1,tol2,tolfac
c
c Variables that define the sequence of maximum number of
c function evaluations.
c See pda_subplx comments for description of maxnfe.
c
      read *, nf1,nf2,nfinc
c
c Set initial stepsizes for optimization.
c See pda_subplx comments for description of scale.
c
      read *, scl
      scale(1) = -abs(scl)
c
c Set starting point for optimization.
c See pda_subplx comments for description of x.
c
      read *, (x(i),i=1,n)
c
      print *, 'n =    ',n
      print *, 'tol1,tol2,tolfac=',tol1,tol2,tolfac
      tol = tol1
      print *, 'nf1,nf2,nfinc=',nf1,nf2,nfinc
      maxnfe = nf1
      print *, 'scale =',scale(1)
      print *, 'x0 ='
      write (6,1005) (x(i),i=1,n)
 1005 format (t3,5e15.5)
c
c Print output headers.
c
        write (6,1010)
 1010   format (///t5,'maxnfe',t15,'tol',t30,
     *          'fx',t45,'nfe',t60,'iflag'/)
c
c Set pda_subplx's operating mode.
c See pda_subplx comments for description of mode.
c
c First call to pda_subplx so continuation mode is off.
c
        mdcont = 0
c
c Using default options so user options mode is off.
c
        mduser = 0
c
c Using optimization so single-step mode is off.
c
        mdsing = 0
c
   20   continue
c
        mode = 4*mdsing + 2*mduser + mdcont
c
        call pda_subplx (fun,n,tol,maxnfe,mode,scale,x,fx,nfe,
     *                     work,iwork,iflag)
c
c Print intermediate results.
c
        write (6,1020) maxnfe,tol,fx,nfe,iflag
 1020   format (t5,i6,t15,e10.2,t30,e13.5,t45,
     *          i6,t60,i5)
c
c Check iflag to see if done or which termination
c test needs to be reset before resuming optimization.
c
        if (iflag .eq. -1) then
          if (maxnfe .ge. nf2) go to 30
          maxnfe = maxnfe+nfinc
        else if (iflag .eq. 0) then
          if (tol .le. tol2) go to 30
          tol = tol*tolfac
        else
          go to 30
        end if
c
c Resume optimization in continuation mode.
c
        mdcont = 1
        go to 20
c
c Print optimization results.
c
   30   continue
        print *, '****************************************'
        print *, '******* optimization results ***********'
        print *, '****************************************'
        write (6,1025) iflag,flgdsc(iflag)
 1025   format (' iflag =',i3,5x,'(',a26,')')
        write (6,1030) nfe,fx
 1030   format (/1x,'nfe=',i7,5x,'fx=',e16.8/)
      print *, 'x ='
      write (6,1005) (x(i),i=1,n)
c
      stop
      end
      double precision function fun(n,x)
c
      integer n
      double precision x(n)
c
c local variables
c
      integer i
c
c fstar = 0 at xstar = (0,...,0)
c
      fun = 0.
      do 10 i = 1,n
        fun = fun+x(i)**2
   10 continue
      return
      end
