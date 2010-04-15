      subroutine mgmon(n,xc,fc1,gc1,istate,gpjnrm,cond,posdef
     :     ,niter,nf,iw,liw,w,lw)
*+
* Name:
*    MGMON

* Invocation:
*    CALL MGMON(N,XC,FC1,GC1,ISTATE,GPJNRM,COND,POSDEF
*          ,NITER,NF,IW,LIW,W,LW)

* Purpose:
*   monitor file for NAG constrained optimization (E04KDF)

* Description:
*   monitor file for NAG constrained optimization (E04KDF)

* Arguments:
*    N = INTEGER (Given)
*
*    XC(N) = DOUBLE PRECISION ARRAY (Given)
*
*    FC1 = DOUBLE PRECISION (Given)
*
*    GC1(N) = DOUBLE PRECISION ARRAY (Given)
*
*    ISTATE(N) = INTEGER ARRAY (Given)
*
*    GPJNRM = DOUBLE PRECISION (Given)
*
*    COND = DOUBLE PRECISION (Given)
*
*    POSDEF = LOGICAL (Given)
*
*    NITER = INTEGER (Given)
*
*    NF = INTEGER (Given)
*
*    IW(LIW) = INTEGER ARRAY (Given)
*
*    LIW = INTEGER (Given)
*
*    W(LW) = DOUBLE PRECISION ARRAY (Given)
*
*    LW = INTEGER (Given)
*
*
      implicit none
      include 'opt_cmn'
*-
      logical posdef
      integer n,lw,liw,nf,niter,isj,j
      double precision fc1,cond,gpjnrm
      double precision gc1(n)
      double precision w(lw)
      double precision xc(n)
      integer istate(n)
      integer iw(liw)

      norm=gpjnrm
      condit_no=cond
      if(.not.keep_itt) return
*
*  Print out monitor of values at current iteration
*
      write(opt_lu,1) niter,nf
 1    format(/,'ITERATION ',I4,4X,' NUMBER OF FUNCTION EVALS ',I5)
      write(opt_lu,2) fc1 ,gpjnrm
 2    format(/,' SUM OF SQUARES = ',1pg17.8,
     :     /,'NORM OF PROJECTED GRADIENT = ',1pg17.8)
      write(opt_lu,3)
 3    format(//,' J ',11X,' XC(J) ',16X,'GC1(J)',13X,'STATUS',2X,
     :     'ISTATE')
      do 100 j=1,n
        isj=istate(j)
        if(isj.gt.0) then
          write(opt_lu,4) j,xc(j),gc1(j),istate(j)
        else
          isj=-isj
          go to (40,60,80),isj
 40       write(opt_lu,5) j,xc(j),gc1(j)
          go to 100
 60       write(opt_lu,6) j,xc(j),gc1(j)
          go to 100
 80       write(opt_lu,7) j,xc(j),gc1(j)
        end if
 100  continue
 4    format(/,i3,6x,2(1pg17.8,5x),2x,'Free',4x,i3)
 5    format(/,i3,6x,2(1pg17.8,5x),2x,'Upper bound')
 6    format(/,i3,6x,2(1pg17.8,5x),2x,'Lower bound')
 7    format(/,i3,6x,2(1pg17.8,5x),2x,'Constant')
      if(cond.ne.0.0d0) then
        if(cond.gt.1.0d6) then
          write(opt_lu,8)
 8        format(//,'ESTIMATED COND NUMBER OF PROJECTED HESSIAN',
     :         'IS GREATER THAN 1.0D+6')
        else
          write(opt_lu,9) cond
 9        format(//,'ESTIMATED CONDITION NUMBER OF PROJECTED HESSIAN',
     :         ' = ',2x,1pg17.8)
        end if
        if(.not.posdef) write(opt_lu,10)
 10     format(//,'**** PROJECTED HESSIAN NOT POSITIVE DEFINITE***')
      end if
*
      end
