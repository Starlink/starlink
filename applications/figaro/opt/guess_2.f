      subroutine guess_2(dens,m,guess,work,absorption,fstat)
*+
* Name:
*    GUESS_2

* Invocation:
*    CALL GUESS_2(DENS,M,GUESS,WORK,ABSORPTION,FSTAT)
*
* Purpose:
*   Guess 2 Gaussians to fit data

* Description:
*   To guess the paramaters of two Gaussians. DENS is assumed
*   to be scaled to the range 0-1. Note that this routine assumes that
*   the X array is linear, scaled from 0-1.
*
* Arguments:
*     DENS(M) = REAL ARRAY (Given)
*        Y (intensity) data array
*     M = INTEGER (Given)
*        Dimension of DATA/DENS
*     ABSORPTION = LOGICAL (Given)
*        If absorptin line
*     FSTAT = INTEGER (Given and returned)
*        Fit error status
*     GUESS(4,2) = REAL ARRAY (Returned)
*        Guesses to parameters
*     WORK(M*4) = REAL ARRAY (Workspace)
* Author:
*     T.N.Wilkins Manchester
* History:
*     TNW/CAVAD Workspace handling changed, 19/9/90
*         "     Minor changes, 7/1/91
*         "     19-20/3/91 Change to work with scaled data
*         "     17/9/91 Changes to allow use for absorption lines, etc.
*         "     18/9/91 Altered to use same format for guesses as for
*               multiples
*-
      implicit none
      integer m
      double precision dens(m)
      real work(m*4)
      real guess(4,2)
      logical absorption
      integer fstat
*
      character*32 chars
      real cen,dat_tot
      logical prompt
      integer ntry
      real factor
      real diff
      integer inl,inr
      integer outl,outr
      real varl,varr
      real bimodf
      real value
      real xbar,xbarl,xbarr,v1,v2,c1
      integer imin
      integer nbad
      integer status
      logical par_quest,loop
      integer i,cnv_fmtcnv
      real disp_on_rt2pi
      real minht
      real irm

      if(fstat.ne.0) return

* Inverse of REAL(M)

      irm = 1.0/real(m)

* Copy to work array

      status = cnv_fmtcnv('double','float',dens,work,m,nbad)

* Base

      call get_median(work,work(m+1),m,c1)
      guess(1,1) = c1
      guess(1,2) = c1

* Subtract base, but don't allow to be negative (or positive if absorption).

      if(absorption) then
        do i = 1, m
          work(i) = -min(work(i)-c1,0.0)
        end do
      else
        do i = 1, m
          work(i)=max(work(i)-c1,0.0)
        end do
      end if
      factor = 1.0

* On first call we don't want to prompt

      prompt = .false.
      ntry = 1
      loop = .true.
      do while(loop)
        value = bimodf(1,m,work(m+1),xbar,xbarl,xbarr,v1,v2,imin,
     :                  work,prompt,factor,c1)

*    If this failed, we will try again up to 4 times, if the error is
*    such that altering factor could help

        if(((imin.ge.-6).and.(imin.le.-4)).and.(ntry.le.4)) then
          factor=factor*0.3
          ntry=ntry+1
        else if(imin.lt.0) then

*    ... otherwise ask user whether to try again

          write(chars,'(''Error in bimodf, number='',i4)')imin
          call opt_wruser(chars,status)
          if(.not.par_quest('Try again?',.false.)) then
            fstat = -1
            return
          end if
          prompt = .true.
          ntry = 1
        else
          loop = .false.
        end if
      end do

* Centre

      guess(4,1) = xbarl*irm
      guess(4,2) = xbarr*irm

* height

      guess(3,1) = work(nint(xbarl))
      guess(3,2) = work(nint(xbarr))
      outl=1
      outr=m
      minht = (1.0-c1)*0.1
      do i=1,nint(xbarl)
        if((guess(3,1).lt.work(i)).and.(work(i).gt.minht)) then
          guess(3,1)=work(i)
          guess(4,1)=real(i)*irm
        end if
        if(work(i).le.1.0e-9) outl=i
      end do
      do i=m,nint(xbarr),-1
        if((guess(3,2).lt.work(i)).and.(work(i).gt.minht)) then
          guess(3,2)=work(i)
          guess(4,2)=real(i)*irm
        end if
        if(work(i).le.1.0e-9) outr=i
      end do

* Widths

      inl = imin
      do i = outl+6,imin
        if(work(i).le.1.0e-9) inl = i
      end do
      inr = imin
      do i = outr-6,imin,-1
        if(work(i).le.1.0e-9) inr = i
      end do
      varl=0.0
      dat_tot=0.0
      cen=guess(4,1)
      do i=outl,inl
        diff = real(i)*irm - cen
        varl = varl + work(i)*diff*diff
        dat_tot = dat_tot + work(i)
      end do
      varl = varl/dat_tot
      guess(2,1) = sqrt(varl)
      disp_on_rt2pi = 0.3989/(m-1)
      guess(3,1) = min(guess(3,1),(disp_on_rt2pi*dat_tot/guess(2,1)))
      varr=0.0
      dat_tot=0.0
      cen=guess(4,2)
      do i=inr,outr
        diff=real(i)*irm-cen
        varr = varr + work(i)*diff*diff
        dat_tot = dat_tot + work(i)
      end do
      varr = varr/dat_tot
      guess(2,2) = sqrt(varr)
      guess(3,2) = min(guess(3,2),(disp_on_rt2pi*dat_tot/guess(2,2)))

      end
