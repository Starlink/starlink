      subroutine choose_bounds(bounds,n_gauss,iloop)
*+
* Name:
*    CHOOSE_BOUNDS

* Invocation:
*    CALL CHOOSE_BOUNDS(BOUNDS,N_GAUSS,ILOOP)

* Purpose:
*   Print out stored bounds and select those to use for current fit.

* Description:
*   Print out stored bounds and select those to use for current fit.
*
* Arguments:
*   N_GAUSS = INTEGER (Given)
*        Number of components
*   ILOOP = INTEGER (Given)
*        Loop number
*   BOUNDS(MAX_PARS,MAX_CMP,MAX_TIMES,2) = REAL ARRAY (Given and returned)
*        Bounds
* Global variables:
*   MAX_TIMES = INTEGER (Given)
*        Maximum number of attempt stored within program (include file opt_cmn)
*   DENSC = DOUBLE PRECISION (Given)
*        Density scale factor (include file opt_cmn)
*   DATSC = DOUBLE PRECISION (Given)
*        X array scale factor (include file opt_cmn)
*   DATAZERO = DOUBLE PRECISION (Given)
*        X array zero point for scaling (include file opt_cmn)
*   TIMES = INTEGER (Given)
*        Pointer to bounds array (include file opt_cmn)
*   MAX_CMP = INTEGER (Given)
*        Maximum number of components in fit (include file opt_cmn)
* History:
*  Altered T.N.Wilkins Manchester 24/1/89 to pass variables as arguments
*  rather than in common.
*  TNW 10/6/92, Output code made shorter
*  TNW 10/9/92, Minor changes
*-
      implicit none
      include 'opt_cmn'
      integer idef,iloop
      integer MAX_PARS
      parameter (MAX_PARS=4)
      real bounds(MAX_PARS,max_cmp,max_times,2)
      integer n
      integer n_gauss
      integer j
      integer status
      character*72 chars
      character*56 header
      real value
      real cent,fwhm,height,base
      integer icomp,ul
      character*3 lubnd(2)
      logical par_quest,qstat,par_qnum
      real EFOLD
      parameter (EFOLD=2.35482004)
      data lubnd/'U-B','L-B'/
* --------------------------------------------------------------
      header  ='  Store Component   Centre    Fwhm       Height    Base'
      call par_wruser(header,status)

* Check on number of used slots

      if(iloop.gt.max_times) then
        idef=max_times
      else
        idef=times-1
      end if

* loop over the stores

      do j = 1,idef
        call par_wruser(' ',status)
        do icomp = 1, n_gauss
          do ul = 2, 1, -1
            height = bounds(3,icomp,j,ul) * real(densc)
            fwhm = bounds(2,icomp,j,ul) * real(datsc) * EFOLD
            cent = bounds(4,icomp,j,ul) * real(datsc) + real(datazero)
            if(icomp.eq.1) then
              base =bounds(1,1,j,ul)*real(densc) + real(denszero)
              write(chars,1)lubnd(ul),j,icomp,cent,fwhm,height,base
            else
              write(chars,1)lubnd(ul),j,icomp,cent,fwhm,height
            endif
            call par_wruser(chars,status)
          end do
        end do
      end do
      qstat = par_qnum('Choose a table entry',1.0,real(idef),
     :     real(times-1),.true.,' ',value)

* Copy entry over

      do n = 1, 2
        call copr2r(MAX_PARS*n_gauss,bounds(1,1,nint(value),n),
     :       bounds(1,1,times,n))
      end do

      if(par_quest('Change any bounds?',.false.)) then
        call find_bounds(bounds,n_gauss)
      end if
 1    format(a3,i2,6x,i2,4x,4(1pg11.4))
      end
