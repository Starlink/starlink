      subroutine find_bounds(bounds,deccntr)
*+
* Name:
*    FIND_BOUNDS

* Invocation:
*    CALL FIND_BOUNDS(BOUNDS,DECCNTR)

* Description:
* This is the general bounds input routine. All bounds must be
* supplied for every parameter.
*
* Arguments:
*    BOUNDS(MAX_PARS,MAX_CMP,MAX_TIMES,2) = REAL ARRAY (Given)
*        the bounds store
*    DECCNTR(*) = INTEGER ARRAY (Given)
*        Fitting details
* In OPT_CMN
*    MAX_TIMES = INTEGER (Given)
*
*    TIMES = INTEGER (Given)
*
*    MAX_CMP = INTEGER (Given)
*

* Authors:
*  TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92 then
* Durham

* History:
*  TNW: 24/1/89 remove opt_cmn and allow max_cmp to be passed as an
* argument.
*  TNW: 25/3/91, Bounds allowed to have virtually any value
*  TNW: 9-10/6/92, Use of qmenu
*  TNW: 6-7/7/93 Use deccntr, fix bugs for Lorentzians, etc.
*  TNW: 11/4/94 Bug fix to use of qcheck
*

      implicit none
      include 'SAE_PAR'
      include 'status_inc'
      include 'fit_coding_inc'
      include 'opt_cmn'
      integer MAX_PARS
      parameter (MAX_PARS = 4)
*-
      real bounds(MAX_PARS,max_cmp,max_times,2)

* local


* default value of bound

      real def,def2
      real value,value2,values(6)
      integer comp
      real EFOLD
      parameter (EFOLD=2.35482004)
      character*54 chars
      logical qstat,par_qnum,ok,duml
      include 'PRM_PAR'
      integer NDICT,iopt,status,ul,i,np,len1
      integer PWIDTH, PHEIGHT, PCENTRE
      parameter (PWIDTH = 2, PHEIGHT = 3, PCENTRE = 4)
      real minval,conv
      parameter (NDICT = 7)
      character*24 dict(NDICT),cdum*1
      data dict/
     :     'F W_MIN : Minimum width',
     :     'F W_MAX : Maximum width',
     :     'F H_MIN : Minimum height',
     :     'F H_MAX : Maximum height',
     :     'F C_MIN : Minimum centre',
     :     'F C_MAX : Maximum centre',
     :     'Q NEXT  : Next component'/

* Allow for conversion factors to fwhm

      if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
         conv = EFOLD
      else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
         conv = 2.0
      else
         conv = 1.0
      endif

* request the lower and upper bounds for the base

      def = bounds(1,1,times,2)*real(densc) + real(denszero)
      def2 = bounds(1,1,times,1)*real(densc) + real(denszero)
      qstat = par_qnum('Enter Lower Bound to Base',VAL__MINR,def2,def,
     :     .true.,' ',value)
      bounds(1,1,times,2) = (value-real(denszero)) / real(densc)

      qstat = par_qnum('Enter Upper Bound to Base',value,def2,def2,
     :     .true.,' ',value2)
      bounds(1,1,times,1) = (value2-real(denszero)) / real(densc)

* loop over the gaussians requesting the bounds on each
* parameter of each component

      do comp = 1, deccntr(FIT_NCMP)
         iopt = 0
         len1 = 0
         call chr_putc('Component ',chars,len1)
         call chr_puti(comp,chars,len1)
         call par_wruser(chars(:len1),status)
         status = SAI__OK
         do i = 1, NDICT-1
            len1 = 30
            np = (i+3)/2
            ul = 1+mod(i,2)
            call chr_fill(' ',dict(i)(len1:))
            value = bounds(np,comp,times,ul)
            if(np.eq.PWIDTH) then
               value = value*real(datsc)*conv
            else if(np.eq.PHEIGHT) then
               value = value*real(densc)
            else if(np.eq.PCENTRE) then
               value = value * real(datsc) + real(datazero)
            endif
            values(i) = value
         enddo

         call qcheck('Set Bounds',dict,NDICT,values,cdum,duml,iopt,
     :        status)
         do i = 1, NDICT - 1
            value = values(i)
            np = (i+3)/2
            def = bounds(np,comp,times,2)
            if(np.eq.PWIDTH) then
               def2 = real(datsc)*conv
               def = def * conv
               minval = 0.0
            else if(np.eq.PHEIGHT) then
               def = def * real(densc)
               def2 = real(densc)*2.0
               minval = -def2
            else if(np.eq.PCENTRE) then
               def = def * real(datsc) + real(datazero)
               def2 = real(datazero+datsc)
               minval = real(datazero)
            endif
            ul = 1 + mod(i,2)
            if(ul.eq.2) then
               value2 = minval
            else
               value2 = def
            endif
            ok = (value.ge.value2).and.(value.le.def2)
            if(ok) then
               if(np.eq.PWIDTH) then
                  bounds(PWIDTH,comp,times,ul) = value/(real(datsc
     :                 )*conv)
               else if(np.eq.PHEIGHT) then
                  bounds(PHEIGHT,comp,times,ul) = value/real(densc
     :                 )
               else if(np.eq.PCENTRE) then
                  bounds(PCENTRE,comp,times,ul) =
     :                 (value-real(datazero))/real(datsc)
               endif
            else
               len1 = 0
               call chr_putc(dict(i)(:5),chars,len1)
               call chr_putc(' must be in range ',chars,len1)
               call chr_putr(value2,chars,len1)
               call chr_putc(' to ',chars,len1)
               call chr_putr(def2,chars,len1)
               call par_wruser(chars(:len1),status)
            endif
         end do
      end do
      end
