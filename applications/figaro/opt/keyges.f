      subroutine keyges(guess_store,n_gauss,i_gauss,last)
*+
* Name:
*    KEYGES

* Invocation:
*    CALL KEYGES(GUESS_STORE,N_GAUSS,I_GAUSS,LAST)

* Purpose:
*  Subroutine to get guesses to gaussian parameters from keyboard.

* Description:
*  Subroutine to get guesses to gaussian parameters from keyboard.

* Arguments:
*    N_GAUSS = INTEGER (Given)
*        number of components at last iteration
*    LAST = INTEGER (Given)
*        last iteration number
*    GUESS_STORE(MAX_PARS,MAX_CMP,MAX_TIMES) = REAL ARRAY (Returned)
*        the guess store
*    I_GAUSS = INTEGER (Returned)
*        current number of components
* Given in OPT_CMN:
*    MAX_TIMES = INTEGER (Returned)
*
*    TIMES = INTEGER (Returned)
*        iteration number
*    MAX_CMP = INTEGER (Returned)

* Authors:
*   TNW: T.N.Wilkins, Durham

* History:
*   TNW: 11/2/94 Altered to use qmenu

      implicit none
      include 'opt_cmn'
      include 'PRM_PAR'
      integer MAX_PARS
      parameter (MAX_PARS  = 4)
*-
      integer last
      integer n_gauss
      integer i_gauss
      real guess_store(MAX_PARS,max_cmp,max_times)

* local
      real w
      real h
      real b
      real c
      integer i,ii,status,ivalue,cmp,len1
      real EFOLD,value
      parameter (EFOLD=2.35482004)
      character dumc
      logical loop,new
      integer NDICT,narg,key
      parameter (NDICT = 8)
      character*52 dict(NDICT),string
      data dict/
     :     'BASE %F   : Base (for all components) . = ',
     :     'CENTRE %F : Centre .................... = ',
     :     'HEIGHT %F : Height .................... = ',
     :     'WIDTH %F  : Width ..................... = ',
     :     'NEW       : Add new component',
     :     'CHANGE %F : Change component .......... = ',
     :     'DELETE    : Delete current component',
     :     'OK        : Settings ok'/
* ----------------------------------------------------------------------
* enter the number of gaussians required

      loop = .true.
      new = .true.
      cmp = 0
      i_gauss = 0
      do while(loop)

* fill in the default values of the first guess from the last iteration
* slot. Testing to see if any of them are zero. If they are use the
* values of the highest available component from the previous fit

         if(new) then
            if(i_gauss.lt.max_cmp) then
               i_gauss = i_gauss + 1
               cmp = i_gauss
               if (cmp.gt.n_gauss) then
                  ii = n_gauss
               else
                  ii = cmp
               end if

*  Copy over results from previous attempt

               guess_store(4,cmp,times) = guess_store(4,ii,last)
               guess_store(3,cmp,times) = guess_store(3,ii,last)
               guess_store(2,cmp,times) = guess_store(2,ii,last)

* get the base only if first time through the loop

               if(cmp.eq.1) then
                  guess_store(1,cmp,times) = guess_store(1,ii,last)
               else
                  guess_store(1,cmp,times) = guess_store(1,1,times)
               end if
               new = .false.
            else
               call par_wruser('Maximum number of components reached'
     :              ,status)
            end if
         end if
         status = 0
         b = guess_store(1,cmp,times)*real(densc) + real(denszero)
         c = guess_store(4,cmp,times)*real(datsc) + real(datazero)
         h = guess_store(3,cmp,times)*real(densc)
         w = guess_store(2,cmp,times)*real(datsc)*EFOLD
         len1 = 42
         call chr_putr(b,dict(1),len1)
         call chr_term(len1,dict(1))
         len1 = 42
         call chr_putr(c,dict(2),len1)
         call chr_term(len1,dict(2))
         len1 = 42
         call chr_putr(h,dict(3),len1)
         call chr_term(len1,dict(3))
         len1 = 42
         call chr_putr(w,dict(4),len1)
         call chr_term(len1,dict(4))
         len1 = 42
         call chr_puti(cmp,dict(6),len1)
         call chr_term(len1,dict(6))
         len1 = 0
         call chr_putc('Number of components = ',string,len1)
         call chr_puti(i_gauss,string,len1)
         call par_wruser(string,status)
         call par_wruser('(use NEW to increase)',status)
         call qmenu('Keyboard Guesses',dict,NDICT,NDICT,value,dumc,key
     :        ,narg,status)
         if(key.eq.1) then

*  Base

            do i = 1, i_gauss
               guess_store(1,cmp,times) = (value-real(denszero)) /
     :              real(densc)
            enddo
         else if(key.eq.2) then

*  Centre

            guess_store(4,cmp,times)= (value-real(datazero))/real(datsc
     :           )

         else if(key.eq.3) then

*  Height

            guess_store(3,cmp,times) = value/real(densc)

         else if(key.eq.4) then

*  Width

            guess_store(2,cmp,times) = value/(real(datsc)*EFOLD)

         else if(key.eq.5) then

*  New

            new = .true.

         else if(key.eq.6) then

*  Change component

            ivalue = nint(value)
            if((ivalue.le.i_gauss).and.(ivalue.gt.0)) then
               cmp = ivalue
            else if(ivalue.eq.(i_gauss+1)) then
               new = .true.
               call par_wruser('Creating new component',status)
            else
               call par_wruser('Component doesn''t exist',status)
            endif

         else if(key.eq.7) then

*  Delete current component

            i_gauss = i_gauss - 1
            do i = cmp, i_gauss
               guess_store(2,i,times) = guess_store(2,i+1,times)
               guess_store(3,i,times) = guess_store(3,i+1,times)
               guess_store(4,i,times) = guess_store(4,i+1,times)
            enddo
            cmp = min(cmp,i_gauss)
         else if(key.eq.8) then

*  Ok

            loop = .false.

         endif
      end do
      end
