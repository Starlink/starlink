      subroutine csub( STATUS )
*+
* Name:
*    CSUB

* Invocation:
*    CALL CSUB( STATUS )

* Purpose:
*   To subtract a continuum from 2 dimensional data.

* Description:
*  A polynomial is
*  fitted to the continuum and this is subtracted.
*  As with VIG, lines can be excluded from the polynomial
*  fitting. CSUB stores the polynomial fitting coefficients in
*  the actual data file.

* Parameters:
*    IMAGE = FILE (Read)
*        Name of image for input
*    OLD = LOGICAL (Read)
*        Old coefficients are to be used for correction
*    OUTPUT = FILE (Write)
*        Name of output file
*            OUTPUT is the name of the resulting spectrum. If OUTPUT is the
*            same as INPUT the data in the input file will be modified in
*            situ. Otherwise a new file will be created.
* History
*       AJH - Replaced 'R' in dsa_map.. with 'READ'
*-
      implicit none
      integer status
      integer max_ord,jptr,iptr,i,xinptr,nels
      integer nl,ni
      integer mord
      integer kp1l
      integer plot_step
      parameter (max_ord=20)
      character*30 labell
      character*30 directionl

* Coefficients

      double precision coeffl(max_ord)
      integer dims(2),ndim,nelm
      logical plot,new_csub,ifexit,par_quest
      integer ptr1,ptr2,slot,dyn_element,ptr01,ptr3
      integer lcorptr
      integer sptr,wptr,lptr
      character*14 chars
      include 'PRM_PAR'
      include 'SAE_PAR'
      include 'dynamic_memory_inc'
*  ---------------------------------------------------------------------
      status = SAI__OK
      mord=max_ord
      directionl='channels'
      labell='channel direction cut'
*
*   Get name of input file
*
      call dsa_open(status)
      call dsa_input('image','image',status)
*
*     Get dimensions of input data
*
      call dsa_data_size('image',2,ndim,dims,nelm,status)
      nl=dims(1)
      if(ndim.eq.2) then
        ni=dims(2)
      else
        ni=1
      end if
*
*  Get name of output file and open it
*
      call dsa_output('output','output','image',0,0,status)
      call gr_init

*  See if .CSUB structure already exists, and create it if it doesn't

      call csub_st('output',ni,nl,new_csub,status)

* Map the coefficient array

      call accres(' ','coeff','du',ni*max_ord,jptr,' ',status)
*
*  Map the data
*
      call dsa_map_data('output','UPDATE','float',iptr,slot,status)
      call dsa_map_axis_data('output',1,'READ','double',lptr,
     :  slot,status)
      iptr = dyn_element(iptr)
      lptr = dyn_element(lptr)

* Map weights array

      call accres(' ','weights','du',nl,wptr,' ',status)
*
*   Find data.
*
*   First get virtual memory:
*       LCORPTR  NL (d)
*       SPTR     NL  (d)  =     XINPTR   NL (d)
*       PTR01 20*20 (d)
*       PTR1    20 (r) = 20 (d)
*       PTR2    20 (r)
*       PTR3    NL*3+60 (d)
*   Since not all of these are needed at one time, some pointers
* will have the same value

      nels = (nl*5 + 460)* val__nbd + nl*2*val__nbr
      call getvm(nels,lcorptr,slot,status)
      if(status.ne.SAI__OK) go to 500
      sptr = lcorptr + nl*val__nbd
      xinptr = sptr
      ptr01 = sptr + nl*val__nbd
      ptr1 = ptr01 + val__nbd*400
      ptr2 = ptr1 + val__nbr*20
      ptr3 = ptr2 + val__nbr*20
      if(new_csub) then

*   Zero out coeficient array

        call zero_dble(dynamic_mem(jptr),ni*max_ord)
        call getrow(dynamic_mem(iptr),nl,ni,dynamic_mem(sptr),
     :      max(1,(ni/2)))
        call weight_fit(0.0,nl,dynamic_mem(wptr),.false.)
*
* open graphics
*
        call gr_soft(status)
        call plot_data(dynamic_mem(lptr),dynamic_mem(sptr),nl,
     :    directionl,labell,dynamic_mem(wptr),0,' ')
        call reject_data(dynamic_mem(lptr),nl,dynamic_mem(wptr),
     :         directionl,'lines','put tramlines around spectral lines'
     :         ,dynamic_mem(ptr1),dynamic_mem(ptr2),20)
*
*   Fit continuum
*
        call contrl_cpoly2(dynamic_mem(lptr),dynamic_mem(sptr),nl,
     :      dynamic_mem(wptr),coeffl,mord,kp1l,directionl,labell,2
     :      ,dynamic_mem(ptr01),.true.,.true.,dynamic_mem(ptr3))
        ifexit = par_quest('Leave now to continue in batch',ni.gt.50)
        call accres(' ','order','wi',1,kp1l-1,' ',status)
      else
        call gr_hard(status)
        ifexit = .false.
      end if
      if(.not.ifexit) then

*   Read in value of order

        call accres(' ','order','ri',1,kp1l,' ',status)
        kp1l = kp1l + 1
*
*   Find data.
*
        plot_step=nint(real(ni)/10.0)
        plot=.true.
        xinptr = lcorptr+ nl*8
        do i=1,ni
          call getrow(dynamic_mem(iptr),nl,ni,dynamic_mem(sptr),i)
*
*   Fit vignetting
*
          if(mod(i,10).eq.0) then
            write(chars,'(a,i4)')'X-section',i
            call par_wruser(chars,status)
          end if
          call contrl_cpoly2(dynamic_mem(lptr),dynamic_mem(sptr),nl,
     :         dynamic_mem(wptr),dynamic_mem(ptr1),mord,kp1l,directionl,
     :         labell,2,dynamic_mem(ptr01),plot,.false.,
     :         dynamic_mem(ptr3))
          call save_cheby(ni,i,i,dynamic_mem(ptr1),kp1l,max_ord,
     :                          dynamic_mem(jptr))
          call correct2(dynamic_mem(iptr),dynamic_mem(jptr),nl,ni,mord,
     :          kp1l,i,dynamic_mem(lcorptr),dynamic_mem(xinptr),.false.)
          plot = mod(i,plot_step).eq.0
          if(plot) call pgpage
        end do
      end if
      call clgrap
 500  continue
      call dsa_close(status)
      end
