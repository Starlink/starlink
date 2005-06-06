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

      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer status
      integer max_ord,jptr,iptr,i,xinptr
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
      integer ptr01,ptr1,ptr2,ptr3
      integer slot,slot1,slot2,slot3,slot4,slot5
      integer lcorptr
      integer sptr,wptr,lptr
      character*14 chars
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
     :                       slot,status)

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

      call dsa_get_work_array(nl,'double',lcorptr,slot,status)
      call dsa_get_work_array(nl,'double',sptr,slot1,status)
      call dsa_get_work_array(400,'double',ptr01,slot2,status)
      call dsa_get_work_array(20,'float',ptr1,slot3,status)
      call dsa_get_work_array(20,'float',ptr2,slot4,status)
      call dsa_get_work_array(nl*3+60,'double',ptr3,slot5,status)
      if(status.ne.SAI__OK) go to 500
      xinptr = sptr
      if(new_csub) then

*   Zero out coeficient array

        call zero_dble(%VAL(CNF_PVAL(jptr)),ni*max_ord)
        call getrow(%VAL(CNF_PVAL(iptr)),nl,ni,%VAL(CNF_PVAL(sptr)),
     :              max(1,(ni/2)))
        call weight_fit(0.0,nl,%VAL(CNF_PVAL(wptr)),.false.)
*
* open graphics
*
        call gr_soft(status)
        call plot_data(%VAL(CNF_PVAL(lptr)),%VAL(CNF_PVAL(sptr)),nl,
     :                 directionl,labell,%VAL(CNF_PVAL(wptr)),0,' ')
        call reject_data(%VAL(CNF_PVAL(lptr)),nl,%VAL(CNF_PVAL(wptr)),
     :                   directionl,'lines',
     :                   'put tramlines around spectral lines',
     :                    %VAL(CNF_PVAL(ptr1)),%VAL(CNF_PVAL(ptr2)),20)
*
*   Fit continuum
*
        call contrl_cpoly2(%VAL(CNF_PVAL(lptr)),%VAL(CNF_PVAL(sptr)),nl,
     :                     %VAL(CNF_PVAL(wptr)),coeffl,mord,kp1l,
     :                     directionl,labell,2,%VAL(CNF_PVAL(ptr01)),
     :                     .true.,.true.,%VAL(CNF_PVAL(ptr3)))
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
          call getrow(%VAL(CNF_PVAL(iptr)),nl,ni,%VAL(CNF_PVAL(sptr)),i)
*
*   Fit vignetting
*
          if(mod(i,10).eq.0) then
            write(chars,'(a,i4)')'X-section',i
            call par_wruser(chars,status)
          end if
          call contrl_cpoly2(%VAL(CNF_PVAL(lptr)),%VAL(CNF_PVAL(sptr)),
     :         nl,%VAL(CNF_PVAL(wptr)),%VAL(CNF_PVAL(ptr1)),mord,kp1l,
     :         directionl,labell,2,%VAL(CNF_PVAL(ptr01)),plot,.false.,
     :         %VAL(CNF_PVAL(ptr3)))
          call save_cheby(ni,i,i,%VAL(CNF_PVAL(ptr1)),kp1l,max_ord,
     :                    %VAL(CNF_PVAL(jptr)))
          call correct2(%VAL(CNF_PVAL(iptr)),%VAL(CNF_PVAL(jptr)),nl,ni,
     :                  mord,kp1l,i,%VAL(CNF_PVAL(lcorptr)),
     :                  %VAL(CNF_PVAL(xinptr)),.false.)
          plot = mod(i,plot_step).eq.0
          if(plot) call pgpage
        end do
      end if
      call clgrap
 500  continue
      call dsa_close(status)
      end
