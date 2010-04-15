      subroutine flavours(line_name,first,status)
*+
* Name:
*    FLAVOURS

* Invocation:
*    CALL FLAVOURS(LINE_NAME,FIRST,STATUS)
*
* Purpose:
*  To produce output from the program LONGSLIT.

* Description:
*  The desired output tables/plots etc. are first selected, and when the
*  use is happy with the selection they are created. This can be run
*  in batch mode, in which case the desired options are specified as
*  keyword parameters.

* Arguments:
*    LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Names of lines
*    FIRST = LOGICAL (Given and returned)
*        If first time routine called
*    STATUS = INTEGER (Given and returned)
*
* Common include files:
*    ARC_DIMS
*
* Authors:
*  TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then
*       Durham
* History:
*  TNW: 9/11/87 to always prompt on second or later calls
*  TNW: 12/11/87 introduction of MGAUSS, so as to allow the "cube" size
*       to change as required.
*  TNW: 8/12/87  bug fix (size of VM obtained wrong)
*  TNW: 4/88   addition of call to GRVEL
*  TNW: 7-8/88   To prompt for VCORR after first call
*  TNW: 11/11/88 No longer closes soft-copy graphics half way thru'
*           this routine. Improved error handling.
*  TNW: 28/11/88 Use of GETWORK
*  TNW: 12/88 Contour option added
*  TNW: 13/1/89  STATIC no longer a parameter, checks instead for
*           whether OUTABLE is given in command line.
*  TNW: 24/1/89  Change to call of plot_linrat
*  TNW: 7/89  Allow for use of get_parnum in plotvel etc.
*           Changes to plot_av_rot.
*  TNW: 3/1/91   More prompting in this routine rather than PLOTVEL,
*           PLOT_CUBE and PLOT_WHOLE_CUBE.
*  TNW: 25/3/91  Changes to VM for PLOT_AV_ROT
*  TNW: 24/4/91  Flavour numbers made symbolic (fl_hard etc.). Changes
* for
*                new results structure
*  TNW: 12/8/93  Reduced use of par_quest
*  TNW: 2/2/94 Bug fix in workspace for plotall
*  TNW: 4/4/94 Re-organised prompting a bit
*  JWP: March 97 Removed SHAPE and STATIC options.

* Note:
*   FLAVOURS
*   --------
*  cur_flav(1) = HARDCOPY  this allows hardcopies of profile fits
*                          to be automatically produced on the
*                          versetec
*  cur_flav(2) = TABLE     print a table of profile parameters
*                          for each line.
*  cur_flav(3) = PLOT      plot the resulting rotation curves on
*                          the versetec or the behavior of individaul
*                          fit parametrs with location.
*  cur_flav(4) = PRINT     print the rotation curves for each line
*  cur_flav(5) = RATIO     print and plot out line ratios
*  cur_flav(6) = GREY      Plot greyscale velocity map
*  cur_flav(7) = CONTOUR   Plot contour velocity map
*  cur_flav(8) = FULL     Output a large table
*  cur_flav(9) = CHECK    Array of profile plots
* Extra bits in same array
*  cur_flav(10) = plot in softcopy
*  cur_flav(11) = show velocities
*-
      implicit none
      integer status
      include 'arc_dims'
      include 'CNF_PAR'          ! For CNF_PVAL function

* Names of lines

      character*10 line_name(line_count)

* Local


* MAX  number of flavours

      integer MAX_FLAVS, MAX_PLOTOPT, DIM_FLAVS
      parameter (MAX_FLAVS = 9, MAX_PLOTOPT = 5, DIM_FLAVS = MAX_FLAVS
     :     + 2)

* keyword control parameters

      logical cur_flav(DIM_FLAVS),cur_plot(MAX_PLOTOPT)
      save cur_flav, cur_plot
      integer vtype
      integer nels

* maximum number of components that could fit in "cube"

      real vcorr
      save vcorr,vtype
      integer ptr1,ptr2,ptr3,ptr4,ptr5,ptr6,ptr7,ptr8,ptr9
      integer ptr10,ptr11,ptr12,ptr13,ptr14
      integer i,pstat,mark
      integer slot,slot2,slot3,slot4,slot5,slot6,slot7,slot8,slot9
      integer slot10,slot11,slot12,slot13,slot14
      real disper
      integer FL_HARD, FL_TABLE, FL_PLOT
      integer FL_PRINT, FL_RATIO, FL_GREY, FL_CONT, FL_FULL
      integer FL_CHK, PL_VEL, PL_WID, PL_FLX, PL_AV, PL_ALV, FL_SOFT,
     :     FL_VEL
      parameter (FL_HARD = 1, FL_TABLE = 2,
     :     FL_PLOT  = 3, FL_PRINT = 4,
     :     FL_RATIO = 5, FL_GREY  = 6, FL_CONT = 7,
     :     FL_FULL = 8, FL_CHK = 9, PL_VEL = 1, PL_WID = 2,
     :     PL_FLX = 3, PL_AV = 4, PL_ALV = 5, FL_SOFT = 10,
     :     FL_VEL = 11)

* If first time routine called

      logical first
      logical par_given
      logical nagerr
      logical whcube
      logical par_quest
      character*43 chars
      character*48 dict(3)
      character*8 flavs(MAX_FLAVS)
      include 'SAE_PAR'
      include 'PRM_PAR'
      integer dumi
      real dumr
      character dumc
*
      data dict/
     :     'NO         : Don''t mark points according to flux',
     :     'CONTINUOUS : Mark using a continuous scale',
     :     'THREE      : Mark by grading into 3 divisions'/
      data flavs/'hardcopy','table','plot','print','*',
     :     'grey','contour','full','check'/
      data vcorr,vtype/0.0,0/

      cur_flav(FL_SOFT) = .true.
      cur_flav(FL_VEL) = .true.

* On first call then check for keywords

      if(first) then
         do i = 1, MAX_FLAVS
            cur_flav(i) = .false.
            if(flavs(i).ne.'*') then
               if(par_given(flavs(i)))
     :              call par_rdkey(flavs(i),.false.,cur_flav(i))
            end if
         end do
         do i = 1, MAX_PLOTOPT
            cur_plot(i) = .true.
         end do

*         if(par_given('outable')) cur_flav(fl_static) = .true.

         first = .false.
      end if
      if(.not.batch) then

* offer the user the current flavours - options

         call change_flavs(cur_flav,cur_plot,status)

      endif
      if(status.ne.SAI__OK) return

      if(vcorr.ne.0.0) then
         write(chars,'(''Current velocity correction = '',e12.5)')vcorr
         call par_wruser(chars,pstat)
      end if

* Get value of VCORR if required

      if(cur_flav(FL_GREY).or.cur_flav(FL_PRINT)
     :     .or.cur_flav(FL_CONT).or.cur_flav(FL_FULL).or.
     :     (cur_flav(FL_VEL).and.
     :     (cur_flav(FL_HARD).or.(cur_flav(FL_PLOT).and.
     :     (cur_plot(PL_VEL).or.cur_plot(PL_WID).or.cur_plot(PL_ALV)) )
     :     ))) then

* Check if VCORR specified in command line, if so use it. This allows
* correction to be made in batch.

         if(par_given('vcorr')) then
            call par_rdval('vcorr',-3.0e5,3.0e5,0.0,'km/s',vcorr)
         else if(.not.batch) then
            if(par_quest('Evaluate correction for Earth motion etc.'
     :           ,(vcorr.eq.0.0))) then
               print 2000, 'before rv_correct'
 2000          format(1x, a)
               call rv_correct('data',vcorr,vtype)
               print2000, 'after rv_correct'
            end if
         end if
      end if
      if(cur_flav(FL_HARD)) then
         whcube = par_quest('Plot whole cube',batch)
      end if
      if((cur_flav(FL_HARD).and.whcube).or.cur_flav(FL_TABLE).or.
     :     cur_flav(FL_PLOT).or.cur_flav(FL_PRINT).or.
     :     cur_flav(FL_RATIO).or.cur_flav(FL_FULL)) then
         nagerr = par_quest('Show fits with NAG errors?',.false.)
      end if

      if(cur_flav(FL_PLOT).or.cur_flav(FL_PRINT).or.cur_flav(FL_FULL))
     :     then
         if(par_quest('Is data flux calibrated?',.false.)) then
            disper = 1.0
         else
            call get_dispers(%VAL(CNF_PVAL(d_xptr)),wavdim,disper)
         end if
      end if

*  Produce hard plots of the data

      if(cur_flav(FL_HARD)) then
         if(whcube) then
            call plot_all_fits(%VAL(CNF_PVAL(d_rptr)),nagerr,vcorr,
     :           vtype,cur_flav(FL_VEL),%VAL(CNF_PVAL(staptr)))
         else
            call plot_cube(vcorr,vtype,cur_flav(FL_VEL))
         end if
      end if
      if(status.ne.SAI__OK) return

*  Print cube

      if(cur_flav(FL_TABLE)) then
         call par_wruser('Printing fits',pstat)
         call table(%VAL(CNF_PVAL(d_rptr)),nagerr,.false.,status)
      end if



*  Plot rotation Curves etc.

      if(cur_flav(FL_PLOT)) then

* Get workspace:-
*  Into PTR1 : NXP*MGAUSS (real)
*  Into PTR2 : NXP*MGAUSS (real)
*  Into PTR3 : NXP*MGAUSS (real)
*  Into PTR4 : NXP*MGAUSS (real)
*
* For plot_av_rot:
*  Into PTR5 : LINE_COUNT (int)
*  Into PTR6 : LINE_COUNT (int)
*  Into PTR7 : LINE_COUNT (int)
*  Into PTR8 : LINE_COUNT*MGAUSS (real)
*  Into PTR9 : LINE_COUNT*MGAUSS (real)
*
* For plotall:
*  Into PTR10 : LINE_COUNT (int)
*  Into PTR11 : NXP*MGAUSS*LINE_COUNT (real)
*  Into PTR12 : NXP*MGAUSS*LINE_COUNT (real)
*  Into PTR13 : NXP*MGAUSS*LINE_COUNT (real)
*  Into PTR14 : NXP*MGAUSS*LINE_COUNT (real)

* Extra memory may be needed for plot_av_rot

         call dsa_get_work_array(nxp*mgauss,'float',ptr1,slot,status)
         call dsa_get_work_array(nxp*mgauss,'float',ptr2,slot2,status)
         call dsa_get_work_array(nxp*mgauss,'float',ptr3,slot3,status)
         call dsa_get_work_array(nxp*mgauss,'float',ptr4,slot4,status)
         if(cur_plot(PL_AV).and.(line_count.gt.1)) then
            call dsa_get_work_array(line_count,'int',ptr5,slot5,status)
            call dsa_get_work_array(line_count,'int',ptr6,slot6,status)
            call dsa_get_work_array(line_count,'int',ptr7,slot7,status)
            call dsa_get_work_array(line_count*mgauss,'float',ptr8,
     :                              slot8,status)
            call dsa_get_work_array(line_count*mgauss,'float',ptr9,
     :                              slot9,status)
         end if

         if(cur_plot(PL_ALV)) then
            call dsa_get_work_array(line_count,'int',ptr10,slot10,
     :                              status)
            call dsa_get_work_array(line_count*mgauss*nxp,'float',ptr11,
     :                              slot11,status)
            call dsa_get_work_array(line_count*mgauss*nxp,'float',ptr12,
     :                              slot12,status)
            call dsa_get_work_array(line_count*mgauss*nxp,'float',ptr13,
     :                              slot13,status)
            call dsa_get_work_array(line_count*mgauss*nxp,'float',ptr14,
     :                              slot14,status)
         end if

         if(status.eq.SAI__OK) then

            if(batch) then
               mark = 1
            else
               if(cur_plot(PL_VEL).or.cur_plot(PL_WID)) then

*        How do we want to mark the points?

                  call qmenu('Flux Marking',dict,3,2,dumr,dumc,mark,
     :                 dumi,status)
                  mark = mark - 1

               end if
            end if

            if(cur_plot(PL_VEL)) then
               call par_wruser('Plotting Velocities',pstat)
               call plotvel(%VAL(CNF_PVAL(d_rptr)),
     :                      %VAL(CNF_PVAL(d_vptr)),
     :                      %VAL(CNF_PVAL(staptr)),line_name,
     :                      %VAL(CNF_PVAL(d_wptr)),%VAL(CNF_PVAL(ptr1)),
     :                      %VAL(CNF_PVAL(ptr2)),%VAL(CNF_PVAL(ptr3)),
     :                      %VAL(CNF_PVAL(ptr4)),cur_flav(FL_VEL),0,
     :                      vcorr,cur_flav(FL_SOFT),nagerr,.false.,mark)
            end if
            if((line_count.gt.1).and.cur_plot(PL_AV)) then
               call par_wruser('Plotting average velocities',pstat)

               call plot_av_rot(%VAL(CNF_PVAL(d_rptr)),
     :                          %VAL(CNF_PVAL(d_vptr)),
     :                          %VAL(CNF_PVAL(staptr)),
     :                          %VAL(CNF_PVAL(d_wptr)),
     :                          %VAL(CNF_PVAL(ptr1)),
     :                          %VAL(CNF_PVAL(ptr2)),
     :                          %VAL(CNF_PVAL(ptr3)),
     :                          %VAL(CNF_PVAL(ptr4)),vcorr,nagerr,
     :                          %VAL(CNF_PVAL(ptr5)),
     :                          %VAL(CNF_PVAL(ptr6)),
     :                          %VAL(CNF_PVAL(ptr7)),
     :                          %VAL(CNF_PVAL(ptr8)),
     :                          %VAL(CNF_PVAL(ptr9)),cur_flav(FL_SOFT))
            end if
            if(cur_plot(PL_FLX)) then
               call plot_flux(%VAL(CNF_PVAL(d_rptr)),
     :                        %VAL(CNF_PVAL(d_vptr)),
     :                        %VAL(CNF_PVAL(staptr)),line_name,
     :                        %VAL(CNF_PVAL(d_wptr)),
     :                        %VAL(CNF_PVAL(ptr1)),%VAL(CNF_PVAL(ptr2)),
     :                        %VAL(CNF_PVAL(ptr3)),%VAL(CNF_PVAL(ptr4)),
     :                        disper,nagerr,cur_flav(FL_SOFT))
            end if
            if(cur_plot(PL_WID)) then
               call plotvel(%VAL(CNF_PVAL(d_rptr)),
     :                      %VAL(CNF_PVAL(d_vptr)),
     :                      %VAL(CNF_PVAL(staptr)),line_name,
     :                      %VAL(CNF_PVAL(d_wptr)),%VAL(CNF_PVAL(ptr1)),
     :                      %VAL(CNF_PVAL(ptr2)),%VAL(CNF_PVAL(ptr3)),
     :                      %VAL(CNF_PVAL(ptr4)),cur_flav(FL_VEL),1,
     :                      vcorr,cur_flav(FL_SOFT),nagerr,.false.,mark)
            end if

*     All lines on 1 plot

            if(cur_plot(PL_ALV)) then
               call plotall(%VAL(CNF_PVAL(d_rptr)),
     :                      %VAL(CNF_PVAL(d_vptr)),
     :                      %VAL(CNF_PVAL(staptr)),line_name,
     :                      %VAL(CNF_PVAL(d_wptr)),
     :                      %VAL(CNF_PVAL(ptr11)),
     :                      %VAL(CNF_PVAL(ptr12)),%VAL(CNF_PVAL(ptr13)),
     :                      %VAL(CNF_PVAL(ptr14)),vcorr,
     :                      cur_flav(FL_SOFT),nagerr,
     :                      %VAL(CNF_PVAL(ptr10)))
            end if

            if(cur_plot(PL_ALV)) then
               call dsa_free_workspace(slot14,status)
               call dsa_free_workspace(slot13,status)
               call dsa_free_workspace(slot12,status)
               call dsa_free_workspace(slot11,status)
               call dsa_free_workspace(slot10,status)
            end if

            if(cur_plot(PL_AV).and.(line_count.gt.1)) then
               call dsa_free_workspace(slot9,status)
               call dsa_free_workspace(slot8,status)
               call dsa_free_workspace(slot7,status)
               call dsa_free_workspace(slot6,status)
               call dsa_free_workspace(slot5,status)
            end if

            call dsa_free_workspace(slot4,status)
            call dsa_free_workspace(slot3,status)
            call dsa_free_workspace(slot2,status)
            call dsa_free_workspace(slot,status)
         end if
      end if

* Option to output line ratios (interactive only)

      if(cur_flav(FL_RATIO)) then
         nels = 5*mgauss
         call dsa_get_work_array(nxp*mgauss,'float',ptr1,slot,status)
         call dsa_get_work_array(nxp*mgauss,'float',ptr2,slot2,status)
         call dsa_get_work_array(nxp*mgauss,'float',ptr3,slot3,status)
         call dsa_get_work_array(nxp*mgauss,'float',ptr4,slot4,status)
         call dsa_get_work_array(mgauss,'float',ptr5,slot5,status)
         call dsa_get_work_array(2*mgauss,'float',ptr6,slot6,status)
         call dsa_get_work_array(2*mgauss,'float',ptr7,slot7,status)
         if(status.eq.SAI__OK) then

            call plot_linrat(%VAL(CNF_PVAL(d_rptr)),
     :                       %VAL(CNF_PVAL(ptr1)),
     :                       %VAL(CNF_PVAL(ptr2)),%VAL(CNF_PVAL(ptr3)),
     :                       %VAL(CNF_PVAL(ptr4)),nagerr,line_name,
     :                       mgauss,%VAL(CNF_PVAL(ptr5)),
     :                       %VAL(CNF_PVAL(ptr6)),%VAL(CNF_PVAL(ptr7)),
     :                       %VAL(CNF_PVAL(d_vptr)),
     :                       %VAL(CNF_PVAL(staptr)))

            call dsa_free_workspace(slot7,status)
            call dsa_free_workspace(slot6,status)
            call dsa_free_workspace(slot5,status)
            call dsa_free_workspace(slot4,status)
            call dsa_free_workspace(slot3,status)
            call dsa_free_workspace(slot2,status)
            call dsa_free_workspace(slot,status)
         end if
      end if
      if(cur_flav(FL_GREY)) then
         call grvel(%VAL(CNF_PVAL(d_sptr)),spdim1,wavdim,
     :              %VAL(CNF_PVAL(d_tlptr)),%VAL(CNF_PVAL(d_trptr)),
     :              line_count,vcorr,vtype,datafile,
     :              %VAL(CNF_PVAL(d_wptr)),%VAL(CNF_PVAL(d_xptr)),
     :              line_name,batch,.false.,.false.,status)
      end if
      if(status.ne.SAI__OK) return
      if(cur_flav(FL_CONT)) then
         call grvel(%VAL(CNF_PVAL(d_sptr)),spdim1,wavdim,
     :        %VAL(CNF_PVAL(d_tlptr)),%VAL(CNF_PVAL(d_trptr)),
     :        line_count,vcorr,vtype,datafile,%VAL(CNF_PVAL(d_wptr)),
     :        %VAL(CNF_PVAL(d_xptr)),line_name,batch,.true.,
     :        cur_flav(FL_SOFT),status)
      end if

      if(cur_flav(FL_CHK)) then
         call quick_plot(%VAL(CNF_PVAL(d_rptr)),%VAL(CNF_PVAL(d_vptr)),
     :                   %VAL(CNF_PVAL(staptr)),line_name,nagerr,1,
     :                   cur_flav(FL_SOFT),.true.,status)
      end if

      call clgrap


*   Print rotation curves

      if(cur_flav(FL_PRINT)) then
         call par_wruser('Print of velocities',pstat)
         call prvel(%VAL(CNF_PVAL(d_rptr)),%VAL(CNF_PVAL(d_vptr)),
     :              %VAL(CNF_PVAL(staptr)),line_name,
     :              %VAL(CNF_PVAL(d_wptr)),vcorr,nagerr,disper,
     :              %VAL(CNF_PVAL(xptr)),%VAL(CNF_PVAL(yptr)),
     :              %VAL(CNF_PVAL(xdptr)),.false.)
      end if
      if(cur_flav(FL_FULL)) then
         call wrtab(%VAL(CNF_PVAL(d_rptr)),%VAL(CNF_PVAL(d_vptr)),
     :              line_name,%VAL(CNF_PVAL(d_wptr)),vcorr,nagerr,
     :              disper,%VAL(CNF_PVAL(staptr)),status)
      end if
      end
