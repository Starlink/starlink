      subroutine fitcont( STATUS )
*+
* Name:
*    FITCONT

* Invocation:
*    CALL FITCONT( STATUS )
*
* Purpose:
*   To fit a Chebyshev polynomial to the continuum for 2D data.

* Description:
*   As with VIG, lines can be excluded from the polynomial
*   fitting. FITCONT stores the polynomial fitting coefficients in
*   the actual data file, for use by LONGSLIT (the program is
*   specifically for use with LONGSLIT, and of no use otherwise).

* Parameters:
*   IMAGE = FILE (Read)
*        Input file
*   XSECT = INTEGER (Read)
*        Cross-section to take first cut from

* Subroutines called:
*   CLGRAP               : Close graphics
*   CNF_PVAL             : Full pointer to dynamically allocated memory
*   CONTRL_CPOLY2        : Fit polynomials to points, finding order if
*                          required
*   COPR2D               : Copy real to double precision
*   FITCONT_ST           : Create .fitcont structure by a number of
*                          elements
*   GR_SOFT              : Open softcopy device
*   PGPAGE               : Clear graphics screen
*   PLOT_DATA            : Plot data
*   REJECT_DATA          : Reject data from polynomial fitting
*   WEIGHT_FIT           : Set weight array
*
*   CNV_FMTCNV           : Format conversion routine
*   DSA_CLOSE            : Close DSA
*   DSA_DATA_SIZE        : Get data size
*   DSA_FREE_WORKSPACE   : Free workspace
*   DSA_GET_WORK_ARRAY   : Get workspace
*   DSA_INPUT            : Open input file
*   DSA_MAP_AXIS_DATA    : Map axis data
*   DSA_MAP_DATA         : Map data
*   DSA_OPEN             : Open DSA
*   DYN_INCAD            : Address offset
*   PAR_RDVAL            : Read value from user
*   PAR_WRUSER           : Write string to user

* History:
*   T.N.Wilkins
*   TNW 25/10/88 Coverted to use of DSA routines
*   AJH 5/1/99 Changed dsa_map 'r' to 'READ' mode
*-
      implicit none

      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function

      integer status
      integer max_ord
      integer nl,ni
      integer kp1l
      integer plot_step
      integer xsect,start
      integer max_work
      parameter (max_ord=20)

* title for plots

      character*30 title

* Label for Y axis of plot

      character*30 labely

* LAbel for X axis of plot

      character*30 labelx
      integer dims(2),ndim,nelm

* if plots to be made

      logical plot

* if best fit order to be sought

      logical seek

* polynomial order to use

      integer order
      logical batch,par_batch,ifexit,par_quest
      character*14 chars
      real value

* type of plot for plot_daTA

      integer plot_type

* SYMBOLIC CONSTANT FOR PLOT_DATA

      integer data_plot
      parameter (data_plot = 0)
      logical newstruct
      integer halfni,nbad,cnv_fmtcnv

* FIGARO VM HANDLING

      integer sptr,wptr,xptr,slot,slota,slotb,slotc
      integer jptr,iptr,i,ptr1,ptr2,ptr3
      logical isnew

* trams pointers

      integer t1ptr,t2ptr

* trams work space

      integer slot1,slot1a

*  ---------------------------------------------------------------------
      batch = par_batch()
      labelx='channels'
      title='channel direction cut'
      labely=' '
      status = SAI__OK
      max_work = 50
      call dsa_open(status)
*
*   Get name of input file
*
      call dsa_input('image','image',status)
*
*     Get dimensions of input data
*
      call dsa_data_size('IMAGE',2,ndim,dims,nelm,status)
      nl=dims(1)
      if(ndim.lt.2) then
        ni=1
      else
        ni=dims(2)
      end if
*
*  Set up coefficient structure
*
      call csub_st('image',ni,nl,newstruct,status)

* Map the coefficient array

      call accres(' ','coeff','du',max_ord*ni,jptr,' ',status)
*
*  Map the data
*
      call dsa_map_data('image','UPDATE','float',iptr,slot,status)

*  Axis data required double precision for NAG use.

      call dsa_map_axis_data('image',1,'READ','double',xptr,slot,status)
      call accres(' ','weights','du',nl,wptr,' ',status)
*
*   Find data.
*
*   First get virtual memory:
*    SPTR  NL  (d)
*    PTR1  MAX_ORD         [d]
*    PTR2  MAX_ORD*MAX_ORD [d]
*    PTR3  NL*3+MAX_ORD*3 (d)

      call dsa_get_work_array(nl,'double',sptr,slot,status)
      call dsa_get_work_array(MAX_ORD,'double',ptr1,slota,status)
      call dsa_get_work_array(MAX_ORD*MAX_ORD,'double',ptr2,slotb,
     :                        status)
      call dsa_get_work_array(3*(nl+MAX_ORD),'double',ptr3,slotc,status)
      if(status.ne.SAI__OK) go to 500
      halfni = max(1,(ni/2))

      if(newstruct)  then

*   Zero out coeficient array

        call zero_dble(%VAL(CNF_PVAL(jptr)),ni*max_ord)
      end if

* if we are not in BATCH mode then we allow the user
* control of the fit order etc.

      if(.not.batch) then

* get the xsect to be fitted

        call par_rdval('xsect',1.0,real(ni),real(halfni),' ',value)
        xsect = nint(value)
        call dyn_incad(iptr,'FLOAT',(xsect-1)*nl,start,isnew,status)
        status = cnv_fmtcnv('float','double',%VAL(CNF_PVAL(start)),
     :                      %VAL(CNF_PVAL(sptr)),nl,nbad)
        call weight_fit(0.0,nl,%VAL(CNF_PVAL(wptr)),.false.)
        if ( isnew ) call cnf_unregp(start)
*
* open graphics
*
        call gr_soft(status)

* get VM for the tram lines defining the regions to be ignored

        call dsa_get_work_array(max_work,'float',t1ptr,slot1,status)
        call dsa_get_work_array(max_work,'float',t2ptr,slot1a,status)
        if(status.ne.SAI__OK) goto 500

* Plot the continuum

        plot_type = data_plot
        call plot_data(%VAL(CNF_PVAL(xptr)),%VAL(CNF_PVAL(sptr)),nl,
     :                 labelx,title,%VAL(CNF_PVAL(wptr)),plot_type,
     :                 labely)

* reject any regions which are contaminated

        call reject_data(%VAL(CNF_PVAL(xptr)),nl,%VAL(CNF_PVAL(wptr)),
     :                   labelx,'lines',
     :                   'put tramlines around spectral lines',
     :                   %VAL(CNF_PVAL(t1ptr)),%VAL(CNF_PVAL(t2ptr)),
     :                   max_work)
        call dsa_free_workspace(slot1a,status)
        call dsa_free_workspace(slot1,status)
*
*   Fit continuum
*   seeking the best fit order to use
        seek = .true.
        plot = .true.
        order = 0
        call contrl_cpoly2(%VAL(CNF_PVAL(xptr)),%VAL(CNF_PVAL(sptr)),nl,
     :                     %VAL(CNF_PVAL(wptr)),%VAL(CNF_PVAL(ptr1)),
     :                     max_ord,kp1l,labelx,labely,2,
     :                     %VAL(CNF_PVAL(ptr2)),plot,seek,
     :                     %VAL(CNF_PVAL(ptr3)))

*
        call accres(' ','order','wi',1,kp1l-1,' ',status)

* continue in Batch mode if it is a big 2d data set? . Set the default
* to TRUE for anything with more than 20 Xsects.

        ifexit =
     :     par_quest('Leave now to continue in batch mode',ni.gt.20)

* clear the graphics screen to finish with

        if(ifexit) then
          call pgpage
        end if
      else

* we are in BATCH mode and the user now has no control. All the
* information required to fit the contimuum has altready been
* stored in the .CONTINUUM structure, anything that is missing
* will be assigned its defualt value
* grab a HARDCOPY device if required

        ifexit = .false.
        call gr_hard(status)
      end if
* if we are to contnue fitting then either we are in BATCH mode
* already or we have a choosen to continue
* so get the fit order required.

      if(.not.ifexit) then
        call accres(' ','order','ri',1,order,' ',status)
        kp1l = order + 1

* set the control state of contrl_cpoly2 so that we
* dont seek the order from the user, we dont sigma clip
* but do use plots and weighted fits. But we don't reallly want plots
* for every cross-section, so the value of PLOT will be reset

        seek       = .false.
        plot       = .true.

* set up so that we plot every 10th X-sect

        plot_step = nint(ni/10.0)

* loop over the X-sects fitting, storing and plotting

        do i=1,ni
*
*   Find data.
*
          call dyn_incad(iptr,'FLOAT',(i-1)*nl,start,isnew,status)
          status = cnv_fmtcnv('float','double',%VAL(CNF_PVAL(start)),
     :                        %VAL(CNF_PVAL(sptr)),nl,nbad)
          call weight_fit(0.0,nl,%VAL(CNF_PVAL(wptr)),.false.)
          if ( isnew ) call cnf_unregp(start)
*
*   Fit continuum.
*
          write(chars,'(a,i4)')' x-section',i
          call par_wruser(chars,status)

          call contrl_cpoly2(%VAL(CNF_PVAL(xptr)),%VAL(CNF_PVAL(sptr)),
     :                       nl,%VAL(CNF_PVAL(wptr)),
     :                       %VAL(CNF_PVAL(ptr1)),max_ord,kp1l,labelx,
     :                       labely,2,%VAL(CNF_PVAL(ptr2)),plot,seek,
     :                       %VAL(CNF_PVAL(ptr3)))

* save these poly result in the .CONTINUUM structure

          call save_cheby(ni,i,i,%VAL(CNF_PVAL(ptr1)),kp1l,max_ord,
     :                          %VAL(CNF_PVAL(jptr)))

* if the next X-sect that would be plotted is beyond the end of
* the data then turn the plotting off

          plot = mod(i,plot_step).eq.0
          if(plot) call pgpage
        end do
      end if

* close down the graphics

      call clgrap
 500  continue
      call dsa_close(status)
      end
