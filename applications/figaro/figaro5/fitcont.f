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
*   the actual data file, for use by LONGSLIT (the program is specfically
*   for use with LONGSLIT, and of no use otherwise).

* Parameters:
*   IMAGE = FILE (Read)
*        Input file
*   XSECT = INTEGER (Read)
*        Cross-section to take first cut from

* Subroutines called:
*   CLGRAP               : Close graphics
*   CONTRL_CPOLY2       : Fit polynomials to points, finding order if
*                          required
*   COPR2D               : Copy real to double precision
*   FITCONT_ST           : Create .fitcont structure
*   GETWORK              : Get virtual memory
*   GR_SOFT              : Open softcopy device
*   PGPAGE               : Clear graphics screen
*   PLOT_DATA            : Plot data
*   REJECT_DATA          : Reject data from polynomial fitting
*   WEIGHT_FIT           : Set weight array
*
*   CNV_FMTCNV           : Format conversion routine
*   DSA_CLOSE            : Close DSA
*   DSA_DATA_SIZE        : Get data size
*   DSA_INPUT            : Open input file
*   DSA_MAP_AXIS_DATA    : Map axis data
*   DSA_MAP_DATA         : Map data
*   DSA_OPEN             : Open DSA
*   PAR_RDVAL            : Read value from user
*   PAR_WRUSER           : Write string to user

* History:
*   T.N.Wilkins
*   TNW 25/10/88 Coverted to use of DSA routines
*   AJH 5/1/99 Changed dsa_map 'r' to 'READ' mode
*-
      implicit none
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

      integer sptr,wptr,xptr,slot,dyn_element
      integer jptr,iptr,i,ptr1,ptr2,nels,ptr3

* trams pointers

      integer t1ptr,t2ptr

* trams work space

      integer slot1

      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'dynamic_memory_inc'
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
      iptr = dyn_element(iptr)

*  Axis data required double precision for NAG use.

      call dsa_map_axis_data('image',1,'READ','double',xptr,slot,status)
      xptr = dyn_element(xptr)
      call accres(' ','weights','du',nl,wptr,' ',status)
*
*   Find data.
*
*   First get virtual memory:
*    SPTR  NL  (d)
*    PTR1  MAX_ORD         [d]
*    PTR2  MAX_ORD*MAX_ORD [d]
*    PTR3  NL*3+MAX_ORD*3 (d)

      nels = nl*4 + max_ord*(max_ord+4)
      call getwork(nels,'double',sptr,slot,status)
      if(status.ne.SAI__OK) go to 500
      ptr1 = sptr + nl*val__nbd
      ptr2 = ptr1 + max_ord*val__nbd
      ptr3 = ptr2 + max_ord*max_ord*val__nbd
      halfni = max(1,(ni/2))

      if(newstruct)  then

*   Zero out coeficient array

        call zero_dble(dynamic_mem(jptr),ni*max_ord)
      end if

* if we are not in BATCH mode then we allow the user
* control of the fit order etc.

      if(.not.batch) then

* get the xsect to be fitted

        call par_rdval('xsect',1.0,real(ni),real(halfni),' ',value)
        xsect = nint(value)
        start = iptr + (xsect-1)*nl*val__nbr
        status = cnv_fmtcnv('float','double',dynamic_mem(start),
     :       dynamic_mem(sptr),nl,nbad)
        call weight_fit(0.0,nl,dynamic_mem(wptr),.false.)
*
* open graphics
*
        call gr_soft(status)

* get VM for the tram lines defining the regions to be ignored

        nels = max_work*2
        call getwork(nels,'float',t1ptr,slot1,status)
        if(status.ne.SAI__OK) goto 500
        t2ptr = t1ptr + max_work*val__nbr

* Plot the continuum

        plot_type = data_plot
        call plot_data(dynamic_mem(xptr),dynamic_mem(sptr),nl,labelx
     :     ,title,dynamic_mem(wptr),plot_type,labely)

* reject any regions which are contaminated

        call reject_data(dynamic_mem(xptr),nl,dynamic_mem(wptr),labelx,
     :            'lines','put tramlines around spectral lines',
     :            dynamic_mem(t1ptr),dynamic_mem(t2ptr),max_work)
*
*   Fit continuum
*   seeking the best fit order to use
        seek = .true.
        plot = .true.
        order = 0
        call contrl_cpoly2(dynamic_mem(xptr),dynamic_mem(sptr),nl,
     :      dynamic_mem(wptr),dynamic_mem(ptr1),max_ord,kp1l,labelx,
     :      labely,2,dynamic_mem(ptr2),plot,seek,dynamic_mem(ptr3))

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
          start = iptr + (i-1)*nl*4
          status = cnv_fmtcnv('float','double',dynamic_mem(start),
     :       dynamic_mem(sptr),nl,nbad)
          call weight_fit(0.0,nl,dynamic_mem(wptr),.false.)
*
*   Fit continuum.
*
          write(chars,'(a,i4)')' x-section',i
          call par_wruser(chars,status)

          call contrl_cpoly2(dynamic_mem(xptr),dynamic_mem(sptr),nl,
     :      dynamic_mem(wptr),dynamic_mem(ptr1),max_ord,kp1l,labelx,
     :      labely,2,dynamic_mem(ptr2),plot,seek,dynamic_mem(ptr3))

* save these poly result in the .CONTINUUM structure

          call save_cheby(ni,i,i,dynamic_mem(ptr1),kp1l,max_ord,
     :                          dynamic_mem(jptr))

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
