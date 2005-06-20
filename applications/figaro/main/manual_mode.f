      subroutine manual_mode(sdata,line_name,left,right,status)
*+
* Name:
*    MANUAL_MODE

* Invocation:
*    CALL MANUAL_MODE(SDATA,LINE_NAME,LEFT,RIGHT,STATUS)
*
* Purpose:
*   Perform manual blocking, fitting, etc.

* Description:
*    The data contained between the current X-sect" LIMITS" is
*    divided into a series of WINDOWS of width NWINDX.Each
*    window may be viewed and a decision as to whether to fit
*    it may be made.
*    Within each window it is also possible to"SEE" individual
*    X-sects,or sub-GROUPS,and fits can also be specified for
*    these as well.
*    Because of the way these operations are carried out it is NOT
*    possible to have multiple fits in a given window with both
*    different fit types and different BLOCKINGS of the data.
*    This is because of the way the checking carried out in
*    the fitting process assumes that higher spatial resolution
*    fits are superior.
*    When a FIT is requested it causes an entry to be made into the
*    the control array and into the results structure in INSTANT
*    mode.
*      An option is also provided to view profile fits 20 to a screen
*    (or the same in hard-copy), successful fits being extracted and
*    displayed. Hardcopy plots of the current profile are available.
*
* Arguments:
*   SDATA(WAVDIM) = REAL ARRAY (Given)
*      Wavelength axis data
*   LEFT(LINE_COUNT) = REAL ARRAY (Given)
*      Left boundaries of lines
*   RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*      Right boundaries of lines
*   LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*      Names of lines
*   STATUS = INTEGER (Given and returned)
*      Error status, 0=ok
* Global variables:
*   WAVDIM = INTEGER (Given and returned)
*      Number of channels in data (include file arc_dims)
*   LINE_COUNT = INTEGER (Given and returned)
*      Number of lines (include file arc_dims)
*   D_RPTR = INTEGER (Given and returned)
*      Array index (pointer) for results array (include file arc_dims)
*   D_CPTR = INTEGER (Given and returned)
*      Array index (pointer) for control array (include file arc_dims)
*   D_WPTR = INTEGER (Given and returned)
*      Array index (pointer) for rest wavelengths array (include file
*      arc_dims)
*   D_VSPTR = INTEGER (Given and returned)
*      Array index (pointer) for sdens work array (include file arc_dims
* )
*   MXPARS,NYP,NXP = INTEGER (Given and returned)
*      Dimensions of results array (include file arc_dims)
*
* Subroutines/functions referenced:
*      DELETE_FIT         : Delete fit
*      DJA_SEE            : Look at part of window
*      ENCODE_CONTRL      : Encode fit type etc.
*      GR_SPEN            : Select graphics pen
*      HARDCOPY_PLOT      : Produce hardcopy fit of profile+fit
*      MANUAL_MENU        : Get option from user
*      ONE_LINE           : Perform fitting
*      QUICK_PLOT         : Output an array of line profile plots
*      RX2CHN             : Convert X array number to pixel number
*      SET_FIT            : Set fit type into control array
*      SET_MASK           : Set mask array
*      UPDTMSK            : Update the mask array to current iteration
*      WINDPLOT           : Display line profile
*      WINDOW_LIMITS      : Get new limits for window
*      WINDOW_SCAN        : Scan through windows
*      WINDOW_WIDTH       : Get width of window from user
*
*      DSA_FREE_WORKSPACE : Free workspace
*      DSA_GET_WORK_ARRAY : Get work array
*      FIG_XVALUE         : Convert real number of pixel into real value
*                           of array element
*      PAR_QNUM           : Obtain number from user
*      PAR_QUEST          : Obtain yes/no response from user
*      PAR_WRUSER         : Write character string to user
*      CHR_FILL           : Fill character string with given character
*      CHR_PUTC           : Insert character string into character
*                           string
*      CHR_PUTI           : Insert integer into character string
*
* Authors:
*   DJA: D.J.Axon Manchester
*   TNW: T.N.Wilkins Manchester until 1/89, then Cambridge until 9/92

* History:
*  DJA: Originally DJA_WINDOW
*  TNW: 15/12/88 Changed to use fig_xvalue
*  TNW: 5/6/89 Not to ask for block number if only one block
*  TNW: 3/90 PGPLOT version
*  TNW: 14/8/90 Minor changes
*  TNW: 18-19/4/91 Include much of MANUAL_MODE, then renamed to that!
*  TNW: 1-8/7/91 Changes for new results structure
*  TNW: 10/7/91 it_plus1 removed
*  TNW: 14-MAR-1994, return values from WIDTH option of menu to here
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      character*10 line_name(line_count)
      real left(line_count)
      real right(line_count)
      integer status
      real sdata(wavdim)
*
* local
      integer nfit
      include 'status_inc'

* start xsect for analysis

      integer xstart,ystart

* end xsect for analysis

      integer xend,yend

* current line

      integer line

* do while control

      logical loop

* PAR routine response

      logical qstat

* PAR routine value

      real value
      integer iblock,jblock

* Current WINDOW width

      integer nwindx,nwindy

* Number of such windows

      integer nxbls,nybls
      integer slot

* "       "    fit status

      integer fit_status(MAX_CONTROL)

* menu option

      integer ib

* current starting value window

      integer istartx,istarty

* do loop

      integer i,j

* current ending value of window

      integer iendx,iendy

* not passed correctly yet

      integer nnew,nold,nfailed,maskedout
      integer pstat,len1

* external references

      logical par_quest
      logical par_qnum
      logical plot_old_fit
      integer ldptr
      integer get_block
      integer rx2chn
      real bimodf,bm,xbar,xbarl,xbarr,v1,v2,fig_xvalue,values(4)
      integer chanl,chanr,imin,ivalue,ivalue1
      character*46 chars
      logical redraw
      logical samblk

* Menu options

      integer OPT_FIT,OPT_ADVANCE,OPT_BACK,OPT_SEE,OPT_CHANGE,OPT_EXIT
      integer OPT_SCAN,OPT_OLD,OPT_DEL,OPT_HARD
      integer OPT_LAST,OPT_NEXT,OPT_TRIM,OPT_CHECK,OPT_UP,OPT_DOWN
      parameter (OPT_LAST = 1, OPT_NEXT = 2, OPT_TRIM = 3,
     :     OPT_CHECK = 4, OPT_FIT = 5, OPT_ADVANCE = 6, OPT_BACK = 7,
     :     OPT_SEE = 8, OPT_CHANGE = 9, OPT_SCAN = 10, OPT_OLD = 11,
     :     OPT_DEL = 12, OPT_HARD = 13, OPT_EXIT = 14,
     :     OPT_UP = 15, OPT_DOWN = 16)

* if fits to be performed instantly

      logical instant
      save instant,line
      data instant/.true./
      data line/1/
* ---------------------------------------------------------------

* Copy default model into current

      do i = 1, MAX_DECODE_CONTROL
         deccntr(i) = default_model(i)
      end do
      nfit = 0

*  Decide on first profile to plot...

      if(line_count.eq.1) then
         line = 1
      else

*  Choose which line to start with-first display a list

         call list_lines(line_count,%VAL( CNF_PVAL(d_wptr) ),line_name)

         qstat = par_qnum('Enter starting line number',1.0,
     :        real(line_count),real(line),.true.,' ',value)
         line = nint(value)
      end if
      xstart = 1
      xend = spdim1

* Set the window size and calculate how many windows fill the data
* limits

      call window_width(xstart,xend,nwindx,nxbls,.true.)

* If there is only one block then we don't need to ask where to start!

      if(nxbls.eq.1) then
         iblock = 1
      else

* enquire for starting window

         qstat = par_qnum('Enter starting cross-section number',
     :        real(xstart),real(xend),real(xstart),.true.,' ',value)

* work out what window this is

         iblock = get_block(xstart,nwindx,nint(value))
      end if

      ystart = 1
      yend = spdim2

* Set the window size and calculate how many windows fill the data
* limits

      call window_width(ystart,yend,nwindy,nybls,.true.)

* If there is only one block then we don't need to ask where to start!

      if(nybls.eq.1) then
         jblock = 1
      else

* enquire for starting window

         qstat = par_qnum('Enter starting spatial Y position',
     :        real(ystart),real(yend),real(ystart),.true.,' ',value)

* work out what window this is

         jblock = get_block(ystart,nwindy,nint(value))
      end if

* Set loop to true, and for the first go through the loop we want to
* plot the line profile

      loop = .true.
      redraw = .true.

* Start of main loop

      do while(loop)

*   Plot profile

         if(redraw) then
            call windplot(line,xstart,xend,ystart,yend,line_name,nwindx,
     :           nwindy,iblock,jblock,.false.,samblk,istartx,iendx,
     :           istarty,iendy,'W I N D O W',status)
         end if

*   Menu options

         call manual_menu(ib,line_name,line_count,spdim2,values,status)

         redraw = .true.
*
*  exit option or abort
*
         if((ib.eq.OPT_EXIT).or.(status.ne.SAI__OK)) then
            loop = .false.
         else if(ib.eq.OPT_LAST) then
            line = line - 1
            if(line.lt.1) then
               call par_wruser('Start of lines : moved to end line'
     :              ,pstat)
               line = line_count
            end if

*   Check fits

         else if(ib.eq.OPT_CHECK) then
            call quick_plot(%VAL( CNF_PVAL(d_rptr) ),
     :           %VAL( CNF_PVAL(d_vptr) ),
     :           %VAL( CNF_PVAL(staptr) ),line_name,
     :           par_quest('Show fits with fitting errors?',.false.),
     :           line,par_quest('Softcopy plot?',.true.),
     :           par_quest('Plot all lines?',.true.),status)
            redraw = par_quest('Replot current data?',.true.)

*     NEXT : increment line counter by 1

         else if(ib.eq.OPT_NEXT) then
            line = line + 1

* check we havent reached the end of the line list
* if we have start again at the first line

            if(line .gt. line_count) then
               call par_wruser('MANUAL > End of lines : starting again',
     :              pstat)
               line = 1
            end if

* l i m i t s

         else if(ib.eq.OPT_TRIM) then

            call window_limits(1,spdim1,xstart,xend)
            call window_width(xstart,xend,nwindx,nxbls,.false.)

*     set starting window (making sure in new range)

            ivalue = max(xstart, min(((istartx+iendx)/2),xend) )

*     work out what window this is

            iblock = get_block(xstart,nwindx,ivalue)

*   If 3-d data then allow user to alter in 3rd dimension

            if(spdim2.gt.1) then
               call window_limits(1,spdim2,ystart,yend)
               call window_width(ystart,yend,nwindy,nybls,.false.)

*     set starting window

               ivalue = max(ystart, min(((istarty+iendy)/2),yend) )

*     work out what window this is

               jblock = get_block(ystart,nwindy,ivalue)
            end if

*   Change to given line

         else if(ib.lt.0) then

            line = -ib

         else if(ib.eq.OPT_HARD) then

*     produce a hardcopy plot

            call hardcopy_plot(%VAL( CNF_PVAL(staptr) ),
     :           %VAL( CNF_PVAL(d_rptr) ),%VAL( CNF_PVAL(d_vptr) ),
     :           line,istartx,iendx,istarty,iendy,line_name,status)

         else if(ib.eq.OPT_DEL) then

*   delete fits in range

            if(par_quest('Are you sure?',.false.)) then
               call delete_fit(%VAL( CNF_PVAL(staptr) ),ncntrl,nyp,nxp,
     :              istartx,iendx,line)
            end if
            redraw = .false.
         else if(ib.eq.OPT_OLD) then

*     Switch plotting old fits on or off

            if(pltold) then
               pltold = .false.
            else
               if(plot_old_fit(%VAL( CNF_PVAL(d_rptr) ),
     :              %VAL( CNF_PVAL(d_vptr) ),
     :              line,istartx,iendx,istarty,iendy,samblk,.true.,
     :              %VAL( CNF_PVAL(staptr) ))) then
                  redraw = par_quest('Replot current data?',
     :                     (.not.samblk))
               else
                  redraw = .false.
               end if
               pltold = (samblk.and.(.not.redraw))
            end if
         else if(ib.eq.OPT_SCAN) then

*     scan through windows

            call window_scan(line,xstart,xend,ystart,yend,line_name,
     :           nwindx,nwindy)

*    Change blocking

         else if(ib.eq.OPT_CHANGE) then
            nwindx = nint(values(1))
            call window_width(xstart,xend,nwindx,nxbls,.false.)

*     enquire for starting window

            ivalue = (istartx+iendx) / 2
            if(spdim2.eq.1) then
               ivalue1 = nint(values(2))
               if((ivalue1.ge.xstart).and.(ivalue1.le.xend)) then
                  ivalue = ivalue1
               end if
            else
               ivalue1 = nint(values(3))
               if((ivalue1.ge.xstart).and.(ivalue1.le.xend)) then
                  ivalue = ivalue1
               end if
            end if

*     work out what window this is

            iblock = get_block(xstart,nwindx,ivalue)

*   If 3-d data then allow user to alter in 3rd dimension

            if(spdim2.gt.1) then
               nwindy = nint(values(2))
               call window_width(ystart,yend,nwindy,nybls,.false.)

*     enquire for starting window

               ivalue1 = nint(values(4))
               if((ivalue1.ge.ystart).and.(ivalue1.le.yend)) then
                  ivalue = ivalue1
               else
                  ivalue = real( (istarty+iendy) / 2 )
               end if

*     work out what window this is

               jblock = get_block(ystart,nwindy,ivalue)
            end if

*    Fit option

         else if(ib.eq.OPT_FIT) then

*     If we want to output bimodality test results, then do so

            if(bimtst) then
               chanl = rx2chn(sdata,wavdim,left(line))
               chanr = rx2chn(sdata,wavdim,right(line))
               call dsa_get_work_array(3*(chanr-chanl+1),'float',ldptr,
     :                                 slot,status)
               if(status.eq.SAI__OK) then
 1                continue
                  bm = bimodf(chanl,chanr,%VAL(CNF_PVAL(ldptr)),xbar,
     :                        xbarl,xbarr,v1,v2,imin,
     :                        %VAL(CNF_PVAL(d_vsptr)),.true.,1.0,-1.0)
                  if(imin.lt.0) then
                     write(chars,'(''Error in BIMODF, number = '',i3)'
     :                    )imin
                     call par_wruser(chars,pstat)
                  else
                     write(chars,'(''Value of BIMODF = '',f12.5)')bm
                     call par_wruser(chars,pstat)
                     write(chars,
     :               '(''Imin (converted to wavelength) = '',f12.5)')
     :                    sdata(imin+chanl-1)
                     call par_wruser(chars,pstat)
                     xbar = xbar+real(chanl)
                     xbarl = xbarl+real(chanl)
                     xbarr = xbarr+real(chanl)
                     xbar = fig_xvalue(xbar,sdata,wavdim)
                     xbarl = fig_xvalue(xbarl,sdata,wavdim)
                     xbarr = fig_xvalue(xbarr,sdata,wavdim)
                     len1 = 0
                     call chr_putc('Xbar = ',chars,len1)
                     call chr_putr(xbar,chars,len1)
                     call par_wruser(chars(:len1),pstat)
                     len1 = 0
                     call chr_putc('Xbarl = ',chars,len1)
                     call chr_putr(xbarl,chars,len1)
                     call chr_putc('Xbarr = ',chars,len1)
                     call chr_putr(xbarr,chars,len1)
                     call par_wruser(chars(:len1),pstat)
                     len1 = 0
                     call chr_putc('V1 = ',chars,len1)
                     call chr_putr(v1,chars,len1)
                     call chr_putc('V2 = ',chars,len1)
                     call chr_putr(v2,chars,len1)
                     call par_wruser(chars(:len1),pstat)
                  end if
                  if(par_quest('Try again?',.false.)) go to 1
                  call dsa_free_workspace(slot,status)
               end if
            end if

*     get the model type

            redraw = .false.
            call set_fit_menu(redraw,instant,deccntr,wavdim,gpscal
     :           ,prvfit,usepeak,bimtst,tyaic,curmcmp,prvpos,mgauss
     :           ,line_count,errpre,inherit,status)
            if(redraw) then
               call windplot(line,xstart,xend,ystart,yend,line_name,
     :              nwindx,nwindy,iblock,jblock,.false.,samblk,istartx,
     :              iendx,istarty,iendy,'W I N D O W',status)
            end if

*     encode into control

            call encode_contrl(deccntr,ncntrl,fit_status)
            do j = istarty,iendy
               do i = istartx,iendx
                  call set_control(%VAL(CNF_PVAL(d_cptr)),line,i,j,
     :                 fit_status)
               end do
            end do
            nfit = nfit + 1

*     Do we want to do the fit now?

            if(instant) then

*       Proceede with model fit

               call one_line(nwindx,left,right,istartx,iendx,nfit,
     :              line_name,%VAL(CNF_PVAL(d_cptr)),line,nnew,nold,
     :              nfailed,maskedout,.false.,.true.,redraw,istarty,
     :              iendy,status)
            else
               redraw = .false.
            end if

*    See section

         else if(ib.eq.OPT_SEE) then
            call dja_see(iblock,real(istartx),real(iendx),nfit,line,
     :           line_name,instant,nnew,nold,nfailed,maskedout,
     :           deccntr,status)

*    Next

         else if(ib.eq.OPT_ADVANCE) then
            iblock = iblock + 1


*    Last

         else if(ib.eq.OPT_BACK) then
            iblock = iblock-1

*    up

         else if(ib.eq.OPT_UP) then
            jblock = jblock + 1


*    down

         else if(ib.eq.OPT_DOWN) then
            jblock = jblock-1
         end if

*     test to see if in bounds

         if(iblock.lt.1) then
            call par_wruser('WINDOW> End of Spectrum',pstat)
            iblock = nxbls
         else if(iblock.gt.nxbls) then
            call par_wruser('WINDOW> Start of Spectrum',pstat)
            iblock = 1
         else if(jblock.lt.1) then
            call par_wruser('WINDOW> End of Spectrum',pstat)
            iblock = nybls
         else if(jblock.gt.nybls) then
            call par_wruser('WINDOW> Start of Spectrum',pstat)
            iblock = 1
         end if
      end do

* End of main loop, output number of fits defined

      len1 = 0
      call chr_putc('Fits defined = ',chars,len1)
      call chr_puti(nfit,chars,len1)
      call par_wruser(chars(:len1),pstat)

* If we've actually performed any fits, then do we want to increase the
* value of iteration to protect them against accidental over-writing?

      if(instant.and.(nfit.gt.0).and.(status.eq.0)) then
         if(par_quest('Increase iteration?',.true.)) then
            iteration = iteration + 1
            write (chars,'(''Iteration now'',i3)') iteration
            call par_wruser(chars,pstat)
            call updtmsk(%VAL(CNF_PVAL(staptr)),
     :                   %VAL(CNF_PVAL(d_mptr)))
         end if
      end if
      end
