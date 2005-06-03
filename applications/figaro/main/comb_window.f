      subroutine comb_window(sdata,sdens,nbls,nwindow,left,right,xstart
     :      ,xend,left_r,right_r)
*+
* Name:
*    COMB_WINDOW

* Invocation:
*    CALL COMB_WINDOW(SDATA,SDENS,NBLS,NWINDOW,LEFT,RIGHT,XSTART
*           ,XEND,LEFT_R,RIGHT_R)

* Purpose:
*    Locate continua in data frame

* Description:
*    The data contained between the current X-sect" LIMITS" is
*    divided into a series of WINDOWS of width NWINDOW.The arc
*    fitting then proceeds by rolling up the spwctrum a window
*    at a time until XEND is reached .
*
* Arguments:
*    SDATA(SPDIM1) = REAL ARRAY (Given)
*        To hold extracted spectrum
*    SDENS(SPDIM1) = REAL ARRAY (Given)
*        To hold extracted spectrum
*    NBLS = INTEGER (Given)
*        Number of such windows
*    NWINDOW = INTEGER (Given)
*        Current WINDOW width
*    LEFT(LINE_COUNT) = REAL ARRAY (Given)
*        Left trams
*    RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        Right trams
*    XSTART = INTEGER (Given)
*        Start channel for current LIMITS
*    XEND = INTEGER (Given)
*        end channel for current LIMITS
*    LEFT_R(LINE_COUNT) = REAL ARRAY (Given)
*        updated trams
*    RIGHT_R(LINE_COUNT) = REAL ARRAY (Given)
*        updated trams
*
* Authors:
*   DJA: D.Axon, Anglo-Australian Observatory
*   TNW: T.N.Wilkins, Manchester
* History:
*   DJA: nov, 1984 Anglo-Australian Observatory, original version
*        (arc_window)
*   TNW: 1985, 1986, changes
*   TNW: 25/1/91 XSTART, XEND integer
*   TNW: 11/2/91 Workspace changes
*   TNW: 7/91 Changes for new results structure, it_plus1 removed
*   TNW: 12/6/92 Order of parameters from profile_fit fixed

* Algorithm:
*   A Spatial mask is used to select the the areas of line-fitting and
*   the fit type,and a datablock is used to store the resulting
*   parameters of gaussian fits
*
*   MASK has the following structure:-
*
*    Each row of the matrix refers to a seperate spatial and spectral
*    location. the mask contains either 0's to signify no fit has
*    yet been achieved or a positive integer specifiying the itteration
*    number at which the fit was accumplished. the details of the fits
*    are contained in RESULTS .
*
* Global variables/constants:
*    ARC_DIMS
*    STATUS_INC
*    GR_INC
*-
      implicit none
      include 'PRM_PAR'
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      include 'status_inc'
      include 'gr_inc'

* character

      integer status
      real left(line_count)
      integer nwindow
      real right(line_count)
      integer nbls
      real sdens(spdim1)
      real sdata(spdim1)
      integer xstart
      integer xend
*
*  Local
*

* No. of previous fits

      integer nold

* No. of new fits

      integer nnew

* Line number being worked on

      integer line
      integer istartx

* Channel boundaries for current LINE

      integer iendx
      integer len1,pstat
*
*  For the condition handler in GAUSID - needs COMMON
*     as conditions are unpredictable and can occur anywhere !!
*
      real sg_parms(7),sg_error(7)

      integer nfailsum
      integer iblock,nfailed,ltram,ixws,i
      character*72 chars
      real xws,xwe
      logical kquick
      include 'opt_cmn'

*   functions

      integer rx2chn

* Variables for line following code


* do loop variables

      integer midblock,sblock,eblock,nrun,do_dirn
      real left_r(line_count),right_r(line_count)
      include 'DYNAMIC_MEMORY'
*
*
*
      call par_rdkey('quick',.false.,kquick)
      exception = .false.
*
* analyse the spectra in groups of Nwindow X-sects
*
* ----------------------------------------------------------------------
*                          START OF MAIN LOOP
* ----------------------------------------------------------------------
*
      noldsum   = 0
      nnewsum   = 0
      nfailsum  = 0
*
*  loop over windows
*
      midblock=nbls/2
      sblock=midblock
      eblock=1
      do_dirn=-1
      do i = 1, MAX_DECODE_CONTROL
        deccntr(i) = default_model(i)
      enddo
      if(kquick) then
        deccntr(FIT_MODEL) = 4
        deccntr(FIT_OPT) = 0
      endif

* so lines are followed

      do nrun=1,2
        do iblock=sblock,eblock,do_dirn
*
*  Initialise Counters
*

          nnew     = 0
          nold     = 0
          nfailed  = 0

          icrash   =  iblock
          istartx  =  xstart  + nwindow*(iblock-1)
          iendx    =  istartx + nwindow-1

* test to see if reached end

          if(iendx.gt.xend) then
            iendx = xend
          end if
*
* Define title and legend(1)
*
          call chr_fill(' ',title)
          len1 = 0
          call chr_putc('Window number ',title,len1)
          call chr_puti(iblock,title,len1)
          call chr_fill(' ',legend(1))
          call opt_wruser(title(:len1),pstat)
          len1 = 0
          call chr_putc('Channel',legend(1),len1)
          call encode_range(' ','s',istartx,iendx,legend(1),len1)
          call opt_wruser(legend(1)(:len1),pstat)
*
* get integral over the window
*
          call fig_ytract(dynamic_mem(d_sptr),wavdim,spdim1,istartx,
     :             iendx,sdens)

* loop over the lines

          do line=1, line_count

* set up parameters defining current X-Y location

            jcrash =  line
            legend(2) =    ' '
            if (iblock.eq.sblock) then
              xws = left(line)
              xwe = right(line)
              xws = max(xws,sdata(1))
              xws = min(xws,sdata(spdim1-1))
              xwe = max(xwe,sdata(2))
              xwe = min(xwe,sdata(spdim1))
              left_r(line)=xws
              right_r(line)=xwe
            else
              xws=left_r(line)
              xwe=right_r(line)
            end if
            ixws  = rx2chn(sdata,spdim1,xws)
            ltram  =  rx2chn(sdata,spdim1,xwe) - ixws + 1
*
* display window
*
            if (terminal) then
              call disp_window(left_r,right_r,line,sdata,sdens,spdim1)
            end if
*
*   set title
*
            len1 = 0
            call chr_fill(' ',chars)
            call chr_putc('Line number ',chars,len1)
            call chr_puti(line,chars,len1)
            call opt_wruser(chars(:len1),pstat)

*.............  Fit the line.................

            nnew    = nnew + 1
            crash   = .false.
            if(kquick) then
              call comb_centrd(sg_parms,sg_error,spdim1,sdata,sdens
     :               ,ixws,ltram)
            else
              call profile_fit(sg_parms,sg_error,spdim1,sdata,sdens
     :               ,ixws,ltram,status)
              call opt_release(status)
              if(status.ne.SAI__OK) return
            end if
            call store_results(sg_parms,sg_error,nnew,nfailed,line,
     :          istartx,iendx,deccntr,0.0,%VAL( CNF_PVAL(d_rptr) ),
     :          %VAL( CNF_PVAL(d_vptr) ),%VAL( CNF_PVAL(staptr) ),
     :          %VAL( CNF_PVAL(d_mptr) ),1,1,VAL__BADR)
            if(.not.crash) then
              left_r(line)=sg_parms(4) + 0.5*(left(line) - right(line))
              right_r(line)=sg_parms(4)+ 0.5*(right(line) - left(line))

* check in range

              left_r(line)=max(left_r(line),sdata(1))
              left_r(line)=min(left_r(line),sdata(spdim1-1))
              right_r(line)=max(right_r(line),sdata(2))
              right_r(line)=min(right_r(line),sdata(spdim1))

*       ...check on CRASHES

            end if

*     ...of inner loop

          enddo

*  Increment accumulators and print out some diagnostics over masked range

          nfailsum = nfailsum + nfailed
          noldsum  = noldsum  + nold
          nnewsum  = nnewsum  + nnew

* check if fits have been added ,converted or deleted

          if(nnew.ne.0.or.nold.ne.0) then
            write(chars,'(1x,2(a,i4))')
     :                    ' BLOCK :',iblock,'    - New Fits :',nnew
            call opt_wruser(chars,pstat)
            write(chars,'(2(5x,a,i4))')'- Failed   Fits :',nfailed,
     :                    '- Already Fitted :',nold
            call opt_wruser(chars,pstat)
          endif

*   ...of main loop

        end do
        eblock=nbls
        sblock=midblock+1
        do_dirn=1

* nrun

      end do
* ----------------------------------------------------------------------
*                          END OF MAIN LOOP
* ----------------------------------------------------------------------
*
*  Send useful diagnostics to user
*
      if( exception ) then
        call par_wruser(
     :     ' **  Exceptional conditions have occurred **',pstat)
        call par_wruser(' ** at all points listed in ARC.OVF **',pstat)
      endif
*
      call underscore
      write(chars,'(2x,a,i6)')
     :'Total Number of New Points Fitted   =',nnewsum
      call par_wruser(chars,pstat)
      write(chars,'(2x,a,i6)')
     :'Total Number of failed Fits         =',nfailsum
      call par_wruser(chars,pstat)
      write(chars,'(2x,a,i6)')
     : 'Total Number Previously Processed  =',noldsum
      call par_wruser(chars,pstat)
      call underscore
      iteration = iteration + 1
      end
