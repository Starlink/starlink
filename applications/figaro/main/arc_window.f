      subroutine arc_window(sdata,left,right,linnam,left_r,right_r,
     :     status)
*+
* Name:
*    ARC_WINDOW

* Invocation:
*    CALL ARC_WINDOW(SDATA,LEFT,RIGHT,LINNAM,LEFT_R,
*                       RIGHT_R,STATUS)

* Purpose:
*   Locate arc lines at each cross-section

* Description:
*    The data contained between the current X-sect LIMITS is
*    divided into a series of WINDOWS of width NWINDOW.The arc
*    fitting then proceeds by rolling up the spwctrum a window
*    at a time until YEND is reached .
*    This program permits analysis of spectra which may be binned up
*    spatially before fitting. A Spatial mask is used to select the the
*    areas of line-fitting and the fit type,and a 36 elememt datablock
*    is used to store the resulting parameters  of up to 4  gausian
*    fits.
*
* MASK has the following structure:-
*
*    Each row of the matrix refers to a seperate spatial and spectral
*    location. the mask contains either 0's to signify no fit has
*    yet been achieved or a positive integer specifiying the iteration
*    number at which the fit was accumplished. the details of the fits
*    are contained in RESULTS .
*
* Arguments:
*    SDATA(WAVDIM) = REAL ARRAY (Given)
*        To hold extracted spectrum
*    LEFT(LINE_COUNT) = REAL ARRAY (Given)
*        Left trams
*    RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        Right trams
*    LINNAM(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        name of line being worked on
*    LEFT_R(LINE_COUNT) = REAL ARRAY (Given)
*        updated trams
*    RIGHT_R(LINE_COUNT) = REAL ARRAY (Given)
*        updated trams
*    STATUS = INTEGER (Given)
*        Error status, 0=ok
*
* Authors:
*    DJA: D. J. Axon
*    TNW: T. N. Wilkins, Manchester until 31/1/89, then IoA Cambridge until
*         30/9/92, then Durham

* History:
*    DJA: Nov, 1984, Anglo-Australian Observatory
*    TNW: 1985, 1986
*    TNW: to use vm for profile fit 4/8/88, dealt with differently
*      from 21/9/88, tidied 26/9/88 TNW.
*    TNW: 7/12/89 reduction of argument list
*    TNW: 11/2/91 Temporary trams passed as workspace
*    TNW: 18/3/91 Change to tram updating-more careful
*    TNW: 7/91 Mask checking removed, changes for new results structure,
*        it_plus1 removed
*    TNW: 10/6/93 Bug fix on tram updates-use centres
* -------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
*-
*  Input
      integer status
      include 'arc_dims'
      character*10 linnam(line_count)
      real left(line_count)

* Current WINDOW width

      integer nwindow
      real right(line_count)

* Number of such windows

      integer nbls
      include 'gr_inc'
      real sdata(wavdim)

* Start X-sect for current LIMITS

      integer ystart

* end X-sect  for current LIMITS

      integer yend
*
*  Local
*
      character*72 chars
      integer pstat

* No. of previous fits

      integer nold

* No. of new fits

      integer nnew

* Line number being worked on

      integer line

* Number of fits  so far

      integer nfit
      integer istartx

* Channel boundaries for current LINE

      integer iendx
      include 'status_inc'
*
*  For the condition handler in GAUSID - needs COMMON
*     as conditions are unpredictable and can occur anywhere
*
      real sg_parms(7),sg_error(7)

      integer nfailsum
      integer iblock,nfailed,ltram,ixws
      real xws,xwe,diff

*   functions

      integer rx2chn
      logical par_quest

* Variables for line following code

* do loop variables

      integer midblock,sblock,eblock,nrun,do_dirn
      real left_r(line_count),right_r(line_count)
      real diftrm
      integer len1
      include 'opt_cmn'

* Get range/blocking

      call range_in(spdim1,ystart,yend,nwindow,nbls)

      exception = .false.

      if(terminal) then
         if(.not.par_quest('Plot profiles and fits?',.false.)) then
            call clgrap
         end if
      end if

      nfit = 0
*
* Analyse the spectra in groups of Nwindow X-sects
*
*
*   START OF MAIN LOOP
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

* so lines are followed

         do nrun=1,2
            do iblock=sblock,eblock,do_dirn
*
*  Initialise Counters
*
               nnew    = 0
               nold    = 0
               nfailed = 0

               icrash  = iblock
               istartx = ystart  + nwindow*(iblock-1)
               iendx   = istartx + nwindow-1

* test to see if reached end

               iendx=min(iendx,yend)
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
               call chr_putc('xsect',legend(1),len1)
               call encode_range(' ','s',istartx,iendx,legend(1),len1)
               call opt_wruser(legend(1)(:len1),pstat)
*
* get integral over the window
*
               call fig_xtract(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,
     :                         istartx,iendx,%VAL(CNF_PVAL(d_vsptr)))

* loop over the lines

               do line = 1, line_count

* set up parameters defining current X-Y location

                  jcrash = line
                  nfit  = nfit+1
                  legend(2) = linnam(line)
                  if (iblock.eq.sblock) then
                     xws = left(line)
                     xwe = right(line)
                     left_r(line) = xws
                     right_r(line) = xwe
                  else
                     xws = left_r(line)
                     xwe = right_r(line)
                  end if

                  ixws = rx2chn(sdata,wavdim,xws)
                  ltram = rx2chn(sdata,wavdim,xwe) - ixws + 1
*
*      Display window
*
                  if (terminal) then
                     call disp_window(left_r,right_r,line,sdata,
     :                                %VAL(CNF_PVAL(d_vsptr)),wavdim)
                  end if
*
*      Set title
*
                  len1 = 0
                  call chr_fill(' ',chars)
                  call chr_putc('Line number ',chars,len1)
                  call chr_puti(line,chars,len1)
                  len1 = len1 + 1
                  call chr_appnd(linnam(line),chars,len1)
                  call opt_wruser(chars(:len1),pstat)

*      Fit the line

                  nnew  = nnew + 1
                  crash = .false.
                  call profile_fit(sg_parms,sg_error,wavdim,sdata,
     :                             %VAL(CNF_PVAL(d_vsptr)),ixws,ltram,
     :                             status)
                  call opt_release(status)
                  if(status.ne.SAI__OK) return
                  call store_results(sg_parms,sg_error,nnew,nfailed,
     :                               line,istartx,iendx,default_model,
     :                               0.0,%VAL(CNF_PVAL(d_rptr)),
     :                               %VAL(CNF_PVAL(d_vptr)),
     :                               %VAL(CNF_PVAL(staptr)),
     :                               %VAL(CNF_PVAL(d_mptr)),1,1,
     :                               VAL__BADR)

*             Update trams, if the fitting didn't crash, and the answer
*             is believable (we will not allow the centre to move too
*             much)

                  diftrm = xwe - xws
                  if((.not.crash).and.(sg_parms(4).gt.(xws-diftrm))
     :                 .and.(sg_parms(4).lt.(xwe+diftrm))) then
                     left_r(line) = sg_parms(4) - diftrm*0.5
                     right_r(line) = sg_parms(4) + diftrm*0.5

* check in range

                     diff = left_r(line) - sdata(1)
                     if(diff.lt.0.0) then
                        left_r(line) = sdata(1)
                        right_r(line) = left_r(line) + diftrm
                     end if
                     diff = sdata(wavdim) - right_r(line)
                     if(diff.lt.0.0) then
                        right_r(line) = sdata(wavdim)
                        left_r(line) = right_r(line) - diftrm
                     end if

*           ...check on CRASHES

                  end if

*     ...of inner loop

               end do

*    Increment accumulators and print out some diagnostics range

               nfailsum = nfailsum + nfailed
               noldsum  = noldsum  + nold
               nnewsum  = nnewsum  + nnew

*    Check if fits have been added ,converted or deleted

               if(nnew.ne.0.or.nold.ne.0) then
                  write(chars,'(2(2x,a,i4))')
     :                 'BLOCK :',iblock,'  - New Fits :',nnew
                  call opt_wruser(chars,pstat)
                  write(chars,'(2(5x,a,i4))')'- Failed fits    :',
     :                 nfailed,'- Already fitted :',nold
                  call opt_wruser(chars,pstat)
               end if

*   ...of main loop

            end do
            eblock=nbls
            sblock=midblock+1
            do_dirn=1

* nrun

            end do

*  END OF MAIN LOOP

*  Send useful diagnostics to user

            if( exception ) then
               call par_wruser(
     :' ** Exceptional conditions have occurred (listed in arc.ovf) **'
     :              ,pstat)
            end if
*
            call underscore
            write(chars,'(2x,a,i6)')
     :           'Total Number of New Points Fitted  =',nnewsum
            call par_wruser(chars,pstat)
            write(chars,'(2x,a,i6)')
     :           'Total Number of failed Fits        =',nfailsum
            call par_wruser(chars,pstat)
            write(chars,'(2x,a,i6)')
     :           'Total Number Previously Processed =',noldsum
            call par_wruser(chars,pstat)
            call underscore
            iteration = iteration + 1
            end
