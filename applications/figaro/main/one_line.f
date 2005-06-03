      subroutine one_line(nwindow,left,right,istartx,iendx,nfit,
     :   line_name,control,line,nnew,nold,nfailed,maskedout,plotwind,
     :   manual,redraw,istarty,iendy,status)
*+
* Name:
*    ONE_LINE

* Invocation:
*    CALL ONE_LINE(NWINDOW,LEFT,RIGHT,ISTARTX,IENDX,NFIT,
*        LINE_NAME,CONTROL,LINE,NNEW,NOLD,NFAILED,MASKEDOUT,PLOTWIND,
*        MANUAL,REDRAW,ISTARTY,IENDY,STATUS)

*
* Purpose:
*   To control the fitting and storing of results in the program LONGSLIT.

* Description:
*   The masking and fit status are checked, and then if we are still to
*   procede the fit is made and then can be stored. This can also plot
*   the line profile with any previous fit superimposed.

* Arguments:
*     NWINDOW = INTEGER (Given)
*        Current WINDOW width
*     LEFT(LINE) = REAL ARRAY (Given)
*        left trams
*     RIGHT(LINE) = REAL ARRAY (Given)
*        right trams
*     ISTARTX = INTEGER (Given)
*        Starting X position of line
*     IENDX = INTEGER (Given)
*        End X position of line
*     LINE_NAME(LINE) = CHARACTER*10 ARRAY (Given)
*        name of line being worked on
*     LINE = INTEGER (Given)
*        Line number being worked on
*     PLOTWIND = LOGICAL (Given)
*        To plot windows.
*     MANUAL = LOGICAL (Given)
*        Affects use of mask and storing of
*                             results-you are allowed to override
*                             fits if this is true, even if masking
*                             of fit type would otherwise prevent
*                             it.
*     INHERIT (in common) = INTEGER ARRAY (Given)
*        Use of INHERIT:
*                             0 - "Normal mode" no use of previous fits
*                                (except interactive multiples)
*                            +1 - inherit from next block
*                            -1 - inherit from last block
*     ISTARTY = INTEGER (Given)
*        Starting position in Y to consider
*     IENDY = INTEGER (Given)
*        End position in Y to consider
*     WINDOWY = INTEGER (Given)
*        Blocking in Y
*     NFIT = INTEGER (Given and returned)
*        Number of fits so far
*     CONTROL(NYP,NXP,SPDIM2) = INTEGER ARRAY (Given and returned)
*        current fit control
*     NNEW = INTEGER (Given and returned)
*        No. of new fits
*     NOLD = INTEGER (Given and returned)
*        No. of previous fits
*     NFAILED = INTEGER (Given and returned)
*        Number of failed fits
*     MASKEDOUT = INTEGER (Given and returned)
*        Number of points masked out
*     REDRAW = INTEGER (Given and returned)
*        If calling routine is to redraw data
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=ok

* Global variables:
*     TERMINAL = LOGICAL (Given and returned)
*        If softcopy device open (include file gr_inc)
*     D_VSPTR = INTEGER (Given and returned)
*        "Pointer" to WAVDIM of REAL workspace (include file arc_dims)
*     D_MPTR = INTEGER (Given and returned)
*        "Pointer" to mask array (include file arc_dims)
*     ERRPTR = INTEGER (Given and returned)
*        "Pointer" to errors array (include file arc_dims)
*     STAPTR = INTEGER (Given and returned)
*        "Pointer" to status array (include file arc_dims)
*     D_XPTR = INTEGER (Given and returned)
*        "Pointer" to wavelength array (include file arc_dims)
*     NCNTRL = INTEGER (Given and returned)
*        Number of elements in control per position (include file arc_dims)
*     NYP = INTEGER (Given and returned)
*        Number of line slots (include file arc_dims)
*     NXP = INTEGER (Given and returned)
*        3rd dimension of results block (include file arc_dims)
*     SPDIM2 = INTEGER (Given and returned)
*        2nd spatial dimension of data (include file arc_dims)
*     WAVDIM = INTEGER (Given and returned)
*        Wavelength dimension of data (include file arc_dims)
*     PRFITS = LOGICAL (Given and returned)
*        If to output details of fitting (include file opt_cmn)

* Subroutines/functions referenced:
*      DISP_WINDOW       : Display data in current window
*      CHECK_MASKING     : Check whether previous fits protected by
*                          masking
*      CHECK_STATII      : Check whether a more "complicated" fit
*                          has been made here
*      DECODE_STATUS     : Decode the fit status word
*      FIT_LINE          : Perform optimisation
*      RX2CHN            : Convert real X value to channel number
*      STORE_RESULTS     : Store results of fitting
*
*      DSA_FREE_WORKSPACE : Free workspace
*      PAR_QUEST         : Get yes/no response from user
*      PAR_WRUSER        : Write character string to user

* Author:
*   T.N.Wilkins Manchester until 1/89, then Cambridge until 9/92, then Durham

* History:
*   Original version, TNW
*   Minor tidying up, TNW 13/6/88
*   Altered to reduce use of common TNW 21/7/88
*   Call to DSA_FREE_WORKSPACE added (here so if
*   crashes workspace is still freed). TNW 26/10/88
*   ERROR argument, TNW 11/11/88
*   TNW 21/9/89 Minor changes
*   Changes to allow variable base and weighting in inherit_fit,
*     TNW 21/12/90
*   MANUAL argument added, changes to allow new format CONTROL
*   array, 9-10/4/91 TNW
*   New fit encoding, TNW 28/5/91
*   New results structure TNW 1-8/7/91
*   New FIT_LINE, removing need for INHERIT_FIT, TNW 9/91
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'arc_dims'
* Given

      integer line
      logical plotwind,manual
      character*10 line_name(line)
      real left(line)
      real right(line)
      integer nwindow
*
      integer istartx
      integer iendx
      integer istarty,iendy

* Altered:

      integer nold
      integer nnew
      integer nfit
      integer nfailed
      integer control(ncntrl,nyp,nxp,spdim2)
      integer maskedout
      integer pstat

* Returned

      logical redraw

* Common blocks

      include 'gr_inc'
      include 'opt_cmn'
      include 'status_inc'
      include 'fit_coding_inc'
* ---------------------------------------------------------------
*
*  Local
*

* Pointer to subset of error array (possibly co-added)

      integer ersptr

* To hold slot used for ersptr

      integer eslot

      logical mstore,ok

* parameters

      real fitpar(max_parms)

* errors on parameters

      real fiterr(max_parms)
      character*23 chars
      integer len1
*

* if current point masked out

      logical masked

* if fit to be performed

      logical fit

* if to store results

      logical stores

      integer m
      integer start
      real odensc
      real aic

*   functions

      logical par_quest
      integer rx2chn

* Trick to pass address to subroutines

      include 'DYNAMIC_MEMORY'

* If status non-zero then something is wrong, so we don't want to
* continue

      if(status.ne.SAI__OK) return

* set up parameters defining current X-Y location

      icrash  = istartx
      jcrash = istarty
      kcrash = line
      nfit   = nfit+1
      legend(2) = line_name(line)
      start  = rx2chn(dynamic_mem(d_xptr),wavdim,left(line))
      m  = rx2chn(dynamic_mem(d_xptr),wavdim,right(line)) - start + 1
*
*   set title
*
      if(prfits) then
        len1 = 0
        call chr_appnd(line_name(line),chars,len1)
        call chr_putc(' (line = ',chars,len1)
        call chr_puti(line,chars,len1)
        call chr_putc(')',chars,len1)
        call par_wruser(chars(:len1),pstat)
      end if

* check if masked out


* .. by definition to start with

      masked = .false.

      call check_masking(line,istartx,iendx,istarty,iendy,masked,
     :       %VAL( CNF_PVAL(d_mptr) ))

* if not masked out check to see if possible to make an improvement
* on the existing fit

* allow to over-ride masking if in manual mode

      if ((.not.masked).or.manual) then

        call check_statii(line,fit,nold,control,istartx,iendx,istarty,
     :            iendy,%VAL( CNF_PVAL(staptr) ))

*.............  Fit the line.................

        if(fit.or.manual) then

*
*      display window
*
          if ((terminal).and.plotwind) then
            call disp_window(left,right,line,dynamic_mem(d_xptr),
     :           dynamic_mem(d_vsptr),wavdim)
          end if
          nnew  = nnew + 1
          crash = .false.

* decode the control element

          call decode_status(ncntrl,control(1,line,istartx,istarty),
     :            deccntr)

* proceede with model fit

          mstore=.false.

*   Copy errors array into 1-d array for use with optimisation routines,
*   assuming of course that the error array is present.

          if(deccntr(FIT_WEIGH).eq.VARIANCE) then
            call getwork(wavdim,'float',ersptr,eslot,status)
            if(status.ne.SAI__OK) return
            call cop_2_1d_err(dynamic_mem(errptr),istartx,iendx,
     :                  istarty,iendy,dynamic_mem(ersptr))
          else

*        Although not used, this plays safe for systems which don't like
*        passing invalid arguments

            ersptr = d_vsptr
          end if

*     Temporary bit to deal with inherit

          if(inherit.ne.0) then
            deccntr(FIT_GUES) = 5 + inherit
            deccntr(FIT_MAN) = MAN_NOALTER
          end if

*     At last the actual fit

          call fit_line(deccntr,dynamic_mem(d_xptr),
     :         dynamic_mem(d_vsptr),start,m,line,nwindow,istartx,
     :         istarty,odensc,dynamic_mem(ersptr),fitpar,fiterr,mstore,
     :         aic,status)
          ok = .true.
          call opt_release(status)

          if(deccntr(fit_weigh).eq.VARIANCE)
     :         call dsa_free_workspace(eslot,status)

*   Now do we store the fit/

          stores = .false.
          if ((.not.manual).or.(mstore.and.fit.and.(.not.masked))) then
            stores = ok
          else if((deccntr(FIT_MAN).eq.MAN_NOALTER).or.
     :           (((.not.fit).or.masked).and.mstore)) then
            if(masked) call opt_wruser(
     :        '*** WARNING :- POINT MASKED OUT***',pstat)
            if(.not.fit) call opt_wruser(
     :        '*** WARNING :- PREVIOUS FIT WITH MORE PARAMETERS***'
     :                         ,pstat)
            stores = par_quest('Store fit results?',
     :                  (.not.masked).and.fit)
          end if

          if(stores) then
            call store_results(fitpar,fiterr,nnew,nfailed,line,
     :           istartx,iendx,deccntr,odensc,%VAL( CNF_PVAL(d_rptr) ),
     :           %VAL( CNF_PVAL(d_vptr) ),%VAL( CNF_PVAL(staptr) ),
     :           %VAL( CNF_PVAL(d_mptr) ),istarty,iendy,aic)
          end if

          if (manual) then
            redraw = par_quest('redraw current data?',.false.)
          end if
        else
          call opt_wruser('Could not over-ride previous fit',pstat)

*   ...FIT

        end if
      else
        maskedout=maskedout+1

* ...MASKEDOUT

      end if
      end
