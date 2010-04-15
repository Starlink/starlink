      subroutine data_window(status)
*+
* Name:
*    DATA_WINDOW

* Invocation:
*    CALL DATA_WINDOW(STATUS)
*
* Purpose:
*    Automatic blocking/fitting

* Description:
*      The data contained between the current X-sect" LIMITS" is
*      divided into a series of WINDOWS of width NWINDOW.The data
*      fitting then proceeds by rolling up the spwctrum a window
*      at a time until XEND is reached .
*
*  Argument:
*    STATUS = INTEGER (Given)
*        Error status
*
* Author:
*    D.Axon nov, 1984 Anglo-Australian Observatory
*    ACD: A C Davenhall, Starlink, Edinburgh
* History:
*    T.N.Wilkins 1985 Manchester
*         "      1-8/7/91 Cambridge, Altered for new results structures
*         "      10/7/91      "     , it_plus1 removed
*    ACD: 28/9/00 Remove local unused variables.
*
* ----------------------------------------------------------------------
*
*    This program permits analysis of spectra which may be
* binned up spatially before fitting. A Spatial mask is used to
* select the the areas of line-fitting and the fit type,and a
* datablock is used to store the resulting parameters of up to 9
* Gausian fits
*
* MASK has the following structure:-
*
*    Each row of the matrix refers to a seperate spatial and spectral
*    location. the mask contains either 0's to signify no fit has
*    yet been achieved or a positive integer specifiying the itteration
*    number at which the fit was accumplished. the details of the fits
*    are contained in RESULTS .
*
* -------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
*
*  Common Blocks Referenced:-
*
      include 'arc_dims'
*
*          CRASH,ICRASH,JCRASH,EXCEPTION,NAGERROR
*            (l)   (i)     (i)     (l)      (l)
*
*  For the condition handler in GAUSID - needs COMMON
*     as conditions are unpredictable and can occur anywhere !!
*
      include 'opt_cmn'
*-
      integer status

*  Local


* Current WINDOW width

      integer nwindow

* Number of such windows

      integer nbls

* Number of fits so far

      integer nfit

* Start X-sect for current LIMITS

      integer xstart

* end X-sect  for current LIMITS

      integer xend


* No singles=>doubles

      integer sgnowdg

* No. of previous fits

      integer nold

* No. of new fits

      integer nnew

* Line number being worked on

      integer line
      integer istartx

* Channel boundaries for current LINE

      integer iendx
      logical dummy
      integer len1


* number of points masked out

      integer maskedout

* total number of failed fits

      integer nfailsum
      integer ystart,yend
      integer iblock
      integer nfailed
      integer pstat
      integer stblk,endblk,blkinc,stblk2,endblk2,nbls2,jblock
      character*72 chars
      logical par_quest

* All these take Y in spatial sense

      integer istarty,iendy,windowy
      include 'gr_inc'
*
* ---------------------------------------------------------------

      call rangeblk(xstart,xend,nwindow,nbls,ystart,yend,windowy,nbls2
     :           ,status)

* Do we want to plot the fits. If we already have opened a softcopy
* graphics device then we will plot them anyway

      if((inherit.eq.0).and.(.not.batch)) then
        if(par_quest('Plot fits?',.true.)) then
          call gr_soft(status)
        else
          call clgrap
        end if
      end if

      exception = .false.
*
* analyse the spectra in groups of Nwindow X-sects
*
* ----------------------------------------------------------------------
*               START OF MAIN LOOP
* ----------------------------------------------------------------------
*
      maskedout = 0
      noldsum   = 0
      nnewsum   = 0
      nfailsum  = 0

* If inherit is one, then loop over windows in reverse order. Otherwise
* start at block 1 and go to block NBLS

      if(inherit.eq.1) then
        stblk = nbls
        endblk = 1
        stblk2 = nbls2
        endblk2 = 1
        blkinc = -1
      else
        stblk = 1
        endblk = nbls
        stblk2 = 1
        endblk2 = nbls2
        blkinc = 1
      end if
*
*  loop over windows
*
      do jblock=stblk2,endblk2,blkinc

* Same for 2nd spatial dimension as below, but less options at present!!

        istarty = windowy*(jblock-1) + ystart
        iendy   = istarty + windowy - 1
        iendy = min(iendy,yend)
        if(spdim2.gt.1) then
          write(chars,'(''Block (spatial dimension 2) :'',i4)')jblock
          call opt_wruser(chars,pstat)
          len1 = 0
          call encode_range('From ','Position ',istarty,iendy,chars,
     :            len1)
          call opt_wruser(chars(:len1),pstat)
        end if
        do iblock=stblk,endblk,blkinc
*
*     Initialise Counters
*
          nnew    = 0
          nold    = 0
          sgnowdg = 0
          nfailed = 0

          istartx = xstart  + nwindow*(iblock-1)
          iendx   = istartx + nwindow - 1

*     test to see if reached end

          iendx = min(iendx,xend)
*
*     get TITLE and SUBTITLE
*
          write(title,'(''window number '',i4)')iblock
          len1 = 0
          call encode_range('Xsects ','Xsect ',istartx,iendx,
     :            legend(1),len1)
          call opt_wruser(title,pstat)
          call opt_wruser(legend(1),pstat)
*
*     get integral over the window
*
           call extr3(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,spdim2,
     :            istartx,iendx,istarty,iendy,%VAL(CNF_PVAL(d_vsptr)))

*     loop over the lines

          do line=1,line_count
            call one_line(nwindow,%VAL(CNF_PVAL(d_tlptr)),
     :         %VAL(CNF_PVAL(d_trptr)),istartx,iendx,nfit,
*    :         dynamic_chars(idsptr:idsend),%VAL(CNF_PVAL(d_cptr)),line,
     :         idstring,%VAL(CNF_PVAL(d_cptr)),line,
     :         nnew,nold,nfailed,maskedout,.true.,.false.,dummy,
     :         istarty,iendy,status)
            if(status.ne.SAI__OK) return

*     ...of inner loop

          enddo

*      Increment accumulators and print out some diagnostics over
*      masked range.

          nfailsum   = nfailsum + nfailed
          noldsum    = noldsum  + nold
          nnewsum    = nnewsum  + nnew
          sgnowdgsum = sgnowdgsum + sgnowdg

*     Write out how many fits have been added ,converted or deleted.

          if(prfits) then
            write(chars,'(2x,3(a,i4))')'BLOCK :',iblock,'  - New fits :'
     :          ,nnew,' - Failed fits :',nfailed
            call par_wruser(chars,pstat)
            write(chars,'(1x,2(4x,a,i4))')'- Already fitted :',nold,
     :                    '- Singles => Doubles :',sgnowdg
            call par_wruser(chars,pstat)
          end if
        end do

* ...of main loop

      end do
* ----------------------------------------------------------------------
*                          END OF MAIN LOOP
* ----------------------------------------------------------------------
      iteration = iteration + 1
      call updtmsk(%VAL( CNF_PVAL(staptr) ),%VAL( CNF_PVAL(d_mptr) ))
*
*  Send useful diagnostics to user
*
      if( exception ) then
        call par_wruser('** Exceptional conditions have occurred '/
     :    /'at all points listed in ARC.OVF **',pstat)
      end if
*
      call underscore
      call par_wruser('Total numbers:',pstat)
      write(chars,'(2(2x,a,i6))')'New points fitted        =',nnewsum,
     :  'Failed fits        =',nfailsum
      call par_wruser(chars,pstat)
      write(chars,'(2(2x,a,i6))')'Points previously fitted =',noldsum,
     :  'Singles => Doubles =',sgnowdgsum
      call par_wruser(chars,pstat)
      call underscore
      end
