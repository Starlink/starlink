      logical function plot_old_fit(results,resvar,line,istartx,iendx,
     :     istarty,iendy,same_blocking,plot_if_diff,fitsta)
*+
* Name:
*    PLOT_OLD_FIT

* Invocation:
*   (LOGICAL) = PLOT_OLD_FIT(RESULTS,RESVAR,LINE,ISTARTX,IENDX,
*          ISTARTY,IENDY,SAME_BLOCKING,PLOT_IF_DIFF,FITSTA)

* Purpose:
*  Plot previous fits

* Description:
*     To extract and plot results from the data cube. This is intended
*   to enable previous fits to be inspected prior to making a new fit.
*     The positions of the parameters are obtained by the routine
*   get_parnum, and thus are not fixed.
*
* Arguments:
*     RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results cube
*     RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Variance on results
*     FITSTA(MXPARS,NYP,NXP) = INTEGER ARRAY (Given)
*        Fit status
*     LINE = INTEGER (Given)
*        Number of line
*     ISTARTX = INTEGER (Given)
*        Starting cross-section of block
*     IENDX = INTEGER (Given)
*        End cross-section of block
*     PLOT_IF_DIFF = LOGICAL (Given)
*        If to plot if the blocking is different
*
* Global variables:
*     D_VSPTR = INTEGER (Given)
*        Index to Intensity data (include file arc_dims)
*     D_XPTR = INTEGER (Given)
*        Index to X array data (include file arc_dims)
*     D_TLPTR = INTEGER (Given)
*        Index to left tram of line location (include file arc_dims)
*     D_TRPTR = INTEGER (Given)
*        Index to right tram of line location (include file arc_dims)
*     MXPARS,NYP,NXP = INTEGER (Given)
*        Dimensions of results cube (include file arc_dims)
*     WAVDIM = INTEGER (Given)
*        Number of channels in data (include file arc_dims)
*     PLOT_OLD_FIT = LOGICAL (Returned)
*        True if plot made (include file arc_dims)
*     SAME_BLOCKING = LOGICAL (Returned)
*        True if the plot was of the same blocking (include file
*        arc_dims)
*
* Author:
*  T.N.Wilkins Manchester
* History:
*  T.N.Wilkins Manchester removal of look-up tables, 27/1/89
*       "      Cambridge Change to use output_fit & get_parnum 19/7/89
*       "         "      New results structures 1-8/7/91
*       "         "      More use made of ARC_DIMS, 16/8/91
*-
      implicit none
      integer status
      include 'arc_dims'
      include 'opt_cmn'
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2)
      integer fitsta(ncntrl,nyp,spdim1,spdim2)
      integer len1
      integer line
      integer istartx,iendx,istarty,iendy
      logical same_blocking
      logical plot_if_diff
* ----------------------------------------------------------------------
      include 'status_inc'
      integer nwindx,nwindy
      integer ix,iy
      integer cstat
      integer first_ix,last_ix,first_iy,last_iy
      integer fit_chosen
      logical replot,qstat,par_qnum
      real fit_parms(28)
      integer nwindow(2,100)
      real value,value2
      integer i,nfit,j
      real odensc
      integer start(2,100)
      integer location(2,100)
      integer ppos
      integer get_parnum
      character*32 chars

* Get and decode fit_status, with meaningful output

      nfit=0
      plot_old_fit=.false.

* so can be set up to plot fits in scan,

      same_blocking = .true.

*  ....even if no fit at current block

      j = istarty
      do while(j.le.iendy)
        i=istartx
        do while(i.le.iendx)
          call decode_status(ncntrl,fitsta(1,line,i,j),deccntr)

*   fit present

          if(deccntr(FIT_STAT).gt.0) then
            nfit=nfit+1
            ppos = get_parnum('Space1_pos')
            value = results(ppos,line,i,j)
            value2 = resvar(ppos,line,i,j)
            if(value2.gt.0.0) value2 = sqrt(value2)
            start(1,nfit)=nint(value - value2 + 0.5)
            nwindow(1,nfit)=nint(value2*2.0)
            location(1,nfit) = i
            if(spdim2.gt.1) then
              ppos = get_parnum('Space2_pos')
              value = results(ppos,line,i,j)
              value2 = resvar(ppos,line,i,j)
              if(value2.gt.0.0) value2 = sqrt(value2)
              start(2,nfit)=nint(value - value2 + 0.5)
              nwindow(2,nfit)=nint(value2*2.0)
              location(2,nfit) = j
            end if
            if(nfit.gt.1) then
              if((start(1,nfit-1).eq.start(1,nfit)).and.
     :             (nwindow(1,nfit-1).eq.nwindow(1,nfit))) nfit=nfit-1
              if(nfit.ge.100) then
                call par_wruser('Too many fits in range will save 100'
     :               ,status)
                i=iendx
              end if
            end if
          end if
          i=i+1
        end do
        j=j+1
      end do

      if(nfit.gt.1) then

*   If more than one fit in range then ask user which to plot, unless
*   only to plot if the blocking is the same.

        write(chars,'(i4,'' previous fits in range : -'')')nfit
        call par_wruser(chars,status)
        do i = 1,nfit
          last_ix = min(spdim1,(start(1,i)+nwindow(1,i)-1))
          len1 = 0
          call chr_putc('Fit ',chars,len1)
          call chr_puti(i,chars,len1)
          call chr_putc('Position ',chars,len1)
          call encode_range(' ',' ',start(1,i),last_ix,chars,len1)
          if(spdim2.gt.1) then
            last_iy = min(spdim2,(start(2,i)+nwindow(2,i)-1))
            call encode_range(' ',' ',start(2,i),last_iy,chars,len1)
          end if
          call par_wruser(chars(:len1),status)
        end do
        if(.not.plot_if_diff) then
          same_blocking=.false.
          return
        end if
        qstat = par_qnum('Which fit?',1.0,real(nfit),1.0,.true.
     :       ,'Integer',value)
        fit_chosen=nint(value)

      else if(nfit.eq.1) then

*   Case of one fit in range

        fit_chosen = 1

      else

*   No fit found, so exit from routine

        call par_wruser('No previous fit',status)
        return
      end if

*   Store location of fit

      first_ix = start(1,fit_chosen)
      ix=location(1,fit_chosen)
      nwindx=nwindow(1,fit_chosen)
      if(spdim2.gt.1) then
        first_iy = start(2,fit_chosen)
        iy=location(2,fit_chosen)
        nwindy=nwindow(2,fit_chosen)
      else
        first_iy = 1
        iy = 1
        nwindy = 1
      end if

* fit present

      if(nfit.ge.1) then

*   Output fit results

        call par_wruser('Previous fit : -',status)
        call output_fit(fitsta,results,resvar,ix,iy,line,deccntr,-1,
*     :       .true.,.true.,dynamic_chars(idsptr:idsend),
     :       .true.,.true.,idstring,
     :       %VAL( CNF_PVAL(d_wptr) ),.false.)
        cstat = deccntr(FIT_STAT)
        if((cstat.eq.1).or.(cstat.eq.2).or.(cstat.eq.5)) then
          if(deccntr(FIT_NCMP).ge.1) then
            call getres(results,line,ix,iy,fit_parms,deccntr,odensc,
     :                  fitsta,status)
            densc = dble(odensc)
          end if
          plot_old_fit=.true.

          replot =.false.
          last_ix = min(spdim1,(first_ix+nwindx-1))
          last_iy = min(spdim2,(first_iy+nwindy-1))

*      If we need to extract the intensity data to make the plot the do so

          if((first_ix.ne.istartx).or.(last_ix.ne.iendx).or.
     :         (first_iy.ne.istarty).or.(last_iy.ne.iendy)) then
            if(plot_if_diff) then
              call extr3(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,spdim2,
     :             first_ix,last_ix,first_iy,last_iy,
     :             %VAL(CNF_PVAL(d_vsptr)))
              replot=.true.
            else
              same_blocking=.false.
              return
            end if
          end if
          same_blocking = .not.replot
          if(replot) then

*    Replot data

            len1 = 0
            call chr_fill(' ',legend(1))
            call chr_putc('Old fit',legend(1),len1)
            call chr_putc('(x=',legend(1),len1)
            call encode_range(' ',' ',first_ix,
     :           (first_ix+nwindx-1),legend(1),len1)
            if(spdim2.gt.1) then
              call chr_putc(', y=',legend(1),len1)
              call encode_range(' ',' ',first_iy,
     :           (first_iy+nwindy-1),legend(1),len1)
            end if
            call chr_putc(')',legend(1),len1)
          end if

          status = SAI__OK

*       Perform the plotting

          call line_plot(fit_parms,%VAL(CNF_PVAL(d_xptr)),
     :                   %VAL(CNF_PVAL(d_vsptr)),
     :                   %VAL(CNF_PVAL(d_tlptr)),
     :                   %VAL(CNF_PVAL(d_trptr)),line,deccntr,replot,
     :                   .true.,first_ix,nwindx,status)

*   fit a success or nag error

        end if
      else
        call par_wruser('No previous fit',status)

* fit present

      end if
      end
