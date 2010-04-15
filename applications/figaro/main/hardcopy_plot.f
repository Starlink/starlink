      subroutine hardcopy_plot(fitsta,results,resvar,line,istartx,iendx,
     :     istarty,iendy,line_name,status)
*+
* Name:
*    HARDCOPY_PLOT

* Invocation:
*    CALL HARDCOPY_PLOT(FITSTA,RESULTS,RESVAR,LINE,ISTARTX,IENDX,
*          ISTARTY,IENDY,LINE_NAME,STATUS)

* Purpose:
*  To extract and plot results from the data cube.

* Description:
*   The fit(s) in the current block are located. If there is more than
*   1 the user is asked to choose which. The profile is then plotted
*   with the fit superimposed, and a residuals plot.

* Arguments:
*     RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results block
*     RESVAR(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results block variance
*     FITSTA(3,NYP,SPDIM1,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status
*     LINE = INTEGER (Given)
*        Current line
*     ISTARTX = INTEGER (Given)
*        Starting cross-section
*     IENDX = INTEGER (Given)
*        End cross-section
*     ISTARTY = INTEGER (Given)
*        Start in Y
*     IENDY = INTEGER (Given)
*        End in Y
*     LINE_NAME(LINE) = CHARACTER*10 ARRAY (Given)
*        Line names
*
* Global variables:
*     WAVDIM = INTEGER (Given)
*        Number of channels in data (include file arc_dims)
*     NXP,NYP,MXPARS = INTEGER (Given)
*        Dimensions of results block (include file arc_dims)
*     D_WPTR = INTEGER (Given)
*        Pointer to rest-wavelengths of lines (include file arc_dims)
*     D_XPTR = INTEGER (Given)
*        Pointer to X array (include file arc_dims)
*     D_LPTR(LINE) = INTEGER ARRAY (Given)
*        Pointer to left tram lines (include file arc_dims)
*     D_RPTR(LINE) = INTEGER ARRAY (Given)
*        Pointer to right tram lines (include file arc_dims)

* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92, then
*        Durham

* History:
*   TNW: Bug fix 6/7/88
*   TNW: 24/11/88 Tidied a bit-current AUTOGRAPH state only saved if
*        plotting to be performed. Also changed to use GET_PREVFIT
*   TNW: 27/1/89 Minor changes to logic
*   TNW: 20/11/89 Made to use plot_line
*   TNW: 3/90 PGPLOT version
*   TNW: 1-8/7/91 Changes for new results structure
*   TNW: 3/9/91 Workspace obtained in plot_line. Only consider
*        successful or NAG-error fits
*   TNW: 8/1/93 Bug fixes.
*-
      implicit none
      integer status
      include 'arc_dims'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'gr_inc'
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2)
      integer fitsta(3,nyp,spdim1,spdim2)
      integer line
      integer istartx,iendx,istarty,iendy
      character*10 line_name(line)
* ----------------------------------------------------------------------
      integer ix,iy
      include 'status_inc'
      integer fit_chosen
      integer pstat
      real value,rhnwin
      real rcenx,rceny
      integer cenx,ceny,st,en
      integer chr_len
      integer i,nfit,len1,j
      integer location(100,2)
      character*32 chars
      logical qstat,par_qnum
      integer get_parnum,ppos1,ppos2

* Get and decode fit_status, with meaningful output

      nfit=0
      j = istarty
      ppos1 = get_parnum('Space1_pos')
      if(spdim2.gt.1) then
        ppos2 = get_parnum('Space2_pos')
      else
        ppos2 = 0
        rceny = 1.0
      end if
      call par_wruser('Previous fits:-',pstat)
      do while(j.le.iendy)
        i=istartx
        do while(i.le.iendx)
          call decode_status(ncntrl,fitsta(1,line,i,j),deccntr)

*   fit present

          if((deccntr(FIT_STAT).eq.1).or.(deccntr(FIT_STAT).eq.2)) then
            rcenx = results(ppos1,line,i,j)
            cenx = nint(rcenx)
            if(ppos2.ne.0) then
              rceny = nint(results(ppos2,line,i,j))
              ceny = nint(rceny)
            end if

*       To simplify matters, we will only accept fits if they are actually
*       at the current position or outside the range (they are of course
*       stored in the range, so they must overlap it).

            if(
     :           ( (cenx.eq.i) .or.
     :           ( (i.eq.istartx).and.(cenx.lt.istartx) ) )
     :           .or. ( (i.eq.iendx).and.(cenx.gt.istartx) )
     :           .and.( (ceny.eq.j).or.((ppos2.ne.0).and.(
     :           ( (j.eq.istarty).and.(ceny.lt.istarty) ).or.
     :           ( (j.eq.iendy).and.(ceny.gt.iendy) ) ) ) ) ) then
              nfit = nfit+1
              location(nfit,1) = i
              if(spdim2.gt.1) location(nfit,2) = j
              len1 = 0
              rhnwin = resvar(ppos1,line,i,j)
              if(rhnwin.gt.0.0) rhnwin = sqrt(rhnwin)
              st = nint(rcenx-rhnwin)
              en = nint(rcenx+rhnwin)
              call chr_puti(nfit,chars,len1)
              call chr_putc(': X =',chars,len1)
              call encode_range(' ',' ',st,en,chars,len1)
              if(ppos2.ne.0) then
                rhnwin = resvar(ppos2,line,i,j)
                if(rhnwin.gt.0.0) rhnwin = sqrt(rhnwin)
                st = nint(rceny-rhnwin)
                en = nint(rceny+rhnwin)
                call chr_putc(', Y =',chars,len1)
                call encode_range(' ',' ',st,en,chars,len1)
              end if
              call par_wruser(chars(:len1),pstat)
              if(nfit.ge.100) then
                call par_wruser('Too many fits in range-will save 100'
     :               ,pstat)
                i=iendx
                j=iendy
              end if
            end if
          end if
          i=i+1
        end do
        j=j+1
      end do


* fit present

      if(nfit.ge.1) then

        if(nfit.gt.1) then

          write(chars,'(i4,'' previous fits in range'')')nfit
          call par_wruser(chars,pstat)
          qstat = par_qnum('Which fit?',1.0,real(nfit),1.0,.true.
     :         ,'Integer',value)
          fit_chosen = nint(value)
        else
          fit_chosen = 1
        end if
        ix = location(fit_chosen,1)
        if(spdim2.gt.1) then
          iy = location(fit_chosen,2)
        else
          iy = 1
        end if

* Output fit results

        call par_wruser('Previous fit : -',pstat)

        call output_fit(fitsta,results,%VAL( CNF_PVAL(d_vptr) ),
     :       ix,iy,line,deccntr,-1,.true.,.true.,line_name,
     :       %VAL( CNF_PVAL(d_wptr) ),.false.)

*      Prepare labels for plot

        title='File '//datafile(:chr_len(datafile))//'.DST'

*      Perform plotting

        call plot_line(line,.false.,0.0,%VAL( CNF_PVAL(d_wptr) ),ix,iy,
     :       .false.,0,line_name,results,%VAL( CNF_PVAL(d_vptr) ),
     :       status)

      else

        call par_wruser('No previous successful fit',pstat)

      end if

      call clgrap

      end
