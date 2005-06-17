      subroutine plot_line(line,ifsoft,vcorr,wavlen,ix,iy,velplt,vtype,
     :                  linnam,results,resvar,status)
*+
* Name:
*    PLOT_LINE

* Invocation:
*    CALL PLOT_LINE(LINE,IFSOFT,VCORR,WAVLEN,IX,IY,VELPLT,VTYPE,
*                       LINNAM,RESULTS,RESVAR,STATUS)

* Purpose:
*   Plot line profile with fit super-imposed

* Description:
*    To plot a line profile. This allows for the plot to be in X axis
*    units or in velocity (km/s). The data is plotted with the fit
*    superimposed, and the residuals from the fit are plotted above.
*
* Arguments:
*      LINE = INTEGER (Given)
*        Number of line to plot
*      IFSOFT = LOGICAL (Given)
*        If to plot in softcopy
*      VCORR = REAL (Given)
*        Velocity correction
*      WAVLEN(LINE) = REAL ARRAY (Given)
*        Line rest wavelengths
*      IX = INTEGER (Given)
*        X position of fit (anywhere in range)
*      IY = INTEGER (Given)
*        Y position of fit (anywhere in range)
*      VELPLT = LOGICAL (Given)
*        If to use velocity scale for X axis
*      VTYPE = INTEGER (Given)
*        Which velocity correction is used
*      LINNAM(LINE) = CHARACTER*10 ARRAY (Given)
*        Line names
*      RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results array
*      RESVAR(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results array variance
*
* Global variables:
*      D_SPTR = INTEGER (Given)
*        Pointer to main file data array
*      D_TLPTR = INTEGER (Given)
*        Pointer to left tram lines
*      D_TRPTR) = INTEGER ARRAY (Given)
*        Pointer to right tram lines
*      D_XPTR = INTEGER (Given)
*        Pointer to X array data
*      WAVDIM = INTEGER (Given)
*        Number of channels in data
*      SPDIM1,SPDIM2 = INTEGER (Given)
*        Number of cross-sections in data
*      LEGEND(2) = CHARACTER*(*) ARRAY (Given)
*        Legends for plot
*      TITLE = CHARACTER*(*) (Given)
*        Title for plot
*      XUNITS = CHARACTER*(*) (Given)
*        X units for plot
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*    Subroutines/functions referenced:
*
* Author:
*   T.N.Wilkins, Cambridge, 17-20-NOV-1989
*
* History:
*   T.N.Wilkins, Cambridge, 24,28-MAY-1991, new fit model arrays
*       "           "       3-SEP-1991 Changes to workspace handling
*       "           "       26-MAR-1992 Changes for 3-d data
*       "        Durham,    17-MAY-1993 Bug fix
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'PRM_PAR'

* Common

      integer status
      include 'arc_dims'
      include 'opt_cmn'

* Arguments

      integer line
      logical ifsoft
      real vcorr
      real wavlen(line)
      integer xwidth,ywidth
      integer ix,iy
      logical velplt
      integer vtype
      character*10 linnam(line)
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2)

* Local

      real fitpars(max_parms),rdensc,value
      integer work1,work2,nwork
      integer xstart, xend, ystart, yend
      integer len1,tnw_cputr,get_parnum,npts,ptsinfit
      integer slot,ppos,jx,jy,px,py,ppos1,ppos2,tmpptr
      logical ifblocked,ok,add
      character*30 dxunit
      character*3 vctype(2)
      character bss*2,bs*1
      include 'status_inc'
      data vctype/'HEL','LSR'/
      data bss/'\\'/
      bs = bss(1:1)

* Initial values (saves complication dealing with 3-d data etc.

      ystart = 1
      yend = 1
      xstart = 1
      xend = 1

      call getres(results,line,ix,iy,fitpars,deccntr,rdensc,
     :            %VAL(CNF_PVAL(staptr)),status)
      densc = dble(rdensc)
      call chr_fill(' ',legend(1))
      len1 = 0
      call chr_appnd(linnam(line),legend(1),len1)
      call chr_putc(' (',legend(1),len1)
      status = tnw_cputr('f7.2',wavlen(line),legend(1),len1)
      call chr_putc(')',legend(1),len1)

* Get workspace

      if(velplt) then
        nwork = wavdim * 2
      else
        nwork = wavdim
      end if
      call dsa_get_work_array(nwork,'float',work1,slot,status)
      if(status.ne.SAI__OK) return

      call chr_fill(' ',legend(2))
      len1 = 0

* The data is taken as blocked if there are valid numbers for the
* blocking for all dimensions which are greater than 1

      ifblocked = .true.
      if(spdim1.gt.1) then
        ppos1 = get_parnum('Space1_pos')
        if(ppos1.gt.0) then
          value = resvar(ppos1,line,ix,iy)
          ifblocked = value.ne.VAL__BADR
        else
          ifblocked = .false.
        end if
      end if
      if(spdim2.gt.1) then
        ppos2 = get_parnum('Space2_pos')
        if(ppos2.gt.0) then
          value = resvar(ppos2,line,ix,iy)
          ifblocked = (value.ne.VAL__BADR).and.ifblocked
        else
          ifblocked = .false.
        end if
      end if

      if(ifblocked) then
        if(spdim1.gt.1) then
          xwidth = nint(sqrt(resvar(ppos1,line,ix,iy))*2.0)
          xstart = nint(results(ppos1,line,ix,iy)-real(xwidth)*0.5)
          xend = xwidth + xstart - 1
        end if
        if(spdim2.gt.1) then
          ywidth = nint(sqrt(resvar(ppos2,line,ix,iy))*2.0)
          ystart = nint(results(ppos2,line,ix,iy)-real(ywidth)*0.5)
          yend = ywidth + ystart - 1
        end if

        if(spdim1.gt.1) then
          call chr_putc('X ',legend(2),len1)
          call encode_range(' ',' ',xstart,xend,legend(2),len1)
        end if
        if(spdim2.gt.1) then
          call chr_putc(',Y ',legend(2),len1)
          call encode_range(' ',' ',ystart,yend,legend(2),len1)
        end if

* Extract data

        call extr3(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,spdim2,xstart,
     :             xend,ystart,yend,%VAL(CNF_PVAL(work1)))
      else

* If data isn't blocked we will have to check all locations until we've
* found all the points included!
        ppos = get_parnum('Pts_in_Fit')
        ptsinfit = nint(results(ppos,line,ix,iy))
        jx = 1
        jy = 1
        add = .false.
        npts = 0
        do while(npts.lt.ptsinfit)
          ok = .true.
          value = results(ppos1,line,jx,jy)
          ok = ok.and.(value.ne.VAL__BADR)
          if(ok) then
            px = nint(value)
            ok = px.eq.ix
          end if
          value = results(ppos2,line,jx,jy)
          ok = ok.and.(value.ne.VAL__BADR)
          if(ok) then
            py = nint(value)
            ok = py.eq.iy
          end if
          if(ok) then
            call chr_putc('(',legend(1),len1)
            call chr_puti(jx,legend(1),len1)
            call chr_putc(',',legend(1),len1)
            call chr_puti(jy,legend(1),len1)
            call chr_putc(')',legend(1),len1)

*        Reference data(1,jx,jy):

            tmpptr = d_sptr
     :           + ((jx - 1) + (jy - 1) * spdim1) * VAL__NBR * wavdim
            call copy2work(%VAL(CNF_PVAL(work1)),wavdim,
     :                     %VAL(CNF_PVAL(tmpptr)),add)
            add = .true.
            npts = npts + 1
          end if
          jx = jx + 1
          if(jx.gt.spdim1) then
            jx = 1
            jy = jy + 1
            if(jy.gt.spdim2) ptsinfit = 0
          end if
        end do
      end if

* Are we to plot using file axis units for X axis of plot, or velocity?

      if(velplt) then
        work2 = work1 + wavdim*VAL__NBR
        dxunit = xunits
        if((vtype.eq.1).or.(vtype.eq.2)) then
           xunits = 'V'//bs//'d'//vctype(vtype)//bs//'u/km s'//bs//
     :          'u-1'//bs//'d'
        else
          xunits = 'km s'//bs//'u-1'//bs//'d'
        end if
        call line_vplot(fitpars,%VAL(CNF_PVAL(work1)),
     :                  %VAL(CNF_PVAL(d_tlptr)),
     :                  %VAL(CNF_PVAL(d_trptr)),line,ifsoft,vcorr,
     :                  wavlen,%VAL(CNF_PVAL(work2)),xstart,xwidth,
     :                  deccntr,status)
        xunits = dxunit
      else
        call line_plot(fitpars,%VAL(CNF_PVAL(d_xptr)),
     :                 %VAL(CNF_PVAL(work1)),%VAL(CNF_PVAL(d_tlptr)),
     :                 %VAL(CNF_PVAL(d_trptr)),line,deccntr,.true.,
     :                 ifsoft,xstart,xwidth,status)
      end if
      call dsa_free_workspace(slot,status)
      end
