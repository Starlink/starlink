      subroutine line_plot(fitpar,sdata,sdens,left,right,line,deccntr,
     :                  pltdat,ifsoft,xsect,nwindow,status)
*+
* Name:
*    LINE_PLOT

* Invocation:
*    CALL LINE_PLOT(FITPAR,SDATA,SDENS,LEFT,RIGHT,LINE,DECCNTR,
*                       PLTDAT,IFSOFT,XSECT,NWINDOW,STATUS)

* Purpose:
*   To plot a fit to a line profile.

* Description:
*   If pltdat is true then the line profile itself will be plotted, as well
*   as a plot of the residuals on the fit. Otherwise a plot of the line profile
*   is assumed already present.

* Parameters:-
*   FITPAR(*) = REAL ARRAY (Given)
*        Fit parameters, in order: base, width_1,
*                      height_1, centre_1, ...
*   SDATA(WAVDIM) = REAL ARRAY (Given)
*        X array data
*   SDENS(WAVDIM) = REAL ARRAY (Given)
*        Extracted spectrum
*   LINE = INTEGER (Given)
*        Number of line
*   LEFT(LINE) = REAL ARRAY (Given)
*        Tram lines
*   RIGHT(LINE) = REAL ARRAY (Given)
*          "     "
*   PLTDAT = LOGICAL (Given)
*        If to plot data
*   IFSOFT = LOGICAL (Given)
*        If fit is to be in soft rather than hardcopy
*   XSECT = INTEGER (Given)
*        Cross-section of start of fit (only used if
*                      deccntr(back_model)=4)
*   NWINDOW = INTEGER (Given)
*        Number of cross-sections for window (only
*                      used if deccntr(back_model)=4)
* Given in ARC_DIMS:
*   WAVDIM = INTEGER (Given)
*        Number of channels
*   TITLE,LEGEND(3),XUNITS - used by disp_window2 (see comments for
*                            that)

* Authors:
*   T.N.Wilkins Manchester until 1/89, Cambridge until 9/92

* History:
*   TNW: 1986 Original version
*   Changes including to graphics (removal of GR_OPEN e.t.c. 11/87) TNW
*   Bug fix re virtual memory 16-17/12/87  TNW
*   Changes to reduce use of common, TNW 27/5/88
*   Allow for no labels, TNW 7/6/88.
*   TNW: 28/11/88 Use GETWORK, and bytesdef include module
*   TNW: 3/90 PGPLOT version
*   TNW: 24/8/90 Single and multiple bits combined a lot
*   TNW: 4/9/91 Use ARC_DIMS
*   TNW: 21/8/92 Changed so functions gaussian/lorentz passed around as
*         arguments (external)
*-
      implicit none
      include 'SAE_PAR'
      include 'arc_dims'
      include 'status_inc'
      include 'fit_coding_inc'
      integer line
      real left(line),right(line)
      real sdata(wavdim),sdens(wavdim)
      integer xsect,nwindow
      logical pltdat
      logical ifsoft
      real fitpar(*)

* Local

      real frac1,frac2,yrange(3)
      integer wstart
      integer funct
      integer status,nels
      integer m,vbase
      integer resptr,hdatptr,hfvptr,ftotptr,slot,adata,nwork,ptr0
      integer nbad,cnv_fmtcnv
      integer rx2chn
      real gaussian, lorentz, skew, cauchy
      external gaussian, lorentz, skew, cauchy
      include 'PRM_PAR'
      include 'DYNAMIC_MEMORY'

      wstart = rx2chn(sdata,wavdim,left(line))
      m = rx2chn(sdata,wavdim,right(line)) - wstart + 1

      if(pltdat) then

        call gr_selct(ifsoft,status)
        if(status.ne.SAI__OK) return

        frac1 = 0.68
        frac2 = 0.75

* Set up viewport for plot

        call pgvport(0.05,0.97,0.11,frac1)

* Produce plot

        call disp_window2(left(line),right(line),sdata(wstart)
     :                       ,sdens(wstart),m,xunits,.false.)
      end if

      if(deccntr(FIT_NCMP).le.0) then
        call par_wruser('No components in fit',status)
        return
      end if

* Get virtual memory:-
*  ADATA X values to evaluate base (double precision) (M*5) (d)
*  HDATPTR Expanded X array (5 times X resolution)    (M*5) (r)
*  RESPTR Residuals                                     (M) (r)
*  FTOTPTR Total values of fit                   (M or M*5) (r)
*  VBASE Base array                              (M or M*5) (r)
* (multiples only:
*  HFVPTR Values of individual components (5 times X resolution)
*                                        (M*DECCNTR(FIT_NCMP)*5) (r)
*                                         )
*  PTR0 Workspace for FIT_GLBASE (d)
*                DECCNTR(BACK_MODEL) ELEMENTS
*                        2          WAVDIM*2+M
*                        3          WAVDIM*6+460
* Since RESPTR and HDATPTR are not needed at the same time, they have
* the same value.

      nels = (3*VAL__NBR +  VAL__NBD) * 5 * m
      if(deccntr(FIT_NCMP).gt.1) then
        nels = nels + 5*deccntr(FIT_NCMP)*m*VAL__NBR
      end if
      if(deccntr(BACK_MODEL).eq.2)then
        nwork = 2*wavdim+m*5
      else if(deccntr(BACK_MODEL).eq.3)then
        nwork = 6*wavdim+460
      else
        nwork = 0
      end if
      nwork = nwork*VAL__NBD
      call getvm(nels+nwork,ptr0,slot,status)
      if(status.ne.SAI__OK) then
        return
      end if
      adata = ptr0 + nwork
      hdatptr = adata + m*5*VAL__NBD
      ftotptr = hdatptr + m*5*VAL__NBR
      vbase = ftotptr + m*5*VAL__NBR
      resptr = hdatptr
      call fill_dat(sdata(wstart),m,dynamic_mem(hdatptr))
      status = cnv_fmtcnv('real','double',dynamic_mem(hdatptr),
     :                 dynamic_mem(adata),m*5,nbad)
      call fit_glbase(xsect,nwindow,sdata,dynamic_mem(hdatptr),
     :            deccntr,1,m*5,vbase,.false.,dynamic_mem(adata),ptr0,
     :            status)

      funct = deccntr(FIT_MODEL)
      if(deccntr(FIT_NCMP).eq.1) then

*   Put contents of array fitpar into array in order as required for
*   plot_fit, so as to evaluate residuals

        call plot_fit(fitpar,funct,m,dynamic_mem(vbase))

* deccntr(fit_ncmp).gt.1

      else
        hfvptr = vbase + m*5*VAL__NBR
        if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
          call comp_plot(fitpar,deccntr(FIT_NCMP),m*5,
     :         dynamic_mem(hfvptr),dynamic_mem(hdatptr),yrange,
     :         gaussian)
        else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
          call comp_plot(fitpar,deccntr(FIT_NCMP),m*5,
     :         dynamic_mem(hfvptr),dynamic_mem(hdatptr),yrange,
     :         lorentz)
        endif
        call multi_plot(fitpar,deccntr(FIT_NCMP),m*5,
     :      dynamic_mem(hfvptr),dynamic_mem(ftotptr),
     :      dynamic_mem(hdatptr),deccntr(BACK_MODEL),dynamic_mem(vbase)
     :      ,yrange)

      end if
      if(pltdat) then
        if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
          call eval_tot(sdata(wstart),m,dynamic_mem(ftotptr),fitpar,
     :            deccntr(FIT_NCMP),gaussian)
        else if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
          call eval_tot(sdata(wstart),m,dynamic_mem(ftotptr),fitpar,
     :            deccntr(FIT_NCMP),skew)
        else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then
          call eval_tot(sdata(wstart),m,dynamic_mem(ftotptr),fitpar,
     :            deccntr(FIT_NCMP),cauchy)
        else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
          call eval_tot(sdata(wstart),m,dynamic_mem(ftotptr),fitpar,
     :            deccntr(FIT_NCMP),lorentz)
        endif
        status = cnv_fmtcnv('real','double',sdata(wstart),
     :                   dynamic_mem(adata),m,nbad)
        call fit_glbase(xsect,nwindow,sdata,sdata,deccntr,wstart,m,
     :              vbase,.false.,dynamic_mem(adata),ptr0,status)
        call gen_addaf(m,dynamic_mem(ftotptr),dynamic_mem(vbase),
     :         dynamic_mem(ftotptr))
        call pgvport(0.05,0.97,frac2,0.9)
        call multi_resid(sdata(wstart),sdens(wstart),m,
     :        dynamic_mem(resptr),dynamic_mem(ftotptr),.true.,.false.
     :        ,title,legend)
      end if

      call dsa_free_workspace(slot,status)

      call gr_spen(1)
      end
