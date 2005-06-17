      subroutine line_plot_sub(fitpar,sdata,sdens,left,right,line,
     :   wavdim,deccntr,pltdat,xsect,nwindow,mparms,status)
*+
* Name:
*    LINE_PLOT_SUB

* Invocation:
*    CALL LINE_PLOT_SUB(FITPAR,SDATA,SDENS,LEFT,RIGHT,LINE,
*        WAVDIM,DECCNTR,PLTDAT,XSECT,NWINDOW,MPARMS,STATUS)

* Purpose:
*  As line plot, but for small plots on part of a display.

* Description:
*  As line plot, but for small plots on part of a display.

* Arguments:
*   MPARMS = INTEGER (Given)
*        Maximum number of parameters
*   FITPAR(MPARMS) = REAL ARRAY (Given)
*        Fit parameters
*   WAVDIM = INTEGER (Given)
*        Number of channels
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
*        If to plot data as well
*   XSECT = INTEGER (Given)
*        Starting cross-section of fit
*   NWINDOW = INTEGER (Given)
*        Number of cross-sections in fit
*   DECCNTR(*) = INTEGER ARRAY (Given)
*        Profile model of fit
*   STATUS = INTEGER (Given and returned)
*        Error status, 0=ok

* Notes:
*   Note that XSECT and NWINDOW are only used for get_cheb_base if a
*   fit by FITCONT is being used.

* Author:
*   T.N.Wilkins Manchester 11/87

* History:
*   TNW 29/11/88 Changed to use getwork
*   TNW 27/1/89 MPARMS made an argument
*   TNW 29/5/91 New way of passing fit model around
*   TNW 21/8/92 Changed so functions gaussian/lorentz passed around as
*         arguments (external)
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'status_inc'
      include 'fit_coding_inc'
      integer line
      integer wavdim
      real left(line),right(line)
      integer mparms
      real fitpar(mparms)
      real sdata(wavdim),sdens(wavdim)
      logical pltdat
      integer nwindow,xsect

* Local

      real yrange(3)
      integer vbase
      integer wstart
      integer resolution
      integer m
      integer hdatptr,fvalptr,totptr,adata,nbad,cnv_fmtcnv
      integer slot,slot2,slot3,slot4,slot5,slot6
      integer status,basewk,ptr0,narray
      logical highres
      real gaussian, lorentz
      external gaussian, lorentz
      integer rx2chn

*  Get line boundaries

      wstart=rx2chn(sdata,wavdim,left(line))
      m=rx2chn(sdata,wavdim,right(line))-wstart+1

* Display data if required

      if(pltdat) then
        call disp_window2(left(line),right(line),sdata(wstart),
     :                    sdens(wstart),m,' ',.true.)
      end if
      highres = pltdat.or.(deccntr(FIT_NCMP).eq.1)
      if(highres) then
        resolution = 5
      else
        resolution = 1
      end if

*  Get virtual memory:
*     FVALPTR   M*DECCNTR(FIT_NCMP)*RESOLUTION (r) (deccntr(FIT_NCMP) > 1 only)
*     TOTPTR    M*RESOLUTION (r) (deccntr(FIT_NCMP) > 1 only)
*
*     VBASE     M*RESOLUTION (r) (base)
*     HDATPTR   M*RESOLUTION (r) (highres only)
*
*     ADATA     M*RESOLUTION (d) (for getting base)
*     PTR0      Workspace for FIT_GLBASE (d):
*                       BACK        ELEMENTS (double precision)
*                        2          WAVDIM*2+M
*                        3          WAVDIM*6+460
*
*  Bug fixed and workspace made to overlap a lot more, TNW 9/11/90

* BASEWK includes PTR0 and ADATA

      if(deccntr(BACK_MODEL).eq.2)then
        basewk = 2*wavdim+m*resolution
      else if(deccntr(BACK_MODEL).eq.3) then
        basewk = 6*wavdim+460
      else
        basewk = 0
      end if

* Now we can actually get the VM!
      call dsa_get_work_array(m*resolution,'double',adata,slot,status)
      if (highres) call dsa_get_work_array(m*resolution,'float',hdatptr,
     :                                     slot2,status)
      call dsa_get_work_array(m*resolution,'float',vbase,slot3,status)
      if(deccntr(FIT_NCMP).gt.1) then
         call dsa_get_work_array(m*resolution*deccntr(FIT_NCMP),'float',
     :                           fvalptr,slot4,status)
         call dsa_get_work_array(m*resolution,'float',totptr,slot5,
     :                           status)
      end if
      if (basewk.ne.0) call dsa_get_work_array(basewk,'double',
     :                                         ptr0,slot6,status)

      if(status.ne.SAI__OK) return

* Sort out base

      if(highres) then
        call fill_dat(sdata(wstart),m,%VAL(CNF_PVAL(hdatptr)))
        status = cnv_fmtcnv('real','double',%VAL(CNF_PVAL(hdatptr)),
     :                      %VAL(CNF_PVAL(adata)),m*resolution,nbad)
        call fit_glbase(xsect,nwindow,sdata,%VAL(CNF_PVAL(hdatptr)),
     :                  deccntr,1,m*resolution,vbase,.false.,
     :                  %VAL(CNF_PVAL(adata)),ptr0,status)
      else
        status = cnv_fmtcnv('real','double',sdata(wstart),
     :                      %VAL(CNF_PVAL(adata)),m,nbad)
        call fit_glbase(xsect,nwindow,sdata,sdata(wstart),deccntr,1,
     :                  m,vbase,.false.,%VAL(CNF_PVAL(adata)),ptr0,
     :                  status)
      end if
      if(deccntr(FIT_NCMP).eq.1) then

* Single component fit

        call plot_fit(fitpar,deccntr(FIT_MODEL),m,%VAL(CNF_PVAL(vbase)))

      else if(deccntr(FIT_NCMP).gt.1) then

        if(highres) then
          if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
            call comp_plot(fitpar,deccntr(FIT_NCMP),m*resolution,
     :                     %VAL(CNF_PVAL(fvalptr)),
     :                     %VAL(CNF_PVAL(hdatptr)),yrange,gaussian)

          else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
            call comp_plot(fitpar,deccntr(FIT_NCMP),m*resolution,
     :                     %VAL(CNF_PVAL(fvalptr)),
     :                     %VAL(CNF_PVAL(hdatptr)),yrange,lorentz)
          end if
          call multi_plot(fitpar,deccntr(FIT_NCMP),m*resolution,
     :                    %VAL(CNF_PVAL(fvalptr)),
     :                    %VAL(CNF_PVAL(totptr)),
     :                    %VAL(CNF_PVAL(hdatptr)),deccntr(BACK_MODEL),
     :                    %VAL(CNF_PVAL(vbase)),yrange)
        else
          if(deccntr(FIT_MODEL).eq.GAUSSIAN_MODEL) then
            call comp_plot(fitpar,deccntr(FIT_NCMP),m,
     :                     %VAL(CNF_PVAL(fvalptr)),sdata(wstart),yrange,
     :                     gaussian)

          else if(deccntr(FIT_MODEL).eq.LORENTZ_MODEL) then
            call comp_plot(fitpar,deccntr(FIT_NCMP),m,
     :                     %VAL(CNF_PVAL(fvalptr)),sdata(wstart),yrange,
     :                     lorentz)
          end if
          call multi_plot(fitpar,deccntr(FIT_NCMP),m,
     :                    %VAL(CNF_PVAL(fvalptr)),
     :                    %VAL(CNF_PVAL(totptr)),sdata(wstart),
     :                    deccntr(BACK_MODEL),%VAL(CNF_PVAL(vbase)),
     :                    yrange)
        end if
      end if

      if (basewk.ne.0) call dsa_free_workspace(slot6,status)
      if(deccntr(FIT_NCMP).gt.1) then
         call dsa_free_workspace(slot5,status)
         call dsa_free_workspace(slot4,status)
      end if
      call dsa_free_workspace(slot3,status)
      if (highres) call dsa_free_workspace(slot2,status)
      call dsa_free_workspace(slot,status)
      end
