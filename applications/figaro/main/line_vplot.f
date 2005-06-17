      subroutine line_vplot(fit_parms,sdens,left,right,line,ifsoft,
     :       vcorr,wavelength,work,xsect,nwindow,deccntr,status)
*+
* Name:
*    LINE_VPLOT

* Invocation:
*    CALL LINE_VPLOT(FIT_PARMS,SDENS,LEFT,RIGHT,LINE,IFSOFT,
*            VCORR,WAVELENGTH,WORK,XSECT,NWINDOW,DECCNTR,STATUS)

* Purpose:
*  Plot the line profile, fit, and residuals, with the x array in
*  units of km/s.

* Description:
*   This converts the X axis array to velocity, and also the widths and
*   centres of the fit, and sets up a call for LINE_PLOT.

* Parameters:-
*   FIT_PARMS(MPARMS) = REAL ARRAY (Given)
*        Fit parameters
*   SDENS(WAVDIM) = REAL ARRAY (Given)
*        Extracted spectrum
*   LINE = INTEGER (Given)
*        Number of line
*   LEFT(LINE) = REAL ARRAY (Given)
*        Tram lines
*   RIGHT(LINE) = REAL ARRAY (Given)
*          "     "
*   IFSOFT = LOGICAL (Given)
*        If fit is to be in soft rather than hardcopy
*   VCORR = REAL (Given)
*        Correction to apply to velocites
*   WAVELENGTH(LINE) = REAL ARRAY (Given)
*        Wavelengths of lines
*   XSECT = INTEGER (Given)
*        Starting cross-section (back=4 only)
*   NWINDOW = INTEGER (Given)
*        Number of cross-section in window (back=4 only)
*  Workspace:
*   WORK(WAVDIM)     (r)
* Global variables:
*   WAVDIM = INTEGER (Given)
*        Number of channels
*   D_XPTR = INTEGER (Given)
*        Pointer to X array data
*   MPARMS = INTEGER (Given)
*        Maximum number of fit parameters
*   XUNITS        (c* = INTEGER (Given)
*        X units
*   TITLE         (c* = INTEGER (Given)
*        Tile for plot
*   LEGEND(2)     (c* = INTEGER ARRAY (Given)
*        Legends (further headings)

* Authors:
*    TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92

* History:
*    Original version, TNW 8/4/87
*    Change to call, TNW 6/7/88, 27/10/88
*    ARC_DIMS used, TNW 12/7/91
*-
      implicit none
      include 'status_inc'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      integer line
      logical ifsoft
      real left(line),right(line)
      real fit_parms(mparms)
      real sdens(wavdim)
      integer xsect,nwindow,status
      real vcorr
      real wavelength(line)
      real work(wavdim)

* Local


* local values for left & right

      real lleft,lright
      integer i
      real C
      parameter (C=2.997925e5)
      integer k1,k3,j

* Create zones for plot

      call copr2r(wavdim,%VAL(CNF_PVAL(d_xptr)),work)

* If plot is to be in velocity, then alter arrays, e.t.c as required

      do i = 1, wavdim
        work(i) = ((work(i) - wavelength(line))*C/wavelength(line))
     :            - vcorr
      end do
      lleft = ((left(line) - wavelength(line))*C/wavelength(line))
     :        - vcorr
      lright = ((right(line) - wavelength(line))*C/wavelength(line))
     :         - vcorr
      do j = 1, deccntr(FIT_NCMP)
        k1 = j * 3 - 1
        k3 = k1 + 2
        fit_parms(k1) = fit_parms(k1)*c/wavelength(line)
        fit_parms(k3) = ((fit_parms(k3)-wavelength(line))*C
     :                      /wavelength(line)) - vcorr
      end do
      call line_plot(fit_parms,work,sdens,lleft,lright,1,deccntr,.true.,
     :               ifsoft,xsect,nwindow,status)
      end
