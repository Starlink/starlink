      subroutine plot_flux(results,resvar,fitsta,line_name,wavelength,
     :      midpos,flux,flux_err,xsect_err,dispersion,nagerr,ifsoft)
*+
* Name:
*    PLOT_FLUX

* Invocation:
*    CALL PLOT_FLUX(RESULTS,RESVAR,FITSTA,LINE_NAME,WAVELENGTH,
*           MIDPOS,FLUX,FLUX_ERR,XSECT_ERR,DISPERSION,NAGERR,IFSOFT)

* Purpose:
*  To produce a plot of flux v. xsect from the results cube

* Description:
*   The flux is plotted in units of the Z array times units of X array,
*   per cross-section.

* Arguments:
*    RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results cube
*    RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results cube variance
*    FITSTA(NCNTRL,NYP,NXP) = INTEGER ARRAY (Given)
*        Fit status
*    LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Line names
*    WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given)
*        Wavelengths of lines
*    DISPERSION = REAL (Given)
*        Dispersion of data
*    NAGERR = LOGICAL (Given)
*        If fits with NAG errors to be shown
*    IFSOFT = LOGICAL (Given)
*        If plots in softcopy
*    MIDPOS(NXP*5) = REAL ARRAY (Workspace)
*        Mid x-sect of fit
*    FLUX(NXP*5) = REAL ARRAY (Workspace)
*        Velocity position of fit
*    FLUX_ERR(NXP*5) = REAL ARRAY (Workspace)
*        Error on velocity position of fit
*    XSECT_ERR(NXP*5) = REAL ARRAY (Workspace)
*        error on centre in x-sect direction

* Authors:
*   T.N.Wilkins Manchester until 1/89, then Cambridge until 9/92

* History:
*   TNW: Original version
*   TNW: Altered to allow softcopy plots 17/11/87
*   TNW: PARAMS added as argument, calls to get_parnum 21/7/89
*   TNW: PARAMS no longer needed 15/9/89
*   TNW: 25/3/91 Check for new fits by checking for first xsect equal to
*        current xsect
*   TNW: 15/4/91 Changed to use ARCDIMS
*   TNW: 28/5/91 New fit type arrays
*-
      implicit none
      include 'arc_dims'
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)
      character*10 line_name(line_count)
      real wavelength(line_count)
      real dispersion
      logical nagerr
      logical ifsoft
      real midpos(nxp*5)
      real flux(nxp*5)
      real flux_err(nxp*5)
      real xsect_err(nxp*5)
* ----------------------------------------------------------------------
      integer line
      integer xsect
      integer get_parnum,ppos
      real mid_xsect,hnwind
      include 'status_inc'
      include 'PRM_PAR'
      integer npts
      real area,area_err
      include 'opt_cmn'
      integer gauss
      character*80 chars
      logical par_quest
      integer status
      integer len1

*

      line=1

      do while(line.le.line_count)

        npts = 0
        len1 = 0
        call chr_putc('Line number ',chars,len1)
        call chr_puti(line,chars,len1)
        call par_wruser(chars(:len1),status)

        do xsect=1,nxp

* Check if fit different from previous x-sect (not already included).
* This is done by only using a fit if the centre position is at the
* current x-section

          ppos = get_parnum('Space1_pos')
          mid_xsect = results(ppos,line,xsect)

          if(mid_xsect.eq.VAL__BADR) then
          else if(nint(mid_xsect).eq.xsect) then
            hnwind = sqrt(resvar(ppos,line,xsect))
            hnwind = min(hnwind,(real(nxp)-mid_xsect))

* Get and decode fit_status

            call decode_status(ncntrl,fitsta(1,line,xsect),deccntr)
            if((deccntr(FIT_STAT).eq.1).or.
     :            ((deccntr(FIT_STAT).eq.2).and.nagerr)) then

*         Get data from file into fit_parms and sg_error

              npts = npts + 1
              midpos(npts) = mid_xsect
              xsect_err(npts) = hnwind
              flux(npts) = 0.0
              flux_err(npts) = 0.0

              do gauss = 1, deccntr(FIT_NCMP)
                call get_flux(results,resvar,mxpars,nyp,nxp,area,
     :                dispersion,area_err,deccntr(FIT_MODEL),xsect,
     :                line,gauss)
                flux(npts) = flux(npts)+area
                flux_err(npts) = sqrt(flux_err(npts) * flux_err(npts)
     :                   + area_err*area_err)
              end do

*         Convert to flux/xsect

              flux(npts) = flux(npts)*0.5/hnwind
              flux_err(npts) = flux_err(npts)*0.5/hnwind
            end if
          end if
        end do

        len1 = 0
        call chr_fill(' ',chars)
        call chr_appnd(line_name(line),chars,len1)
        len1 = len1 + 1
        call chr_putr(wavelength(line),chars,len1)
        call chr_putc(' File : ',chars,len1)
        call chr_appnd(datafile,chars,len1)
        call chr_putc('.DST',chars,len1)

*  If more than 1 point then create plot, otherwise go onto next line

        if(npts.gt.0) then
          call pldiagnosis(npts,midpos,flux,xsect_err,flux_err,
     :       'Flux v. Cross-section','Cross-section','Flux',.true.,
     :        chars,'xy',ifsoft,.false.)

*     If plot in softcopy, then prompt before going onto next plot. If
*     user desires not to, then set line to greater than line_count, so
*     will leave loop.

          if(ifsoft) then
            if(.not.par_quest('Go onto next plot?',.true.)) then
              line = line_count + 1
            end if
          end if
        else
          call par_wruser('No data for this line',status)
          call par_wruser('- will go onto next',status)
        end if
        npts=0
        line=line+1

      end do

      end
