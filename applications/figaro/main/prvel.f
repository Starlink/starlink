      subroutine prvel(results,resvar,fitsta,line_name,wavelength,vcorr
     :     ,nagerr,disper,xs,ys,xds,hex)
*+
* Name:
*    PRVEL

* Invocation:
*    CALL PRVEL(RESULTS,RESVAR,FITSTA,LINE_NAME,WAVELENGTH,VCORR
*          ,NAGERR,DISPER,XS,YS,XDS,HEX)

* Purpose:
*   Write velocity information to text file

* Description:
*     To produce rotation curves from the data cube. The results are
*   output to a file.
*
*   (Nwindow is the number of x-sections the data was averaged over,
*   i.e. the data was averaged over first_xsect to
*   first_xsect-1 + nwindow).
*
* Arguments:
*    RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results cube
*    RESVAR(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results cube variance
*    FITSTA(NCNTRL,NYP,SPDIM1,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status
*    LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Names of lines
*    WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given)
*        Wavelengths of lines
*    WAVDIM = INTEGER (Given)
*        Number of channels in data
*    SDATA(WAVDIM) = REAL ARRAY (Given)
*        X data array
*    VCORR = REAL (Given)
*        Correction for Earths motion (this is
*                                    subtracted from the observed
*                                   velocities)
*    NAGERR = LOGICAL (Given)
*        If to show fits with NAG errors
*    DISPER = REAL (Given)
*        Dispersion (or 1.0 if data
*                                  flux-calibrated)
*  Used for 3-d data only:
*     XS(SPDIM1) = REAL ARRAY (Given)
*        First spatial axis
*     YS(SPDIM2) = REAL ARRAY (Given)
*        Second spatial axis
*     XDS(SPDIM2) = REAL ARRAY (Given)
*        Correction to first spatial axis with Y
*     HEX = LOGICAL (Given)
*        If XDS to be used
* Global variables:
*  MXPARS,NYP,NXP           (i) : dimensions of results "cube" (include file arc_dims)
*  LINE_COUNT               (i) : Number of lines (include file arc_dims)
*  DATAFILE             (c*(*)) : Name of datafile file (include file arc_dims)

* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, Cambridge until 9/92

* History:
*   TNW: Original version
*   TNW: 6/1/89 Bug fix, exponent had been missed off value of c
*   TNW: 24/1/89, Call of modflux changed
*   TNW: 27/1/89   "   "     "      "
*   TNW: 15/4/91 Changed to use ARCDIMS
*   TNW: 12/5/92 Changes for 3-d support
*-
      implicit none
      include 'SAE_PAR'
      include 'arc_dims'
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2),xs(*),ys(*),xds(*)
      integer fitsta(ncntrl,nyp,spdim1,spdim2)
      character*10 line_name(line_count)
      real wavelength(line_count)
      real vcorr
      logical nagerr,hex
* ----------------------------------------------------------------------
      integer line,ix,status,lu,iy
      logical first_of_line,ok
      integer first_xsect
      integer last_xsect
      integer pposx,pposy,get_parnum
      integer chr_len,len1,ixpos,iypos
      real disper,rcenx,rceny,px,py
      character*41 date,chars*80
      include 'PRM_PAR'
      include 'status_inc'
      include 'fit_coding_inc'
      real rhnwin

      status = SAI__OK

* Check if fits with NAG errors are to have their fit parameters output
* (any fit attempted will have its fit_status decoded and the relevant
* information output, even if the fit failed).

* by the way, date here is ignored

      call chr_fill(' ',chars)
      len1 = 0
      call chr_appnd(datafile,chars,len1)
      call chr_putc('.rot',chars,len1)
      call dsa_open_text_file(chars,' ','new',.true.,lu,date,
     :     status)

      write(lu,10) datafile(:chr_len(datafile))
 10   format(2x,'ROTATION CURVES',/,'LINE FITS CONTAINED IN FILE '
     :     ,2a)
      write(lu,11)
 11   format(2x,'Fits made by program LONGSLIT')
      call get_date(date)
      write(lu,12) date(:chr_len(date))
 12   format(2x,'(print out made on ',a,')')
      if(vcorr.ne.0.0) write(lu,16) vcorr
 16   format(2x,'Correction of ',f10.4,
     :     'km/s subtracted from centre velocities')
      write(lu,13)
 13   format(2x,'(Centres and widths in km/s, flux is total in line)')

* Loop over lines

      pposx = get_parnum('Space1_pos')
      if(spdim2.gt.1) then
        pposy = get_parnum('Space2_pos')
      else
        pposy = -1
      endif
      do line = 1, line_count
        first_of_line = .true.

*  ...and position

        do iy = 1, spdim2
          do ix = 1, spdim1

            ok = .false.

* Check if fit different from previous x-sect

            rcenx = results(pposx,line,ix,iy)
            if(rcenx.ne.VAL__BADR) then
              ixpos = nint(rcenx)
              if(ixpos.eq.ix) then
                if(pposy.gt.0) then
                  rceny = results(pposx,line,ix,iy)
                  iypos = nint(rceny)
                else
                  iypos = 1
                end if
                ok = iypos.eq.iy
              end if
            end if

* Get and decode fit_status, with meaningful output

            if(ok) then
              call decode_status(ncntrl,fitsta(1,line,ix,iy),deccntr)
              if(deccntr(FIT_MODEL).eq.0) deccntr(FIT_STAT) = 0
              if((deccntr(FIT_STAT).eq.1).or.
     :             ((deccntr(FIT_STAT).eq.2).and.nagerr)) then

* Output position and id of line

                if (first_of_line) then
                  write(lu,9)line_name(line),wavelength(line)
 9                format(/,a10,'(',f9.3,')')
                  first_of_line = .false.
                end if

                if(spdim2.gt.1) then
                  call morepts(results,resvar,ix,iy,line,lu)
                  px = xs(ix)
                  if(hex) px = px + xds(iy)
                  py = ys(iy)
                  write(lu,'(''Spatial position : '',f8.3,'','',f8.3)')
     :                 px,py
                else
                  rhnwin = resvar(pposx,line,ix,iy)
                  rhnwin = sqrt(rhnwin)
                  first_xsect = nint(rcenx-rhnwin)
                  last_xsect = first_xsect-1 + nint(rhnwin*2.0)
                endif

                call describe_fit(deccntr,chars)
                call wft(chars,lu,status)

*   Output fit results-first format header

                call chr_fill(' ',chars)
                len1 = 5
                if(spdim2.eq.1) then
                  call chr_putc('Position',chars,len1)
                  len1 = len1 + 2
                endif

*           double or multiple gaussians

                if(deccntr(FIT_NCMP).gt.1) then
                  call chr_putc('Component',chars,len1)
                  len1 = len1 + 6
                else
                  len1 = len1 + 15
                endif

                call chr_putc('Centre',chars,len1)
                len1 = len1 + 10
                call chr_putc('Fwhm',chars,len1)
                len1 = len1 + 10
                call chr_putc('Flux',chars,len1)

*         Skew

                if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then

                  len1 = len1 + 10
                  call chr_putc('Skew',chars,len1)

*         Cauchy

                else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then

                  len1 = len1 + 10
                  call chr_putc('Cauchy',chars,len1)

                end if
                call wft(chars(:len1),lu,status)

                call prvel_sub(results,resvar,line,ix,iy,deccntr,lu,
     :               first_xsect,last_xsect,disper,wavelength(line),
     :               vcorr,status)

*         fit a success or nag error if allowed

              end if

*     new fit

            end if

*   over position

          end do
        end do

* over lines

      end do
      call dsa_free_lu(lu,status)
      end
