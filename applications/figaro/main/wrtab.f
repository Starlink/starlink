      subroutine wrtab(results,resvar,line_name,wavelength,vcorr,nagerr
     :            ,disper,fitsta,status)
*+
* Name:
*    WRTAB

* Invocation:
*    CALL WRTAB(RESULTS,RESVAR,LINE_NAME,WAVELENGTH,VCORR,NAGERR
*                 ,DISPER,FITSTA,STATUS)

* Purpose:
*    To output a table of results from LONGSLIT/FIBDISP.

* Description:
*    The fit status is decoded for each new fit, and the results are
*    located and written to the file in a format suitable for input
*    to programs (i.e. a well defined fixed format). This is the UNIX
*    version (the difference is in the format of the OPEN statement).

* Arguments:
*      RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results array
*      RESVAR(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given)
*        Results array variance
*      FITSTA(NCNTRL,NYP,NXP,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status
*      LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Line names
*      WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given)
*        Line rest wavelengths
*      VCORR = REAL (Given)
*        Correction for Earths motion (this is subtracted from the
*        observed velocities)
*      NAGERR = LOGICAL (Given)
*        If to print fits with NAG errors
*      DISPER = REAL (Given)
*        Dispersion (or 1.0 if data flux-calibrated)
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=okay
* Global variables:
*     arc_dims:
*      NXP,NYP,MXPARS,SPDIM2 = INTEGER (Given)
*        Dimensions of results block
*      LINE_COUNT = INTEGER (Given)
*        Number of lines
*      DATAFILE = CHARACTER*72 (Given)
*        Name of data file
*      D_XPTR = INTEGER (Given)
*        Start of X axis array
*
* Authors:
*    TNW: T.N.Wilkins, Cambridge
* History:
*    TNW: 14-DEC-1990 Original version
*    TNW: 8-MAR-1991 Bug fixed re flux error.
*    TNW: 28-MAY-1991 New DECODE_STATUS, etc.
*    TNW: 3-JUN-1991 Precautions to prevent numbers meeting when formatted
*         into string.
*-
      implicit none
      integer status
      include 'arc_dims'
      real results(mxpars,nyp,nxp,spdim2)
      real resvar(mxpars,nyp,nxp,spdim2)
      integer fitsta(ncntrl,nyp,nxp,spdim2)
      character*10 line_name(line_count)
      real wavelength(line_count)
      real vcorr
      integer lu,len1,chr_len,len2
      integer gauss,line
      real wave_obs,width
      real vel_obs,vel_w,flux,flux_err
      real wave_err,width_err,vel_err,vel_w_err
      real fit_parms(6)
      real fit_error(6)
      logical nagerr
      integer model,cstat
      real cauchy,cauchy_err,skew,skew_err,disper,base,base_err
      character*1 number(9)

*  POS/OPOS contain the start positions of the fits and the blockings

      integer ppos,get_parnum,pppos(4),k,ix,iy,pos(2)
      real poscen(2),poserr(2)
      character*150 chars
      include 'status_inc'
      include 'fit_coding_inc'
      include 'PRM_PAR'
      data number/'1','2','3','4','5','6','7','8','9'/

* Open text file

      len1 = 0
      call chr_appnd(datafile,chars,len1)
      call chr_putc('.tab',chars,len1)
      call dsa_get_lu(lu,status)
      open(unit=lu,file=chars(:len1),status='new')


      write(lu,1) datafile(:chr_len(datafile))
  1   format('* Line fits contained in file ',2a)
      call get_date(chars)
      write(lu,2) chars(:chr_len(chars))
  2   format('*',2x,'(print out made on ',a,')',/,
     :'* Format of output',/,
     :'* Comment strings, starting with "*" in column 1',/,
     :'* Data, starting with component  number in column 1, followed by
     : position (X start/end, Y start/end),',/
     :,'* then centre as wavelength, then velocity, then width ditto,
     : then height, flux, skew, Cauchy, Base',/,
     :'* Note that position, skew, Cauchy and base are only given for
     : component 1',/,
     :'* Errors, lines start "E", and have errors on data of line above,
     : for elements which have errors',/,'* If no valid data for item,
     : then a "*" is used instead')
      if(vcorr.ne.0.0) write(lu,3) vcorr
  3   format('*',2x,'Correction of ',f10.4,
     :    'km/s subtracted from centre velocities')

      call zero_int(pppos,4)
      pppos(1) = get_parnum('Space1_pos')
      if(spdim2.gt.1) then
        pppos(2) = get_parnum('Space2_pos')
      end if

* Loop over data

      do line = 1, line_count
        len1 = 0
        write(lu,'(/)')
        call chr_fill(' ',chars)
        call chr_putc('* Line ',chars,len1)
        call chr_appnd(line_name(line),chars,len1)
        len1 = len1 + 1
        call chr_putr(wavelength(line),chars,len1)
        write(lu,'(a)')chars(:len1)
        do iy = 1, spdim2
          do ix = 1, nxp

* Check if fit starts at this position (only way to check in 3-d case)

            call zero_int(pos,4)
            do k = 1, 2
              if(pppos(k).gt.0) then
                poscen(k) = results(pppos(k),line,ix,iy)
                if(poscen(k).ne.VAL__BADR) pos(k) = nint(poscen(k))
                poserr(k) = resvar(pppos(k),line,ix,iy)
                if(poserr(k).ne.VAL__BADR) then
                  poserr(k) = sqrt(poserr(k))
                end if
              end if
            end do
            if((pos(1).eq.ix).and.((pos(2).eq.iy).or.(spdim2.eq.1)))
     :                         then

*        We have a new fit. Get and decode fit status.

              call decode_status(ncntrl,fitsta(1,line,ix,iy),deccntr)
              cstat = deccntr(FIT_STAT)
              if(deccntr(FIT_MODEL).eq.0) cstat = 0

*           If we have a sucessful fit, or a NAG error if we are
*           interested in those, then proceed to output results

              if((cstat.eq.1).or.((cstat.eq.2).and.nagerr)) then

*           Write status and model/type of fit to file

                len1 = 0
                call chr_fill(' ',chars)
                call chr_putc('* ',chars,len1)
                call describe_fit(deccntr,chars(len1+1:))
                len1 = chr_len(chars)
                len2 = 0
                if(len1.gt.130) then
                  len2 = len1
                  len1 = 130
                  do while(chars(len1:len1).ne.' ')
                    len1 = len1 - 1
                  enddo
                endif
                write(lu,'(/,a)')chars(:len1)
                if(len2.ne.0) then
                  write(lu,'(''* '',a)')chars(len1+1:len2)
                endif
                write(lu,4)
  4             format('*Cmp Position       Centre',28x,'Fwhm',14x,
     :                  'Height',8x,'Flux',10x,'Skew',3x,'Cauchy',5x,
     :                  'Base')
                if(deccntr(BACK_MODEL).gt.0) then
                  ppos = get_parnum('Base')
                  base = results(ppos,line,ix,iy)
                  base_err = sqrt(resvar(ppos,line,ix,iy))
                end if

* For single component fit, if this is Cauchy
* or skew we need to get the results for these at this stage.
* N.B. Modflux doesn't use elements 3 and 4 of fit_parms and fit_error.

                model = deccntr(FIT_MODEL)
                if(deccntr(FIT_TYPE).eq.SINGLE) then

*             Single

*               skew

                  if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
                    ppos = get_parnum('Skew_1')
                    skew = results(ppos,line,ix,iy)
                    skew_err = sqrt(resvar(ppos,line,ix,iy))
                    fit_parms(5) = skew
                    fit_error(5) = skew_err

*            cauchy

                  else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then
                    ppos = get_parnum('Cauchy_1')
                    cauchy = results(ppos,line,ix,iy)
                    cauchy_err = sqrt(resvar(ppos,line,ix,iy))
                    fit_parms(5) = cauchy
                    fit_error(5) = cauchy_err
                  end if
                end if

*           Loop over components

                do gauss = 1, deccntr(FIT_NCMP)
                  ppos = get_parnum('Centre_'//number(gauss))
                  wave_obs = results(ppos,line,ix,iy)
                  wave_err = sqrt(resvar(ppos,line,ix,iy))
                  ppos = get_parnum('Width_'//number(gauss))
                  width = abs(results(ppos,line,ix,iy))
                  width_err = sqrt(resvar(ppos,line,ix,iy))
                  call wave2vel(wavelength(line),wave_obs,wave_err,
     :                        width,width_err,vel_obs,vel_err,vel_w,
     :                        vel_w_err)
                  fit_parms(1) = width/disper
                  ppos = get_parnum('Height_'//number(gauss))
                  fit_parms(2) = results(ppos,line,ix,iy)
                  fit_error(1) = width_err/disper
                  fit_error(2) = sqrt(resvar(ppos,line,ix,iy))
                  call modflux(flux,fit_parms,flux_err,fit_error,model)
                  vel_obs = vel_obs - vcorr

*            Fill output strings

                  call chr_fill(' ',chars)
                  len1 = 0

*            Output number of component
*            Note that we need to put something to indicate that an
*            item is absent, so we use an asterix

                  call chr_puti(gauss,chars,len1)
                  if(gauss.eq.1) then
                    len1 = 2
                    call chr_putr(poscen(1),chars,len1)
                    len1 = 9
                    call chr_putr(poscen(2),chars,len1)
                  else
                    len1 = 3
                    call chr_putc('*',chars,len1)
                    len1 = 10
                    call chr_putc('*',chars,len1)
                  end if
                  len1 = 16
                  call chr_putr(wave_obs,chars,len1)
                  len1 = max(len1+1,30)
                  call chr_putr(vel_obs,chars,len1)
                  len1 = max(len1+1,44)
                  call chr_putr(width,chars,len1)
                  len1 = max(len1+1,58)
                  call chr_putr(vel_w,chars,len1)
                  len1 = max(len1+1,72)
                  call chr_putr(fit_parms(2),chars,len1)
                  len1 = max(len1+1,86)
                  call chr_putr(flux,chars,len1)

*              Skew and Cauchy

                  len1 = max(len1+1,100)
                  if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
                    call chr_putr(skew,chars,len1)
                    len1 = len1 + 1
                    call chr_putc('*',chars,len1)
                  else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then
                    call chr_putc('*',chars,len1)
                    len1 = len1 + 1
                    call chr_putr(cauchy,chars,len1)
                  else
                    call chr_putc('*',chars,len1)
                    len1 = max(len1+1,106)
                    call chr_putc('*',chars,len1)
                  end if
                  len1 = max(len1+1,116)
                  if((gauss.eq.1).and.(deccntr(BACK_MODEL).gt.0)) then
                    call chr_putr(base,chars,len1)
                  else
                    call chr_putc('*',chars,len1)
                  end if
                  call wft(chars,lu,status)

*               Now errors

                  call chr_fill(' ',chars)
                  len1 = 0
                  call chr_putc('E',chars,len1)
                  if(gauss.eq.1) then
                    len1 = 2
                    call chr_putr(poserr(1),chars,len1)
                    len1 = 9
                    call chr_putr(poserr(2),chars,len1)
                  else
                    len1 = 3
                    call chr_putc('*',chars,len1)
                    len1 = 10
                    call chr_putc('*',chars,len1)
                  end if
                  len1 = max(len1+1,16)
                  call chr_putr(wave_err,chars,len1)
                  len1 = max(len1+1,30)
                  call chr_putr(vel_err,chars,len1)
                  len1 = max(len1+1,44)
                  call chr_putr(width_err,chars,len1)
                  len1 = max(len1+1,58)
                  call chr_putr(vel_w_err,chars,len1)
                  len1 = max(len1+1,72)
                  call chr_putr(fit_error(2),chars,len1)
                  len1 = max(len1+1,86)
                  call chr_putr(flux_err,chars,len1)

*             Skew and Cauchy errors

                  len1 = max(len1+1,100)
                  if(deccntr(FIT_MODEL).eq.SKEW_MODEL) then
                    call chr_putr(skew_err,chars,len1)
                    len1 = len1 + 1
                  else if(deccntr(FIT_MODEL).eq.CAUCHY_MODEL) then
                    call chr_putc('*',chars,len1)
                    len1 = len1 + 1
                    call chr_putr(cauchy_err,chars,len1)
                  else
                    call chr_putc('*',chars,len1)
                    len1 = max(len1+1,106)
                    call chr_putc('*',chars,len1)
                  end if
                  len1 = max(len1+1,116)
                  if((gauss.eq.1).and.(deccntr(BACK_MODEL).gt.0)) then
                    call chr_putr(base_err,chars,len1)
                  else
                    call chr_putc('*',chars,len1)
                  end if
                  call wft(chars,lu,status)
                end do

              end if
            end if
          end do
        end do
      end do
      call dsa_free_lu(lu,status)
      end
