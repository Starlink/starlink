      subroutine plotall(results,resvar,fitsta,line_name,wavelength,
     :      midpos,vpos,vposerr,xsecterr,vcorr,ifsoft,nagerr,perlin)
*+
* Name:
*    PLOTALL

* Invocation:
*    CALL PLOTALL(RESULTS,RESVAR,FITSTA,LINE_NAME,WAVELENGTH,
*           MIDPOS,VPOS,VPOSERR,XSECTERR,VCORR,IFSOFT,NAGERR,PERLIN)

* Purpose:
*   To plot the velocity in all line across a longslit spectrum.

* Description:
*   To plot the velocity in all line across a longslit spectrum.

* Arguments:-
*   RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results cube
*   RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results cube variance
*   FITSTA(NCNTRL,NYP,NXP) = INTEGER ARRAY (Given)
*        Fit status
*   LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Line names
*   WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given)
*        Line wavelengths
*   VCORR = REAL (Given)
*        correction for Earths motion (subtracted
*                                 from observed velocity)
*   IFSOFT = LOGICAL (Given)
*        If plot is in softcopy
*   NAGERR = LOGICAL (Given)
*        If fits with NAG errors to be shown
*   MIDPOS(*) = REAL ARRAY (Workspace)
*        Mid-position of fit (in cross-section)
*   XSECTERR(*) = REAL ARRAY (Workspace)
*        Extent of fit (in cross-sections)
*   VPOS(*) = REAL ARRAY (Workspace)
*        Velocity of centre (or width)
*   VPOSERR(*) = REAL ARRAY (Workspace)
*        Error on above
*   PERLIN(*) = INTEGER ARRAY (Workspace)
*        Number per line
* Global variables:
*  MXPARS,NYP,NXP           (i) : dimensions of results "cube" (include file arc_dims)
*  LINE_COUNT               (i) : Number of lines (include file arc_dims)
*  DATAFILE             (c*(*)) : Name of datafile file (include file arc_dims)

* Notes:
*     These work arrays should have enough space to fit every separate
*     component of every fit, for a given line. The recommended size for
*     each is the maximum possible number of components multiplied by
*     the number of cross-sections multiplied by the number of lines.
*
* Authors:
*   TNW: T.N.Wilkins Cambridge

* History:
*   TNW: 3-OCT-1991 Based on PLOTVEL
*-
      implicit none
      include 'arc_dims'
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)
      character*10 line_name(line_count)
      real wavelength(line_count)
      real vcorr
      integer perlin(*)
      logical ifsoft
      logical nagerr
* ----------------------------------------------------------------------
      real wave_obs
      real wave_err
      real width_obs
      real width_err
      real vel_obs
      real vel_err
      real velwid
      real evelwid
      real rhnwin
      integer line,ppos
      include 'PRM_PAR'
      integer xsect
      real mid_xsect
      integer cstat
      integer npts

* Mid Cross-section of fit

      real midpos(*)

* Velocity position of fit

      real vpos(*)

* Error on velocity position of fit

      real vposerr(*)

* error on centre in Cross-section direction

      real xsecterr(*)
      integer gauss
      integer status
      integer itmp,itmp2
      integer get_parnum
      character*30 chars,chars2
      integer tnw_cputr,tline
      character*1 number(9)
      logical par_quest,qstat
      character bss*2,bs*1
      include 'status_inc'
      data number/'1','2','3','4','5','6','7','8','9'/
      data bss/'\\'/
      bs = bss(1:1)

      call zero_int(perlin,line_count)

*
      npts = 0

      do line = 1, line_count
        do xsect = 1,nxp

* Check if fit different from previous Cross-section

          ppos = get_parnum('Space1_pos')
          mid_xsect = results(ppos,line,xsect)
          rhnwin = resvar(ppos,line,xsect)
          if(mid_xsect.ne.VAL__BADR) then
            rhnwin = sqrt(rhnwin)
            rhnwin = min(rhnwin,(real(nxp)-mid_xsect))
          else
            mid_xsect = -1
          end if

*     new fit (not already output)

          if(nint(mid_xsect).eq.xsect) then

* Get and decode fit_status

            call decode_status(ncntrl,fitsta(1,line,xsect),deccntr)
            cstat = deccntr(FIT_STAT)
            if((cstat.eq.1).or.((cstat.eq.2).and.nagerr)) then

* Get data from file and convert to velocities if required.

              do gauss = 1, deccntr(FIT_NCMP)
                npts = npts+1
                perlin(line) = perlin(line) + 1

                itmp = get_parnum('Centre_'//number(gauss))
                wave_obs = results(itmp,line,xsect)
                wave_err = sqrt(resvar(itmp,line,xsect))

                itmp = get_parnum('Width_'//number(gauss))
                width_obs = results(itmp,line,xsect)
                width_err = sqrt(resvar(itmp,line,xsect))

*            Convert wavelengths to velocities

                if(wavelength(line).gt.0.0) then
                  call wave2vel(wavelength(line),wave_obs,wave_err,
     :                   width_obs,width_err,vel_obs,vel_err,velwid,
     :                   evelwid)
                  vpos(npts) = vel_obs - vcorr
                  vposerr(npts) = vel_err
                  midpos(npts) = mid_xsect
                  xsecterr(npts) = rhnwin
                end if
              end do
            end if
          end if
        end do
      end do

      if(npts.ne.0) then
        call chr_fill(' ',title)
        itmp = 0
        call chr_putc('All lines, file ',title,itmp)
        call chr_appnd(datafile,title,itmp)
        call pldiagnosis(npts,midpos,vpos,xsecterr,vposerr,
     :       'Line centre v. Cross-section',
     :       'Cross-section','Velocity (kms'//bs//'u-1'//bs//'d)',.true.
     :       ,title(:itmp),'xy',ifsoft,.true.)
        npts = 1
        tline = 0
        do line = 1, line_count
          if(perlin(line).gt.0) then
            tline = tline + 1
            call pgpoint(perlin(line),midpos(npts),vpos(npts),line+2)
            npts = npts + perlin(line)
          end if
        end do

*     Create key plot

        if(ifsoft) qstat = par_quest('Go to next plot',.true.)
        tline = max(tline,10)
        call pgenv(0.0,1.0,real(tline+1),-1.0,0,-1)
        tline = 0
        call pgtext(0.3,0.0,'K E Y')
        do line = 1, line_count
          if(perlin(line).gt.0) then
            tline = tline + 1
            itmp = 0
            call chr_fill(' ',chars)
            call chr_appnd(line_name(line),chars,itmp)
            call greek_letters(chars,chars2,itmp2,.false.)
            chars = chars2
            itmp = itmp2 + 1
            call chr_putc('(',chars,itmp)
            status = tnw_cputr('f7.2',wavelength(line),chars,itmp)
            call chr_putc(')',chars,itmp)
            call pgtext(0.4,real(tline),chars(:itmp))
            call pgpoint(1,0.2,real(tline),line+2)
            npts = npts + perlin(line)
          end if
        end do
      end if
      end
