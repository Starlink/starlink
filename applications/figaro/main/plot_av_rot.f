      subroutine plot_av_rot(results,resvar,fitsta,wavelength,midpos,
     :      vpos,e_vpos,xsect_err,vcorr,nagerr,fxsect,lxsect,ngauss,
     :      vpos1,e_vpos1,ifsoft)
*+
* Name:
*    PLOT_AV_ROT

* Invocation:
*    CALL PLOT_AV_ROT(RESULTS,RESVAR,FITSTA,WAVELENGTH,MIDPOS,
*           VPOS,E_VPOS,XSECT_ERR,VCORR,NAGERR,FXSECT,LXSECT,NGAUSS,
*           VPOS1,E_VPOS1,IFSOFT)

* Purpose:
*   Plot average velocities

* Description:
*   To plot the average velocity against cross-section for as many of
*   the lines as possible in a LONGSLIT results cube.
*
* Arguments:
*    RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results array
*    RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results array variance
*    FITSTA(NCNTRL,NYP,NXP) = INTEGER ARRAY (Given)
*        Fit status
*    WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given)
*        Wavelengths of lines
*    MIDPOS(NXP*MGAUSS) = REAL ARRAY (Given)
*        Mid x-sect of fit
*    VPOS(NXP*MGAUSS) = REAL ARRAY (Given)
*        Velocity position of fit
*    E_VPOS(NXP*MGAUSS) = REAL ARRAY (Given)
*        Error on velocity position of fit
*    XSECT_ERR(NXP*MGAUSS) = REAL ARRAY (Given)
*        error on centre in x-sect direction
*    VCORR = REAL (Given)
*        Velocity correct (e.g. to LSR)
*    NAGERR = LOGICAL (Given)
*        If fits with Nag errors to be included
*    IFSOFT = LOGICAL (Given)
*        If plot in soft-copy
*    FXSECT(LINE_COUNT) = INTEGER ARRAY (Workspace)
*        First x-sections of fits
*    LXSECT(LINE_COUNT) = INTEGER ARRAY (Workspace)
*        Last      "       "   "
*    NGAUSS(LINE_COUNT) = INTEGER ARRAY (Workspace)
*        Number of components for fits
*    VPOS1(LINE_COUNT,MGAUSS) = REAL ARRAY (Workspace)
*        Velocities for lines
*    E_VPOS1(LINE_COUNT,MGAUSS) = REAL ARRAY (Workspace)
*        Errors on VPOS1
* In ARC_DIMS = CHARACTERommon (Workspace)
*
*    MGAUSS = INTEGER (Workspace)
*        Maximum number of Gaussians
*
* Authors:
*   T.N.Wilkins Manchester until 1/89, then Cambridge until 9/92

* History:
*   TNW: Original version
*   TNW: 9/11/87 Revised to allow max_gauss to easily be changed
*   TNW:  21/7/89 Use of get_parnum, more arrays obtained as workspace
*       as arguments.
*   TNW: 25/3/91 Check for new fits by checking for first xsect equal to
*        current xsect
*   TNW: 15/4/91 Changed to use ARCDIMS
*   TNW: 28/5/91 New fit status decoding
*-
      implicit none
      include 'arc_dims'
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)
      real wavelength(line_count)
      real vcorr
      logical nagerr
      real midpos(nxp*mgauss)
      real vpos(nxp*mgauss)
      real e_vpos(nxp*mgauss)
      real xsect_err(nxp*mgauss)
      real vpos1(line_count,mgauss)
      real e_vpos1(line_count,mgauss)
      integer fxsect(line_count),lxsect(line_count)
      integer ngauss(line_count)
      logical ifsoft

* Local parameter

      integer MAX_GAUSS
      parameter (MAX_GAUSS = 9)
* ----------------------------------------------------------------------
      real wavobs
      real waverr
      real velobs
      real velerr
      integer line
      integer xsect
      integer maxfxs
      integer minlxs
      logical newfits
      integer npts
      integer i
      logical sort

* for sorting etc.

      real value,dummy
      include 'status_inc'
      integer gauss
      integer get_parnum
      integer status
      integer len1
      integer num(MAX_GAUSS)
      integer peak,oldnum
      integer lincnt
      integer parpos
      real rhnwin,rcen
      character*80 chars
      character*1 number(MAX_GAUSS)
      character bss*2,bs*1
      data bss/'\\'/
      data number/'1','2','3','4','5','6','7','8','9'/
      bs = bss(1:1)
*
      len1 = 0
      call chr_putc('File : ',chars,len1)
      call chr_appnd(datafile,chars,len1)
      call chr_appnd('.DST',chars,len1)

      npts = 0
      do xsect = 1,nxp
        lincnt = 0
        call zero_int(num,MAX_GAUSS)
        call zero_int(ngauss,line_count)
        do line = 1,line_count

* Get and decode fit_status

          call decode_status(ncntrl,fitsta(1,line,xsect),deccntr)
          if((deccntr(FIT_STAT).eq.1).or.
     :                  ((deccntr(FIT_STAT).eq.2).and.nagerr)) then
            ngauss(line) = deccntr(FIT_NCMP)
            num(ngauss(line)) = num(ngauss(line))+1
          end if
        end do

* find most frequent number of components

        peak = 0
        oldnum = 0

        do i = 1, MAX_GAUSS
          if((num(i).ge.oldnum).and.(num(i).ge.1)) then
            oldnum = num(i)
            peak = i
          end if
        end do

        if(peak.gt.0) then
          minlxs = 999999
          maxfxs = -9
          newfits = .false.

          do line = 1,line_count
            if(ngauss(line).eq.peak) then

* Get data from file and convert to velocities

              parpos = get_parnum('Space1_pos')
              rcen = results(parpos,line,xsect)
              rhnwin = resvar(parpos,line,xsect)
              if(rhnwin.gt.0.0) rhnwin = sqrt(rhnwin)
              fxsect(line) = nint(rcen - rhnwin + 0.5)
              lxsect(line) = nint(rcen + rhnwin - 0.5)
              lxsect(line) = min(lxsect(line),nxp)
              if(fxsect(line).eq.xsect) newfits = .true.
              minlxs = min(lxsect(line),minlxs)
              maxfxs = max(fxsect(line),maxfxs)
              lincnt = lincnt+1

              do gauss = 1,ngauss(line)
                parpos = get_parnum('Centre_'//number(gauss))
                wavobs = results(parpos,line,xsect)
                waverr = sqrt(resvar(parpos,line,xsect))
                call wave2vel(wavelength(line),wavobs,waverr,0.0,0.0,
     :                  velobs,velerr,value,dummy)
                vpos1(lincnt,gauss) = velobs - vcorr
                e_vpos1(lincnt,gauss) = velerr
              end do

              sort = .true.

* Sort into ascending order

              do while(sort)
                sort = .false.

                do gauss = 2,ngauss(line)
                  if(vpos1(lincnt,gauss).lt.vpos1(lincnt,gauss-1)) then
                    value = vpos1(lincnt,gauss)
                    vpos1(lincnt,gauss) = vpos1(lincnt,gauss-1)
                    vpos1(lincnt,gauss-1) = value
                    value = e_vpos1(lincnt,gauss)
                    e_vpos1(lincnt,gauss) = e_vpos1(lincnt,gauss-1)
                    e_vpos1(lincnt,gauss-1) = value
                    sort = .true.
                  end if
                end do

              end do
            end if


*     over lines

          end do

*      If there are any new fits at this cross-section, then combine
*      results

          if(newfits) then
            do gauss = 1,peak
              npts = npts+1
              call dja_combine(lincnt,1,vpos1(1,gauss),e_vpos1(1,gauss)
     :                  ,vpos(npts),e_vpos(npts))
              midpos(npts) = real(minlxs+maxfxs)*0.5
              xsect_err(npts) = real(minlxs-maxfxs+1)*0.5
              if(e_vpos(npts).eq.0.0) npts = npts - 1
            end do
          end if

        end if


* over x-sects

      end do


* then plot data

      if(npts.ne.0) then
        call pldiagnosis(npts,midpos,vpos,xsect_err,e_vpos,
     :     'Average velocity centre v. Cross-section','Cross-section',
     :        'Average velocity (kms'//bs//'u-1'//bs//'d)',.true.
     :        ,chars(:len1),'xy',ifsoft,.true.)
      else
        call par_wruser('No suitable data in this file',status)
      end if

      end
