      subroutine plotvel(results,resvar,fitsta,line_name,wavelength,
     :      midpos,vpos,vposerr,xsecterr,showvel,iflag,vcorr,ifsoft,
     :      nagerr,ifcomb,mark)
*+
* Name:
*    PLOTVEL

* Invocation:
*    CALL PLOTVEL(RESULTS,RESVAR,FITSTA,LINE_NAME,WAVELENGTH,
*           MIDPOS,VPOS,VPOSERR,XSECTERR,SHOWVEL,IFLAG,VCORR,IFSOFT,
*           NAGERR,IFCOMB,MARK)
* Purpose:
*   To plot the velocity in a line across a longslit spectrum.

* Description:
*   To plot the velocity in a line across a longslit spectrum.

* Arguments:
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
*   SHOWVEL = LOGICAL (Given)
*        If velocity plots to be given
*   IFLAG = INTEGER (Given)
*        0 for centre v. cross-section
*                                 1 for width v. cross-section
*   VCORR = REAL (Given)
*        correction for Earths motion (subtracted
*                                 from observed velocity)
*   IFSOFT = LOGICAL (Given)
*        If plot is in softcopy
*   NAGERR = LOGICAL (Given)
*        If fits with NAG errors to be shown
*   IFCOMB = LOGICAL (Given)
*        If called from COMB
*   MARK = INTEGER (Given)
*        How to mark points:
*                              0 - No mark-error bars indicate position
*                              1 - Use continuous scale
*                              2 - Use grading into 3 divisions
*   MIDPOS(*) = REAL ARRAY (Workspace)
*        Mid-position of fit (in cross-section)
*   XSECTERR(*) = REAL ARRAY (Workspace)
*        Extent of fit (in cross-sections)
*   VPOS(*) = REAL ARRAY (Workspace)
*        Velocity of centre (or width)
*   VPOSERR(*) = REAL ARRAY (Workspace)
*        Error on above
*
* Global variables:
*  MXPARS,NYP,NXP = INTEGER (Given)
*       dimensions of results "cube" (include file arc_dims)
*  LINE_COUNT  = INTEGER (Given)
*       Number of lines (include file arc_dims)
*  DATAFILE = CHARACTER*(*) (Given)
*       Name of datafile file (include file arc_dims)

* Notes:
*     These work arrays should have enough space to fit every separate
*     component of every fit, for a given line. The recommended size for
*     each is the maximum possible number of components multiplied by
*     the number of cross-sections.

* Authors:
*   T.N.Wilkins Manchester until 1/89, then Cambridge until 9/92

* History:
*   TNW: Original version
*   TNW: 29/11/88 To use getwork, also look-up tables removed
*   TNW: 20/3/89 IFCOMB argument added
*   TNW: 20/7/89 Use of get_parnum
*   TNW: 3/1/91 Less prompting in this routine
*   TNW: 25/3/91 Check for new fits by checking for first xsect equal to
*        current xsect
*   TNW: 15/4/91 Changed to use ARCDIMS
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)
      integer iflag
      character*10 line_name(line_count)
      real wavelength(line_count)
      real vcorr
      logical ifsoft
      logical nagerr
      logical ifcomb
      integer mark
* ----------------------------------------------------------------------
      logical par_quest
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
      integer xsect
      integer slot
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
      integer vmptr
      integer tnw_cputr
      character*30 label
      character*24 ylab
      character*80 chars
      logical showvel
      character*13 xaxlabel
      integer itmp
      integer get_parnum
      character bss*2,bs*1
      character*1 number(9)
      include 'status_inc'
      data number/'1','2','3','4','5','6','7','8','9'/
      data bss/'\\'/
      bs = bss(1:1)
      status = SAI__OK

      if(ifcomb) then
        xaxlabel = 'Channels'
      else
        xaxlabel = 'Cross-section'
      end if

*
      if(showvel)  then
        if(iflag.eq.0) then
          ylab = 'Velocity (kms'//bs//'u-1'//bs//'d)'
        else
          ylab = 'Width (kms'//bs//'u-1'//bs//'d)'
        end if
      else
        if(ifcomb) then
          if(iflag.eq.0) then
            ylab = 'Centre (pixels)'
          else
            ylab = 'Width (pixels)'
          end if
        else
          if(iflag.eq.0) then
            ylab = 'Wavelength ('//bs//'Angtroms)'
          else
            ylab = 'Width ('//bs//'Angstroms)'
          end if
        end if
      end if
      line = 1

* Set up labels for plot

      if(iflag.eq.0) then
        label = 'Line centre v. Cross-section'
      else if(iflag.eq.1) then
        label = 'Line width v. Cross-section'
      else
        label = '          v. Cross-section'
      end if
      do while(line.le.line_count)
        npts = 0
        write(chars,'(''Line number'',i3)')line
        call par_wruser(chars(:14),status)
        do xsect = 1,nxp

* Check if fit different from previous Cross-section

          ppos = get_parnum('Space1_pos')
          mid_xsect = results(ppos,line,xsect)

C         print444, ppos, line, xsect
C 444     format(5x, 'ppos, line, xsect: ', i5, i5, i5)

C         print445, resvar(ppos,line,xsect)
C 445     format(5x, 'resvar: ', 1pe15.6)

          rhnwin = resvar(ppos,line,xsect)

C         print446, rhnwin
C 446     format(5x, 'rhnwin: ', 1pe15.6)

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
C               print666, npts
C 666           format(5x, 'npts: ', i4 )

                itmp = get_parnum('Centre_'//number(gauss))
                wave_obs = results(itmp,line,xsect)
                wave_err = sqrt(resvar(itmp,line,xsect))

                itmp = get_parnum('Width_'//number(gauss))
                width_obs = results(itmp,line,xsect)
                width_err = sqrt(resvar(itmp,line,xsect))

*            Convert wavelengths to velocities

                if(showvel.and.(wavelength(line).gt.0.0)) then
                  call wave2vel(wavelength(line),wave_obs,wave_err,
     :                   width_obs,width_err,vel_obs,vel_err,velwid,
     :                   evelwid)
                  if(iflag.eq.0) then
                    vpos(npts) = vel_obs - vcorr
                    vposerr(npts) = vel_err
                  else
                    vpos(npts) = velwid
                    vposerr(npts) = evelwid
                  end if
                else
                  if(iflag.eq.0) then
                    vpos(npts) = wave_obs
                    vposerr(npts) = wave_err
                  else
                    vpos(npts) = width_obs
                    vposerr(npts) = width_err
                  end if
                end if
                midpos(npts) = mid_xsect
                xsecterr(npts) = rhnwin
              end do
            end if
          end if
        end do
        itmp = 0
        call chr_fill(' ',chars)
        call chr_appnd(line_name(line),chars,itmp)
        itmp = itmp + 1
        status = tnw_cputr('f7.2',wavelength(line),chars,itmp)
        itmp = itmp + 1
        call chr_putc('(',chars,itmp)
        call chr_appnd(datafile,chars,itmp)
        call chr_putc(')',chars,itmp)

*   then plot data

C       print667, npts, itmp, chars
C 667   format(5x, 'npts, itmp, chars: ', i5, i4 / a )

        if(npts.ne.0) then
C         print668, 'before  pldiagnosis'
C 668     format(5x, a)
          call pldiagnosis(npts,midpos,vpos,xsecterr,vposerr,label,
     :            xaxlabel,ylab,.true.,chars(:itmp),'xy',ifsoft,
     :            ((iflag.eq.0).and.showvel))
C         print669, mark
C 669     format(5x, 'mark: ', i5)

          if(mark.gt.0) then
C           print668, 'before getwork'
            call dsa_get_work_array(npts,'float',vmptr,slot,status)
            if(status.ne.SAI__OK) return

C           print668, 'before flux_mark'
            call flux_mark(results,resvar,fitsta,vpos,midpos,npts,
     :                     %VAL(CNF_PVAL(vmptr)),line,mark.eq.2)
C           print668, 'before dsa_free_workspace'
            call dsa_free_workspace(slot,status)
          end if
          if(ifsoft) then
            if(.not.par_quest('Plot for next line?',.true.)) line = 100
          end if
        else
          call par_wruser('No data for this line - will go onto next',
     :                    status)
        end if
        line = line+1
      end do
      end
