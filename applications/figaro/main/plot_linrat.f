      subroutine plot_linrat(results,midpos,lratio,rat_err,xsect_err,
     :              nagerr,line_name,max_gauss,centre,fluxc,fluxc_err,
     :              resvar,fitsta)
*+
* Name:
*    PLOT_LINRAT

* Invocation:
*    CALL PLOT_LINRAT(RESULTS,MIDPOS,LRATIO,RAT_ERR,XSECT_ERR,
*                   NAGERR,LINE_NAME,MAX_GAUSS,CENTRE,FLUXC,FLUXC_ERR,
*                   RESVAR,FITSTA)

* Purpose:
*  Plot line ratios

* Description:
*   To plot the a ratio of 2 lines as a function of cross-section, the
*  results are also output to a file DATAFILE.RAT. This is only suitable
*  for use interactively.
*
* Arguments (given):-
*  RESULTS(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results "cube"
*  RESVAR(MXPARS,NYP,NXP) = REAL ARRAY (Given)
*        Results "cube" variance
*  FITSTA(NCNTRL,NYP,NXP) = INTEGER ARRAY (Given)
*        Fit status
*  NAGERR = LOGICAL (Given)
*        If fits with NAG errors to be used
*  LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Names of lines
*  MAX_GAUSS = INTEGER (Given)
*        Maximum number of components allowed
* Work arrays (all real):-
*  MIDPOS(NXP*MAX_GAUSS)
*  LRATIO(NXP*MAX_GAUSS)
*  RAT_ERR(NXP*MAX_GAUSS)
*  XSECT_ERR(NXP*MAX_GAUSS)
*  CENTRE(MAX_GAUSS)
*  FLUXC(2,MAX_GAUSS)
*  FLUXC_ERR(2,MAX_GAUSS)
* Global variables:
*  MXPARS,NYP,NXP = INTEGER (Given)
*        dimensions of results "cube" (include file arc_dims)
*  LINE_COUNT = INTEGER (Given)
*        Number of lines (include file arc_dims)
*  DATAFILE = CHARACTER*(*) (Given)
*        Name of datafile file (include file arc_dims)

* Authors:
*   T.N.Wilkins Manchester until 1/89, then Cambridge until 9/92, then Durham

* History:
*   TNW: Original version August 1987
*   TNW: 24/1/89 Change to call of modflux, addition of max_gauss argument and
*        centre, fluxc, fluxc_err
*   TNW: 27/1/89 Change to call of modflux
*   TNW: 15/4/91 Changed to use ARCDIMS
*   TNW: 28/5/91 New fit status decoding
*   TNW: 1-8/7/91 New results structure
*   TNW: 26/2/93 Bug fix for working out errors of ratios
*   TNW: 23/5/94 Bug fix (not all uses of resvar had sqrt)
*-
      implicit none
      include 'SAE_PAR'
      include 'arc_dims'
      real results(mxpars,nyp,nxp)
      integer fitsta(ncntrl,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      logical nagerr
      include 'status_inc'
      include 'fit_coding_inc'
      character*10 line_name(line_count)
      integer max_gauss

* Mid x-sect of fit

      real midpos(nxp*max_gauss)

* Ratio of lines

      real lratio(nxp*max_gauss)

* Error on ratio

      real rat_err(nxp*max_gauss)

* error on centre in x-sect direction

      real xsect_err(nxp*max_gauss)
      real centre(max_gauss)
      real fluxc(2,max_gauss)
      real fluxc_err(2,max_gauss)
* ----------------------------------------------------------------------
      integer model
      integer line,liner(2)
      integer xsect
      real xcentre(2),hnwind(2)
      real max_first_xsect
      real min_last_xsect
      integer model1(2)
      integer npts,nref
      integer i,len1,len2
      integer gauss,ngauss(2)
      integer status
      integer lu
      logical new_fits
      logical sort,loop,add,limit
      logical qstat,par_qnum
      real value,maxrat
      real flux(2),flux_err(2)
      real frerr1, frerr2
      real fit_parms(6)
      real sg_error(6)
      real area,area_err
      character*80 chars,chars2
      integer get_parnum
      include 'PRM_PAR'
      character*1 number(0:9)
      integer NDICT
      parameter (NDICT = 3)
      character*70 dict(NDICT)
      integer narg,iopt
      character dumc
      data number /'0','1','2','3','4','5','6','7','8','9'/
      data limit,add/.false.,.false./
      data dict/
     :     ' ',
     :     'ADD  : Add together components of each line =',
     :     'OK   : Use current settings'/
*
      status = SAI__OK
      loop = .not.batch
      do while(loop)
         call chr_fill(' ',dict(1))
         len1 = 0
         call chr_putc('LIMIT %',dict(1),len1)
         if(limit) then
            call chr_putc('f(New max if wanted)',dict(1),len1)
         else
            call chr_putc('F"Maximum ratio"',dict(1),len1)
         endif

         call chr_putc(' : Set maximum ratio to plot = ',dict(1),len1)
         call chr_putl(limit,dict(1),len1)

         if(limit) then
            call chr_putc(' (',dict(1),len1)
            call chr_putr(maxrat,dict(1),len1)
            call chr_putc(')',dict(1),len1)
         endif
         len1 = 50
         call chr_putl(add,dict(2),len1)
         call qmenu('Plot Line Ratios',dict,NDICT,3,maxrat,dumc,iopt,
     :        narg,status)
         if(iopt.eq.1) then

*   If LIMIT is true, the user can reset the limit, but if no argument is given
*   limiting is switched off.

            if(.not.limit) then
               limit = .true.
            else if(narg.eq.0) then
               limit = .not.limit
            endif
         else if(iopt.eq.2) then
            add = .not.add
         else
            loop = .false.
         endif
      enddo
      loop = .true.
      do while(loop)
        call par_wruser('Lines are:-',status)
        do line = 1,line_count
          call chr_fill(' ',chars)
          len1 = 3
          call chr_puti(line,chars,len1)
          len1 = 8
          call chr_appnd(line_name(line),chars,len1)
          call par_wruser(chars(:len1),status)
        end do
        qstat = par_qnum('Enter first line number (0 to end)',0.0,
     :    real(line_count),1.0,.true.,' ',value)
        liner(1) = nint(value)
        if(liner(1).eq.0) then
          return
        else
          qstat = par_qnum('Enter second line number',1.0,
     :              real(line_count),1.0,.true.,' ',value)
          liner(2) = nint(value)
        end if
        npts = 0
        do xsect = 1,nxp
          new_fits = .false.
          do line = 1,2
            ngauss(line) = 0

* Get and decode fit status

            call decode_status(ncntrl,fitsta(1,liner(line),xsect),
     :                        deccntr)
            if((deccntr(FIT_STAT).eq.1).or.
     :                  ((deccntr(FIT_STAT).eq.2).and.nagerr)) then
              ngauss(line) = deccntr(FIT_NCMP)

              model1(line) = deccntr(FIT_MODEL)

*           Check if this fit is new

              nref = get_parnum('Space1_pos')
              xcentre(line) = results(nref,liner(line),xsect)
              hnwind(line) = resvar(nref,liner(line),xsect)
              if(hnwind(line).gt.0.0) hnwind(line) = sqrt(hnwind(line))
              if(nint(xcentre(line)).eq.xsect) then
                new_fits = .true.
              end if
            end if
          end do
          if(.not.new_fits)ngauss(1) = 0
          if((add.or.(ngauss(1).eq.ngauss(2))).and.
     :              (ngauss(1)*ngauss(2).gt.0)) then
            npts = npts+1
            do line = 1,2
              model = model1(line)

* Get data from file into fit_parms and sg_error

              midpos(npts) = xcentre(line)
              xsect_err(npts) = hnwind(line)
              flux(line) = 0.0
              flux_err(line) = 0.0
              do gauss = 1,ngauss(line)
                nref = get_parnum('Width_'//number(gauss))
                fit_parms(1) = results(nref,liner(line),xsect)
                sg_error(1) = sqrt(resvar(nref,liner(line),xsect))

                if(model.eq.SKEW_MODEL) then
                  nref = get_parnum('Skew_1')
                  fit_parms(5) = results(nref,liner(line),xsect)
                  sg_error(5) = sqrt(resvar(nref,liner(line),xsect))
                else if(model.eq.CAUCHY_MODEL) then
                  nref = get_parnum('Cauchy_1')
                  fit_parms(5) = results(nref,liner(line),xsect)
                  sg_error(5) = sqrt(resvar(nref,liner(line),xsect))
                end if

                fit_parms(1) = abs(fit_parms(1))

                nref = get_parnum('Height_'//number(gauss))
                fit_parms(2) = results(nref,liner(line),xsect)
                sg_error(2) = sqrt(resvar(nref,liner(line),xsect))

                nref = get_parnum('Centre_'//number(gauss))
                fit_parms(3) = results(nref,liner(line),xsect)
                sg_error(3) = sqrt(resvar(nref,liner(line),xsect))

                fit_parms(4) = 0.0
                sg_error(4) = 0.0

* Calculate flux in line

                call modflux(area,fit_parms,area_err,sg_error,model)
                fluxc(line,gauss) = area
                fluxc_err(line,gauss) = area_err
                centre(gauss) = fit_parms(3)
                flux(line) = flux(line)+area
                flux_err(line) = sqrt(flux_err(line)*flux_err(line)+
     :               area_err*area_err)
              end do

* Convert to flux/xsect

              flux(line) = flux(line)*0.5/hnwind(line)
              flux_err(line) = flux_err(line)*0.5/hnwind(line)
              sort = .not.add

* Sort into ascending order

              do while(sort)
                sort = .false.
                do gauss = 2,ngauss(line)
                  if(centre(gauss).lt.centre(gauss-1)) then
                    value = centre(gauss)
                    centre(gauss) = centre(gauss-1)
                    centre(gauss-1) = value
                    value = fluxc(line,gauss)
                    fluxc(line,gauss) = fluxc(line,gauss-1)
                    fluxc(line,gauss-1) = value
                    value = fluxc_err(line,gauss)
                    fluxc_err(line,gauss) = fluxc_err(line,gauss-1)
                    fluxc_err(line,gauss-1) = value
                    sort = .true.
                  end if
                end do
              end do

*       over lines

            end do

* Work out range of cross-sections that this applies to

            min_last_xsect = min((xcentre(1) + hnwind(1)),
     :                                (xcentre(2) + hnwind(2)))
            max_first_xsect = max((xcentre(1) - hnwind(1)),
     :                                (xcentre(2) - hnwind(2)))
            xsect_err(npts) = min_last_xsect - max_first_xsect
            midpos(npts) = max_first_xsect + xsect_err(npts) * 0.5
            xsect_err(npts) = xsect_err(npts) * 0.5

* Get ratios

            if(add) then

* Give ratios for whole line

              lratio(npts) = flux(1)/flux(2)
              frerr1 = flux_err(1)/flux(1)
              frerr2 = flux_err(2)/flux(2)
              rat_err(npts) = lratio(npts) *
     :             sqrt(frerr1 * frerr1 + frerr2 * frerr2)
            else

* Add each component separately

              do i = 1,ngauss(1)
                nref = npts - 1 + i
                midpos(nref) = midpos(npts)
                xsect_err(nref) = xsect_err(npts)
                lratio(npts) = fluxc(1,i)/fluxc(2,i)
                frerr1 = flux_err(1)/flux(1)
                frerr2 = flux_err(2)/flux(2)
                rat_err(npts) = lratio(npts) *
     :               sqrt(frerr1 * frerr1 + frerr2 * frerr2)
              end do
              npts = nref
            end if
          end if

*   over x-sects

        end do

* If data limited, then use data only up to limit requested.

        if(limit) then
          nref = 0
          do i = 1, npts
            value = lratio(i)
            if(value.le.maxrat) then
              nref = nref + 1
              lratio(nref) = value
              rat_err(nref) = rat_err(i)
              midpos(nref) = midpos(i)
              xsect_err(nref) = xsect_err(i)
            end if
          end do
          npts = nref
        end if

*   then plot data

        if(npts.ne.0) then

* Prepare title

          len1 = 0
          call chr_fill(' ',chars)
          call chr_putc('Ratio ',chars,len1)
          call chr_appnd(line_name(liner(1)),chars,len1)
          call chr_putc(' / ',chars,len1)
          call chr_appnd(line_name(liner(2)),chars,len1)
          call chr_putc(' v. Cross-section',chars,len1)

* Write data to a file

          call dsa_open_text_file(datafile,'.rat','new',.true.,lu,
     :              chars2,status)
          write(lu,*)chars(:len1)
          do i = 1, npts
            write(lu,*)lratio(i),rat_err(i),midpos(i),xsect_err(i)
          end do
          close(lu)
          call dsa_free_lu(lu,status)

          len2 = 0
          call chr_putc('File: ',chars2,len2)
          call chr_appnd(datafile,chars2,len2)
          call chr_putc('.DST',chars2,len2)

          call pldiagnosis(npts,midpos,lratio,xsect_err,rat_err,
     :       chars(:len1),'Cross-section','Ratio',.true.,chars2(:len2),
     :       'xy',.false.,.false.)
        else
          call par_wruser('No suitable data in this file',status)
        end if
      end do
      end
