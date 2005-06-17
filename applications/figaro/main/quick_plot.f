      subroutine quick_plot(results,resvar,fitsta,line_name,errfit,
     :                  cur_line,ifsoft,all,status)
*+
* Name:
*    QUICK_PLOT

* Invocation:
*    CALL QUICK_PLOT(RESULTS,RESVAR,FITSTA,LINE_NAME,ERRFIT,
*                       CUR_LINE,IFSOFT,ALL,STATUS)

* Purpose:
*    To extract and plot results from the data cube.

* Description:
*  This plots a montage of profiles, with the fits superimposed.
*
* Arguments:
*    RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results "cube"
*    RESVAR(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results "cube" variance
*    FITSTA(NCNTRL,NYP,SPDIM1,SPDIM2) = INTEGER ARRAY (Given)
*        Fit status
*    LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Names of lines
*    ERRFIT = LOGICAL (Given)
*        If to plot fits with Nag errors
*    CUR_LINE = INTEGER (Given)
*        Current line (starting point for
*                                  plotting)
*    IFSOFT = LOGICAL (Given)
*        If to plot in softcopy
*    ALL = LOGICAL (Given)
*        If to plot all lines
*    STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
* Global variables:
*    LINE_COUNT = INTEGER (Given)
*        Number of lines (include file arc_dims)
*    SPDIM1,SPDIM2,NYP,MXPARS = INTEGER (Given and returned)
*        Dimensions of results "cube" (include file arc_dims)
*    WAVDIM = INTEGER (Given)
*        Number of channels (include file arc_dims)
*    NIXS = INTEGER (Given)
*        Number of cross-sections (include file arc_dims)
*    D_SPTR = INTEGER (Given)
*        Pointer to intensity data array (2-D) (include file arc_dims)
*    D_VSPTR = INTEGER (Given)
*        Pointer to intensity work array (1-D) (include file arc_dims)
*    D_XPTR = INTEGER (Given)
*        Pointer to wavelength data array (include file arc_dims)
*    D_TLPTR = INTEGER (Given)
*        Pointer to left boundaries of lines (include file arc_dims)
*    D_TRPTR = INTEGER (Given)
*        Pointer to right boundaries of lines (include file arc_dims)
*
*    The common blocks opt_cmn and gr_inc are also used.
* Subroutines/functions called:
*    DECODE_STATUS               : Decode fit status
*    FIG_XTRACT = INTEGER (Given and returned)
*        Extract 1-d spectrum from 2-d
*    GR_SELCT                    : Select graphics device
*    CHR_LEN = INTEGER (Given and returned)
*        Get non-blank length of string
*    LINE_PLOT_SUB               : Plot profile with fit
*    PAR_QUEST           (PAR) = LOGICAL ARRAY (Given and returned)
*        Ask yes/no question & get reply
*    GR_SPEN = INTEGER (Given and returned)
*        Select graphics pen
*    PGMTEXT = INTEGER (Given and returned)
*        Write text onto plot
*    PGVPORT = INTEGER (Given and returned)
*        Set viewport

* History:
*   T.N.Wilkins Manchester 1988, revised 9/4/88 to plot all "bad" fits
*   if required, rather than just those with Nag errors.
*   PGPLOT version, TNW/Cambridge 3/90
*   Start plotting at top left (rather than bottom left) TNW 22/6/90
*   Change to 5 by 4 plots, defined by parameters, text size smaller
*   for hardcopy, also put file name on plots. TNW 10/8/90
*   For new fits just check that first_ix.eq.ix, TNW 26/3/91
*   Options to plot all lines, or just current line, TNW 22/4/91
*   Altered so can be run in batch, TNW, 8-9/5/91
*-
      implicit none
      include 'SAE_PAR'
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      include 'opt_cmn'
      integer status
      real results(mxpars,nyp,spdim1,spdim2)
      real resvar(mxpars,nyp,spdim1,spdim2)
      integer fitsta(ncntrl,nyp,spdim1,spdim2)
      character*10 line_name(line_count)
      integer cur_line
      logical errfit
      logical all,ifsoft
      include 'gr_inc'
* ----------------------------------------------------------------------
      include 'status_inc'
      integer line,len1
      integer ix,iy
      integer cstat,pstat
      integer first_ix
      integer last_ix,ppos
      real fit_parms(max_parms),odensc
      integer nwindx,icount
      integer nwindy,first_iy,last_iy
      integer get_parnum
      logical par_quest,qplt
      integer NXZON,NYZON
      parameter (NXZON = 5)
      parameter (NYZON = 4)
      real zones(NXZON*NYZON,4)
      real xinc,yinc
      real rhnwin,rcen
      integer i1,j1,chr_len,end_line,start_line

      qplt = batch

      if(.not.(ifsoft.or.batch)) then
        qplt = par_quest('Use quick plotting?',.true.)
      end if

* Do we want to plot all the lines, or just this one?

      if(all) then
        start_line = 1
        end_line = line_count
      else
        start_line = cur_line
        end_line = cur_line
      end if

      icount = 0
      xinc = 0.99/real(NXZON)
      yinc = 0.98/real(NYZON)
      do j1 = 1, NYZON
        do i1 = 1, NXZON
          icount = icount + 1
          zones(icount,1) = real(i1-1)*xinc + 0.03
          zones(icount,2) = real(i1)*xinc
          zones(icount,3) = 1.01 - real(j1)*yinc
          zones(icount,4) = 0.97 - real(j1-1)*yinc
        end do
      end do
      icount = 0

*  Loop over lines

      do line = start_line, end_line

*   Do we need to start with a fresh plot-we may want to prompt before
*   removing old one?

        if((line.eq.start_line).or.(icount.gt.0)) then
          if (ifsoft.and.(icount.gt.0)) then
            if(par_quest('End?',.false.)) goto 500
          end if
          call gr_selct(ifsoft,status)
          if(qplt) then
            call pgscf(1)
            call pgslw(1)
          end if
        end if
        icount = 0
        do ix = 1, spdim1
          do iy = 1, spdim2

* Check if fit different from previous x-sect

            ppos = get_parnum('Space1_pos')
            rcen = results(ppos,line,ix,iy)
            rhnwin = resvar(ppos,line,ix,iy)
            if(rcen.ne.VAL__BADR) then
              rhnwin = sqrt(rhnwin)
              first_ix = nint(rcen - rhnwin + 0.5)
              nwindx = nint(rhnwin*2.0)
            end if
            if(spdim2.gt.1) then
              ppos = get_parnum('Space2_pos')
              rcen = results(ppos,line,ix,iy)
              rhnwin = resvar(ppos,line,ix,iy)
              if(rcen.ne.VAL__BADR) then
                rhnwin = sqrt(rhnwin)
                first_iy = nint(rcen - rhnwin + 0.5)
                nwindy = nint(rhnwin*2.0)
              end if
            else
              first_iy = 1
              nwindy = 1
            endif

*     new fit (not already output)

            if((first_ix.eq.ix).and.(first_iy.eq.iy)) then
              last_ix = first_ix - 1 + nwindx
              last_iy = first_iy - 1 + nwindy

* Get and decode fit status

              call decode_status(ncntrl,fitsta(1,line,ix,iy),deccntr)
              cstat = deccntr(FIT_STAT)
              if((cstat.eq.1).or.((cstat.gt.1).and.errfit)) then
                len1 = 0
                call chr_fill(' ',title)
                call chr_appnd(line_name(line),title,len1)
                call encode_range(' ',' ',first_ix,last_ix,title
     :               ,len1)
                if(cstat.gt.1) then
                  call par_wruser('Failed fit: '//title(:len1),pstat)
                end if

*          Extract data

                call extr3(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,spdim2,
     :                     first_ix,last_ix,first_iy,last_iy,
     :                     %VAL(CNF_PVAL(d_vsptr)))

*          Find out number of components and put results into fit_parms

                pstat=0
                call getres(results,line,ix,iy,fit_parms,deccntr,
     :                      odensc,fitsta,pstat)
                densc = dble(odensc)

*           Set character height to 0.7 default (cannot be done at start
*           of routine because device might be closed and re-opened).

                call pgsch(0.7)
                icount = icount + 1
                call pgvport(zones(icount,1),zones(icount,2),
     :               zones(icount,3),zones(icount,4))

*            Plot profile with fit. first_ix and nwindx passed for
*            use with fitcont fits only.

                call line_plot_sub(fit_parms,%VAL(CNF_PVAL(d_xptr)),
     :               %VAL(CNF_PVAL(d_vsptr)),%VAL( CNF_PVAL(d_tlptr)),
     :               %VAL(CNF_PVAL(d_trptr)),line,wavdim,deccntr,
     :               .true.,first_ix,nwindx,max_parms,status)

                if(status.ne.SAI__OK) goto 500
                call gr_spen(1)
                call pgmtext('T',-1.5,0.5,0.5,title)
                if(icount.eq.NXZON*NYZON) then
                  if (ifsoft) then
                    if(par_quest('End?',.false.)) goto 500
                  end if
                  call gr_selct(ifsoft,status)
                  if(qplt) then
                    call pgscf(1)
                    call pgslw(1)
                  end if
                  icount = 0
                else if(icount.eq.1) then
                  call pgmtext('T',0.4,0.5,0.5,
     :                 datafile(:chr_len(datafile)))
                end if

*         fit a success or nag error

              end if

*     new fit

            end if

*   over position

          end do
        end do

* over lines

      end do
 500  continue

      if(ifsoft) then
        call pgsch(1.0)
      else
        call clgrap
      end if
      end
