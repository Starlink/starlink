      subroutine transfer(results,resvar,line_name,left,right,control,
     :             wavelength,status)
*+
* Name:
*    TRANSFER

* Invocation:
*    CALL TRANSFER(RESULTS,RESVAR,LINE_NAME,LEFT,RIGHT,CONTROL,
*                  WAVELENGTH,STATUS)

* Purpose:
*    To extract results from the data cube and repeat the fits on
*   another line.

* Description:
*    To extract results from the data cube and repeat the fits on
*   another line.
*
* Arguments:
*     LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Names of lines
*     LEFT(LINE_COUNT),RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        Tram arrays
*     CONTROL(NYP,NXP,SPDIM2) = INTEGER*2 ARRAY (Given)
*        Fit control array
*     WAVELENGTH(LINE_COUNT) = REAL ARRAY (Given)
*        Rest wavelengths of lines
*     RESVAR(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given and returned)
*        Results cube variance
*     RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given and returned)
*        Results cube
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=okay
*
* Global variables:
*     MXPARS,NYP,NXP,SPDIM2 = INTEGER (Given)
*        Dimensions of results "cube"
*     LINE_COUNT = INTEGER (Given)
*        Number of lines identified
*     WAVDIM = INTEGER (Given)
*        Number of channels in data
*     D_XPTR = INTEGER (Given)
*        "Pointer" to first axis array
*     D_SPTR = INTEGER (Given)
*        "Pointer" to main data array
*     D_MPTR = INTEGER (Given)
*        "Pointer" to mask array
* Notes:
*   The common block in opt_cmn is also used to pass arguments
*   to routines called by this routine.
*
* Subroutines referenced:
*    CHECK_MASKING       : Check whether fit masked out
*    CHECK_STATII        : Check whether more "complicated" fit here
*                          already
*    FIG_XTRACT          : Take slice through 2-d array in direction
*                          of 1st dimension
*    GETWORK             : Get work array
*    GETRES              : Get previous fit from results block
*    FIT_LINE            : Fit the line
*    STORE_RESULTS       : Store fit results in data block
*    UPDTMSK             : Update mask array
*    DSA_FREE_WORKSPACE  : Free workspace
*    PAR_WRUSER          : Write character string to user
*
* Authors:
*   T.N.Wilkins Manchester
*   ACD: A C Davenhall, Starlink, Edinburgh

* History:
*   TNW 29/11/88 Changed to use getwork
*   TNW 27/1/89 Look-up table removed
*   Changes to allow variable base and weighting, TNW/CAVAD 21/12/90
*   TNW Cambridge, 25/3/91 Check for new fits by checking for first
*                 xsect equal to current xsect
*    "      "     7/91 Changes for new results structure, it_plus1
*                      removed
*   ACD: 28/9/00 Remove character strings continued across continuation
*     lines.
*-
* ----------------------------------------------------------------------
      implicit none
      include 'SAE_PAR'
      integer status
      include 'PRM_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      include 'status_inc'
      real results(mxpars,nyp,nxp)
      real resvar(mxpars,nyp,nxp)
      character*10 line_name(line_count)
      real left(line_count),right(line_count)
      integer control(ncntrl,line_count,nxp)
      real wavelength(line_count)
      include 'opt_cmn'
      integer ersptr
      integer line
      integer xsect
      integer istarty
      integer iendy
      integer nnew,nfailed,ppos
      logical masked,fit
      real odensc,wratio
      integer get_parnum
      real fit_parms(MAXPARMS)
      real fit_error(MAXPARMS)
      integer nwindow,tline,d1,start,ltram
      integer pstat,fstatus,len1
      real rhnwin
      character*57 chars
      logical dummy
      real aic
      integer k1,gauss,rx2chn,gsptr,slot2,slot3

      nnew = 0
      nfailed = 0
      do line = 1, line_count

* Find line to transfer fits from, and perform fits as requested

        tline = -control(ncntrl,line,1)
        if((tline.gt.0).and.(tline.le.line_count)) then
          write(chars,
     :      '(''TRANSFERRING fits from line'',i3,'' to line'',i3)')
     :      tline,line
          call par_wruser(chars,pstat)

*       Get pixel limits of line

          start = rx2chn(%VAL(CNF_PVAL(d_xptr)),wavdim,left(line))
          ltram  = rx2chn(%VAL(CNF_PVAL(d_xptr)),wavdim,right(line))-
     :             start + 1
          wratio = wavelength(line)/wavelength(tline)
          do xsect=1,spdim1

* Check if fit different from previous x-sect

            ppos = get_parnum('Space1_pos')
            rhnwin = sqrt(resvar(ppos,line,xsect))
            nwindow = nint(rhnwin*2.0)
            istarty = nint(results(ppos,line,xsect)-rhnwin)

            if(istarty.eq.xsect) then

*        new fit (not already output)

              iendy=istarty-1+nwindow

* check if masked out


*         .. by definition to start with

              masked = .false.

              call check_masking(line,istarty,iendy,1,1,masked,
     :                           %VAL(CNF_PVAL(d_mptr)))

* if not masked out check to see if possible to make an improvement
* on the existing fit


*         fail safe

              fit = .false.
              call check_statii(line,fit,d1,control,istarty,iendy,1,1,
     :                          %VAL(CNF_PVAL(staptr)))

              if(fit.and.(.not.masked)) then
                call getres(results,tline,xsect,1,fit_parms,deccntr,
     :                      odensc,%VAL(CNF_PVAL(staptr)),fstatus)

*           If fit not ok to use, getres will have set ftype to 0

                if(deccntr(FIT_MODEL).ne.0) then

*            Extract data

                  len1 = 0
                  call chr_putc('Line ',chars,len1)
                  call chr_puti(line,chars,len1)
                  call chr_putc(' ',chars,len1)
                  call chr_appnd(line_name(line),chars,len1)
                  call chr_putc(' (',chars,len1)
                  call chr_putr(wavelength(line),chars,len1)
                  call chr_putc(')',chars,len1)
                  call par_wruser(chars(:len1),pstat)
                  len1 = 0
                  call chr_putc('Cross-section',chars,len1)
                  call encode_range(' ','s',istarty,iendy,chars,len1)
                  call par_wruser(chars(:len1),pstat)
                  call fig_xtract(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,
     :                            istarty,iendy,%VAL(CNF_PVAL(d_vsptr)))

*        Correct parameters to this line, rather than one at a
*        different wavelength (so radial velocities the same).


*             single

                  if(deccntr(FIT_NCMP).eq.1) then
                    fit_parms(4) =
     :                     (fit_parms(4) - wavelength(tline)) * wratio
     :                     + wavelength(line)
                    fit_parms(1) = fit_parms(1) * wratio
                  else
                    do gauss = 1, deccntr(FIT_NCMP)
                      k1 = gauss*3 - 1
                      fit_parms(k1) = fit_parms(k1) * wratio
                      k1 = k1 + 2
                      fit_parms(k1) = (fit_parms(k1) -
     :                   wavelength(tline)) * wratio + wavelength(line)
                    end do
                  end if

*           Copy errors array into 1-d array for use with optimisation
*           routines, assuming of course that the error array is
*           present.
*           Get workspace for fit parameters array (previous results) at
*           same time (if weighted fits used).

                  if(deccntr(FIT_WEIGH).eq.1) then
                    call dsa_get_work_array(4*deccntr(FIT_NCMP),'float',
     :                                      gsptr,slot2,status)
                    call dsa_get_work_array(wavdim,'float',ersptr,slot3,
     :                                      status)
                    if(status.ne.SAI__OK) return
                    call cop_2_1d_err(%VAL(CNF_PVAL(errptr)),istarty,
     :                                iendy,1,1,%VAL(CNF_PVAL(ersptr)))
                  else
                    call dsa_get_work_array(4*deccntr(FIT_NCMP),'float',
     :                                      gsptr,slot2,status)
                    if(status.ne.SAI__OK) return
                  end if

*              Perform the new fitting

                  deccntr(FIT_GUES) = deccntr(FIT_GUES) + 90

                  call fit_line(deccntr,%VAL(CNF_PVAL(d_xptr)),
     :                          %VAL(CNF_PVAL(d_vsptr)),start,ltram,
     :                          line,nwindow,istarty,1,odensc,
     :                          %VAL(CNF_PVAL(ersptr)),fit_parms,
     :                          fit_error,dummy,aic,status)

                  call opt_release(status)
                  if(deccntr(FIT_WEIGH).eq.1)
     :              call dsa_free_workspace(slot3,status)
                  call dsa_free_workspace(slot2,status)

                  nnew=nnew+1
                  call store_results(fit_parms,fit_error,nnew,nfailed,
     :                               line,istarty,iendy,deccntr,odensc,
     :                               results,resvar,
     :                               %VAL(CNF_PVAL(staptr)),
     :                               %VAL(CNF_PVAL(d_mptr)),1,1,
     :                               VAL__BADR)

*           fit present and ok

                end if
              end if

*       new fit

            end if

*     over xsects

          end do
        else
          write(chars,
     :'(''Control array has invalid value for TRANSFER for line'',i3)'
     :      ) line
          call par_wruser(chars,pstat)

*   if control points to another line

        end if

* over lines

      end do
      write(chars,
     :'(''Number of new fits='',i4,3x,''Number of failed fits='',i4)')
     :  nnew,nfailed
      call par_wruser(chars,pstat)

* Update itteration

      iteration = iteration + 1
      call updtmsk(%VAL(CNF_PVAL(staptr)),%VAL(CNF_PVAL(d_mptr)))
      end
