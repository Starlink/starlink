      subroutine redo_all_fits(results,resvar,line_name,left,right,
     :            status)
*+
* Name:
*    REDO_ALL_FITS

* Invocation:
*    CALL REDO_ALL_FITS(RESULTS,RESVAR,LINE_NAME,LEFT,RIGHT,
*                 STATUS)

*
* Purpose:
*  To extract results from the data cube and repeat the fits.
*
* Description:
*  To extract results from the data cube and repeat the fits.
*
* Arguments:
*     LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Names of lines
*     LEFT(LINE_COUNT),RIGHT(LINE_COUNT) = REAL ARRAY (Given)
*        Tram arrays
*     RESULTS(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given and returned)
*        Results cube
*     RESVAR(MXPARS,NYP,NXP,SPDIM2) = REAL ARRAY (Given and returned)
*        Results cube variance
* Global variables:
*     MXPARS,NYP,NXP,SPDIM2 = INTEGER (Given)
*        Dimensions of results "cube" (include file arc_dims)
*     LINE_COUNT = INTEGER (Given)
*        Number of lines identified (include file arc_dims)
*     WAVDIM = INTEGER (Given)
*        Number of channels in data (include file arc_dims)
*     D_XPTR = INTEGER (Given)
*        "Pointer" to first axis array (include file arc_dims)
*     D_SPTR = INTEGER (Given)
*        "Pointer" to main data array (include file arc_dims)
*     D_MPTR = INTEGER (Given)
*        "Pointer" to mask array (include file arc_dims)
* Note:
*   The common block in opt_cmn is also used to pass arguments
*   to routines called by this routine and opt_commands is also
*   referenced.
*
* Author:
*   T.N.Wilkins Manchester
* History:
*   Changes to allow variable base and weighting, TNW/CAVAD 21/12/90
*   TNW Cambridge, 25/3/91 Check for new fits by checking for first xsect
*     equal to current xsect
*-
      implicit none
      include 'SAE_PAR'
      integer status
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
      real results(mxpars,nyp,nxp,spdim2)
      real resvar(mxpars,nyp,nxp,spdim2)
      character*10 line_name(line_count)
      real left(line_count),right(line_count)
      include 'opt_cmn'
*
* ----------------------------------------------------------------------
      integer line
      integer xsect,xpos
      integer ersptr
      integer istarty
      integer iendy
      integer get_parnum,ppos
      integer nnew,nfailed
      real odensc
      real fitpar(MAXPARMS)
      real fiterr(MAXPARMS)
      integer pstat
      integer nwindow,ltram,rx2chn,start
      integer slot2,gsptr,len1
      real rhnwin
      logical dummy
      real aic
      character*52 chars
      include 'status_inc'
      include 'PRM_PAR'
      include 'DYNAMIC_MEMORY'

      nnew=0
      nfailed=0
      xpos = 1 ! we won't deal with 3-case as yet

      do line=1,line_count

*  Get pixel start and end of line

        start = rx2chn(dynamic_mem(d_xptr),wavdim,left(line))
        ltram  = rx2chn(dynamic_mem(d_xptr),wavdim,right(line))-
     :           start + 1
        do xsect=1,spdim1

*     Check if fit different from previous x-sect

          ppos = get_parnum('Space1_pos')
          rhnwin = resvar(ppos,line,xsect,xpos)
          if(rhnwin.ne.VAL__BADR) then
            rhnwin = sqrt(rhnwin)
            nwindow = nint(rhnwin*2.0)
            istarty = nint(results(ppos,line,xsect,xpos)-rhnwin)
          else
            istarty = -999
          end if

*     new fit (not already output)

          if(istarty.eq.xsect) then
            iendy=istarty-1+nwindow

*        Get previous results

            call getres(results,line,xsect,1,fitpar,deccntr,odensc,
     :                  %VAL( CNF_PVAL(staptr) ),status)

*        If fit not ok to use, getres will have set fit model to 0

            if(deccntr(fit_model).ne.0) then

*          Extract data

              write(chars,'(''Line '',i2,'', '',a)')line,line_name(line)
              call par_wruser(chars,pstat)
              len1 = 0
              call chr_putc('Cross-section',chars,len1)
              call encode_range(' ','s',istarty,iendy,chars,len1)
              call par_wruser(chars(:len1),pstat)
              call fig_xtract(dynamic_mem(d_sptr),wavdim,spdim1,
     :            istarty,iendy,dynamic_mem(d_vsptr))

*   Copy errors array into 1-d array for use with optimisation routines,
*   assuming of course that the error array is present.
*   Get workspace for fit parameters array (previous results) at same
*   time (if weighted fits used).

              if(deccntr(fit_weigh).gt.1) then
                call getwork(wavdim+deccntr(fit_ncmp)*4,'float',
     :                  ersptr,slot2,status)
                if(status.ne.SAI__OK) return
                gsptr = ersptr + wavdim*VAL__NBR
                call cop_2_1d_err(dynamic_mem(errptr),istarty,iendy,1,
     :                  1,dynamic_mem(ersptr))

              else


                call getwork(deccntr(fit_ncmp)*4,'float',gsptr,slot2,
     :                  status)
                if(status.ne.SAI__OK) return
              end if

*          Perform fitting

              deccntr(FIT_GUES) = deccntr(FIT_GUES) + 90

              call fit_line(deccntr,dynamic_mem(d_xptr),
     :                 dynamic_mem(d_vsptr),start,ltram,line,nwindow,
     :                 istarty,1,odensc,dynamic_mem(ersptr),fitpar,
     :                  fiterr,dummy,aic,status)

              call opt_release(status)
              call dsa_free_workspace(slot2,status)
              nnew=nnew+1

*          Store results

              call store_results(fitpar,fiterr,nnew,nfailed,line,
     :            istarty,iendy,deccntr,odensc,results,resvar,
     :            %VAL( CNF_PVAL(staptr) ),%VAL( CNF_PVAL(d_mptr) ),
     :            1,1,-1.0e38)

*       fit present and ok

            end if

*     new fit

          end if

*   over xsects

        end do

* over lines

      end do
      write(chars,
     :'(''Number of new fits='',i4,3x,''Number of failed fits='',i4)')
     : nnew,nfailed
      call par_wruser(chars,pstat)
      end
