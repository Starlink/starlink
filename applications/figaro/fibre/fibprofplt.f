      subroutine fibprofplt(y,ix,iy,results,idata)
*+
* Name:
*    FIBPROFPLT

* Invocation:
*    CALL FIBPROFPLT(Y,IX,IY,RESULTS,IDATA)

* Purpose:
*  To plot a line profile with the fit superimposed.

* Description:
*  To plot a line profile with the fit superimposed.

* Arguments:
*     IX = INTEGER (Given)
*        X position
*     IY = INTEGER (Given)
*        Y position
*     RESULTS(MXPARS,NYP,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Results block
*     IDATA(WAVDIM,SPDIM1,SPDIM2) = REAL ARRAY (Given)
*        Data
*   In common arc_dims:
*     WAVDIM = INTEGER (Given)
*        Number of pixels in wavelength direction
*     XUNITS = CHARACTER*(*) (Given)
*        Units of wavelength array
*     D_WPTR = INTEGER (Given)
*        Pointer to wavelength of line
*     D_XPTR = INTEGER (Given)
*        Pointer to 1st axis array
*     LEGEND(2) = CHARACTER*60 ARRAY (Given)
*        Legends for plot
*     SPDIM1 = INTEGER (Given)
*        X (1st spatial) dimension of data
*     SPDIM2 = INTEGER (Given)
*        Y dimension of data
*     MXPARS = INTEGER (Given)
*        1st dimension of results block
*   Work array:
*     Y(WAVDIM) = REAL ARRAY (Given)
*        Intensity data
*
*   Subroutines/functions referenced:
C     CNF_PVAL       : Full pointer to dynamically allocated memory
*     COPY2WORK      : Copy data to work array
*     DECODE_STATUS  : Decode fit status element
*     RV_CORRECT     : Get correction to VLSR etc.
*     LINE_PLOT      : Plot profile with fit
*     LINE_VPLOT     : As line_plot but with velocity scale on X axis
*     DSA_GET_WORK_ARRAY : Get virtual memory
*     DSA_FREE_WORKSPACE : Free virtual memory
*     PAR_QUEST      : Get yes/no response from user
*
* Author:
*    T.N.Wilkins Manchester 7/7/88
* History:
*    TNW 14/11/88 To plot profile if no fit
*     "  1-8/7/91 Changes for new results structure
*     "  12/7/91 X axix array accessed via pointer from this routine,
*                tram arrays used.
*-
      implicit none
      include 'opt_cmn'
      include 'status_inc'
      include 'arc_dims'
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      real fit_parms(max_parms)
      real y(wavdim),vcorr,odensc
      integer vtype,status,w1ptr,ix,iy,tix,tiy
      integer i,j,slot,get_parnum,ppos1,ppos2
      logical par_quest,add
      character*30 dxunit
      character*43 chars
      real results(mxpars,1,spdim1,spdim2),idata(wavdim,spdim1,spdim2)
      real value
      include 'PRM_PAR'
      save vcorr,vtype
      character*3 vctype(2)
      character bss*2,bs*1
      data vctype/'HEL','LSR'/
      data bss/'\\'/
      bs = bss(1:1)

      status = SAI__OK
      add = .false.
      call par_qstr('Enter title for plot',' ',.true.,.false.,title)

      call getres(results,1,ix,iy,fit_parms,deccntr,odensc,
     :            %VAL(CNF_PVAL(staptr)),status)

      if((status.eq.SAI__OK).and.((deccntr(fit_stat).eq.1).or.
     :            (deccntr(fit_stat).eq.2))) then

* Get data to plot

        ppos1 = get_parnum('Space1_pos')
        ppos2 = get_parnum('Space2_pos')
        do j = 1, spdim2
          do i= 1, spdim1
            value = results(ppos1,1,i,j)
            if(value.ne.VAL__BADR) then
              tix = nint(value)
              value = results(ppos2,1,i,j)
              if(value.ne.VAL__BADR) then
                tiy = nint(value)
                if((tix.eq.ix).and.(tiy.eq.iy)) then
                  call copy2work(y,wavdim,idata(1,i,j),add)
                  add = .true.
                end if
              end if
            end if
          end do
        end do

        if(par_quest('Use velocity scale',.true.)) then

*   Plot with velocity scale on X-axis

          call dsa_get_work_array(wavdim,'float',w1ptr,slot,status)
          if(status.ne.SAI__OK) then
            return
          end if

*     Get correction to local standard of rest etc. velocity

          if(vcorr.ne.0.0) then
            write(chars,'(''Current velocity correction = '',
     :           e12.5)') vcorr
            call par_wruser(chars,status)
          end if
          if(par_quest('Evaluate velocity correction?',.true.)) then
            call rv_correct('cube',vcorr,vtype)
          end if
          dxunit = xunits
          if((vtype.eq.1).or.(vtype.eq.2)) then
             xunits = 'V'//bs//'d'//vctype(vtype)//bs//'u/km s'
     :            //bs//'u-1'//bs//'d'
          else
             xunits = 'km s'//bs//'u-1'//bs//'d'
          end if

*     Make plot

          call line_vplot(fit_parms,y,%VAL(CNF_PVAL(d_tlptr)),
     :                    %VAL(CNF_PVAL(d_trptr)),1,.false.,vcorr,
     :                    %VAL(CNF_PVAL(d_wptr)),%VAL(CNF_PVAL(w1ptr)),
     :                    0,0,deccntr,status)
          call dsa_free_workspace(slot,status)
          xunits = dxunit
        else

*   Plot with wavelength scale on X-axis

          call line_plot(fit_parms,%VAL(CNF_PVAL(d_xptr)),y,
     :                   %VAL(CNF_PVAL(d_tlptr)),
     :                   %VAL(CNF_PVAL(d_trptr)),1,deccntr,
     :                   .true.,.false.,0,0,status)
        end if
      else
        call gr_hard(status)
        call plot_spect(wavdim,%VAL(CNF_PVAL(d_xptr)),idata(1,ix,iy),
     :                  title,xunits,' ')
      end if
      end
