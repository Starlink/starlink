      subroutine examprof(ref,ix,iy,npts,status)
*+
* Name:
*    EXAMPROF

* Invocation:
*    CALL EXAMPROF(REF,IX,IY,NPTS,STATUS)
*
* Purpose:
*  To examine a line profile.
*
* Description:
*  To examine a line profile.
*
* Arguments:
*     REF(SPDIM1,SPDIM2) = INTEGER*2 ARRAY (Given)
*        Reference array-gives profiles being used are set to 1
*     IX,IY = INTEGER (Given)
*        Position of profile (or a point used for it)
*     NPTS = INTEGER (Given)
*        Number of points co-added
*     STATUS = INTEGER (Given and returned)
*        Global status
* Global variables:
*     NW = INTEGER (Given)
*        Number of pixels
*     D_XPTR = INTEGER (Given)
*        "Pointer" to wavelength array
*     SPDIM1 = INTEGER (Given)
*        X (spatial) dimension of data
*     SPDIM2 = INTEGER (Given)
*        Y (spatial) dimension of data
*     D_SPTR = INTEGER (Given)
*        "Pointer" to the data array
*     SPDIM1 = INTEGER (Given)
*        X (spatial) dimension of data
*     SPDIM2 = INTEGER (Given)
*        Y (spatial) dimension of data
*     NZP = INTEGER (Given)
*        first dimension of results block
*     WAVELENGTH = REAL (Given)
*        Wavelength of line
*     XUNITS = CHARACTER*(*) (Given)
*        Units of wavelength array
*     ITERATION = INTEGER (Given)
*        Iteration number
*     NYP = INTEGER (Given)
*        Last dimension of results block (=1 at present)
*     D_RPTR = INTEGER (Returned)
*        "Pointer" to results block
*
* Subroutines/functions called:
*     ACCRES       : Access application-specific structure
C     CNF_PVAL     : Full pointer to dynamically allocated memory
*     FIT_LINE     : Perform optimisation of fit
*     ENCODE_CONTROL : Encode fit type into fit status element
*     FIBCHKFIT    : Check if fit more "advanced" than previous
*     CHECK_MASKING : Check if fit masked out
*     GR_SOFT      : Select soft-copy device
*     UPDTMSK      : Update mask array
*     FIBPROFPLT   : Plot profile with fits superimposed
*     FIBSTRES     : Store fit results
*     PLOT_SPECT   : Plot a 1-d spectrum
*     QMENU        : Get menu response from user
*     SET_FIT      : Set fit model/type etc. to use
*
*     DSA_FREE_WORKSPACE : Free virtual memory
*     DSA_GET_WORK_ARRAY : Get virtual memory
*
*     PAR_QUEST = LOGICAL (Returned)
*        Get yes/no response from user
*
* Authors:
*   TNW: T.N.Wilkins Manchester/Cambridge (from 2/89)
* History:
*   TNW 6/88 Original version
*   TNW 11/8/88 To use GET_SOFT, and to only replot when required.
*   TNW 14/11/88 Title to menu
*    "  1-8/7/91 Changes for new results structure
*    "  12/7/91 Change to call of fibprofplt
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'arc_dims'
      integer odensc
      integer*2 ref(spdim1,spdim2)
      character*39 dict(3)
      integer key,ix,iy,nfailed,nnew,slot
      integer nparms
      include 'opt_cmn'
      integer w1ptr,npts
      integer i,j,dumi
      real g_parms(MAX_PARMS),g_error(MAX_PARMS),aic
      logical loop,mstore,par_quest,masked,fit
      logical stored,plot,dummy1,dummy2
      include 'status_inc'
*
*  TRIAL is required by workaround for bug in Digital Fortran Compiler v4.1
*  Remove when fixed.
*
      logical trial

      data dict/
     :     'FIT  : Fit model to profile',
     :     'HARD : Produce hardcopy plot of profile',
     :     'EXIT : Exit this section of program'/

      status = SAI__OK
      plot = .true.
      loop = .true.
      stored = .false.

      nparms = MAX_PARMS
      do while(loop)
        if(plot) then
          call gr_soft(status)
          call gr_spen(1)

*   Plot profile

          call plot_spect(wavdim,%VAL(CNF_PVAL(d_xptr)),
     :         %VAL(CNF_PVAL(d_vsptr)),' ',xunits,' ')
        end if
        plot = .false.

*  Get menu reponse

        call qmenu('Profile Menu',dict,3,1,0.0,' ',key,dumi,status)

        if(status.ne.SAI__OK) then
          return
        else if(key.eq.1) then

*    Fit Gaussians, etc. to profile

          call set_fit_menu(dummy1,dummy2,deccntr,wavdim,gpscal
     :          ,prvfit,usepeak,bimtst,tyaic,curmcmp,prvpos,mgauss
     :          ,line_count,errpre,inherit,status)

*    Note that spdim1 maps to nYp in fit_line, and spdim2 to nXp! This
*    is due to "reverse" order of dimensions in fit_line etc.

          call fit_line(g_parms,g_error,%VAL(CNF_PVAL(d_xptr)),
     :                  %VAL(CNF_PVAL(d_vsptr)),1,wavdim,deccntr,
     :                  1,1,1,odensc,mstore,iy,0,aic,status)
          call opt_release(status)
          if(status.ne.SAI__OK) return
          plot = par_quest('Replot data',.false.)
          masked = .false.
          fit = .true.
          do j = 1 ,spdim2
            do i = 1, spdim1
              if(ref(i,j).eq.1) then
                call check_masking(1,i,i,j,j,masked,
     :                             %VAL(CNF_PVAL(d_mptr)))
                call fibchkfit(%VAL(CNF_PVAL(staptr)),deccntr,i,j,fit)
              end if
            end do
          end do
*
*  Workaround for bug in Digital Fortran Compiler v4.1.
*  Evaluate logical separately.
*
          trial = fit.and.(.not.masked)
          if(par_quest('Store fit results',trial)) then
*
*  This is the original test.
*  Restore when Digital Fortran Compiler v4.1 is fixed.
*
*         if(par_quest('Store fit results',(fit.and.(.not.masked))))
*    :             then
            call fibstres(g_parms,g_error,nparms,ref,
     :                    %VAL(CNF_PVAL(d_rptr)),ix,iy,deccntr,npts,
     :                    %VAL(CNF_PVAL(d_mptr)),nfailed,nnew,aic,
     :                    %VAL(CNF_PVAL(d_vptr)),%VAL(CNF_PVAL(staptr)))
            stored = .true.
          end if
        else if(key.eq.2) then

*     Hardcopy plot (need workspace here separate from vsptr)

          call dsa_get_work_array(wavdim,'float',w1ptr,slot,status)
          if(status.ne.SAI__OK) return
          call fibprofplt(%VAL(CNF_PVAL(w1ptr)),ix,iy,
     :                    %VAL(CNF_PVAL(d_rptr)),
     :                    %VAL(CNF_PVAL(d_sptr)))
          call dsa_free_workspace(slot,status)

        else if(key.eq.3) then

*     Exit

          loop = .false.
        end if
      end do
      if(stored) then
        if(par_quest('Increase iteration?',.true.)) then
          iteration = iteration + 1
          call accres(' ','more.twodspec.iteration','ws',1,iteration,
     :            ' ',status)
          call updtmsk(%VAL(CNF_PVAL(staptr)),%VAL(CNF_PVAL(d_mptr)))
        end if
      end if
      end
