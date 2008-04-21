      subroutine plot_cube(vcorr,vtype,velplt)
*+
* Name:
*    PLOT_CUBE

* Invocation:
*    CALL PLOT_CUBE(VCORR,VTYPE,VELPLT)

* Purpose:
*    To extract and plot results from the data cube.

* Description:
*    To extract and plot results from the data cube.

* Parameters:-
*     LINE_NAME(LINE_COUNT) = CHARACTER*10 ARRAY (Given)
*        Line names
*     VCORR = REAL (Given)
*        Correction to apply to velocities of lines.
*     VTYPE = INTEGER (Given)
*        Which velocity correction is used
*     VELPLT = LOGICAL (Given)
*        If to use velocity scale for X axis
* Global variables:
*     IDSPTR,IDSEND = INTEGER (Given)
*        "Pointers" to names of lines (include file arc_dims)
*     D_RPTR = INTEGER (Given)
*        Pointer to results cube (include file arc_dims)
*     D_VPTR = INTEGER (Given)
*        Pointer to variance on results cube (include file arc_dims)
*     STAPTR = INTEGER (Given)
*        Pointer to fit status (include file arc_dims)
*     LINE_COUNT = INTEGER (Given)
*        Number of lines (include file arc_dims)
*     D_VSPTR = INTEGER (Given)
*        Pointer to extracted spectrum (include file arc_dims)
*     D_TLPTR = INTEGER (Given)
*        Pointer to left tram lines (include file arc_dims)
*     D_TRPTR = INTEGER (Given)
*        Pointer to right tram lines (include file arc_dims)
*     D_XPTR = INTEGER (Given)
*        Pointer to X array data (include file arc_dims)
*     D_WPTR = INTEGER (Given)
*        Pointer to wavelengths of lines (include file arc_dims)
*     XUNITS = CHARACTER*(*) (Given)
*        X units for plot (include file arc_dims)
*     TITLE = CHARACTER*(*) (Given)
*        Title for plot (include file arc_dims)
*     LEGEND(2) = CHARACTER*(*) ARRAY (Given)
*        Legends for plot (include file arc_dims)
*     NXSECTS = INTEGER (Given)
*        Number of cross-sections in data (include file arc_dims)
*     NCHANNELS = INTEGER (Given)
*        Number of channels in data (include file arc_dims)
*     D_SPTR = INTEGER (Given)
*        Pointer to main file data array (include file arc_dims)
*
* History:
*  Change to call of line_vplot TNW 6/7/88
*  Changed to use GET_PREVFIT TNW 24/11/88
*  Use of GETWORK TNW 28/11/88
*  Use of GET_PARNUM TNW 18/7/89
*  Less prompting in this routine 3/1/91 TNW
*  Changes for new results structure, etc. TNW 7/91
*  QMENU used TNW 15/8/91
*  Made to work for 3-d data, LINE_NAME removed from call TNW 11/6/92
*-
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      integer status
      include 'arc_dims'
      real vcorr
      logical velplt
* ----------------------------------------------------------------------
      include 'status_inc'
      integer line,vtype
      integer ix,iy
      logical loop
      logical par_qnum,par_quest
      integer chr_len,iplt
      logical qstat
      real value
      integer dumi
      real dumr
      character dumc
      character*23 dict(3)
      data dict/
     :     'HARD : Plot in hardcopy',
     :     'SOFT : Plot in softcopy',
     :     'DON''T : Don''t plot'/

      loop=.true.
      line=1
      ix = 1
      iy = 1
      do while(loop)
        qstat=par_qnum('Enter line number',1.0,real(line_count),
     :     real(line),.true.,' ',value)
        line=nint(value)
        qstat=par_qnum('Enter X position',1.0,real(spdim1),real(ix)
     :       ,.true.,' ',value)
        ix=nint(value)
        if(spdim2.gt.1) then
          qstat = par_qnum('Enter Y position',1.0,real(spdim2),real(iy)
     :       ,.true.,' ',value)
          ix=nint(value)
        end if

        title='File '//datafile(:chr_len(datafile))

* Output fit results

        call output_fit(%VAL(CNF_PVAL(staptr)),%VAL(CNF_PVAL(d_rptr)),
     :                  %VAL(CNF_PVAL(d_vptr)),ix,iy,line,deccntr,-1,
*     :       .true.,dynamic_chars(idsptr:idsend),
     :                  .true.,idstring,%VAL(CNF_PVAL(d_wptr)),.false.)
        if((deccntr(FIT_STAT).eq.1).or.(deccntr(FIT_STAT).eq.2)) then
          iplt = 3
          if(deccntr(FIT_NCMP).ge.0) then
            status = SAI__OK
            call qmenu('Plot Menu',dict,3,1,dumr,dumc,iplt,dumi,status)
          end if

          if(iplt.ne.3) then
            call plot_line(line,(iplt.eq.2),vcorr,
     :                     %VAL(CNF_PVAL(d_wptr)),
*     :           ix,iy,velplt,vtype,dynamic_chars(idsptr:idsend),
     :                     ix,iy,velplt,vtype,idstring,
     :                     %VAL(CNF_PVAL(d_rptr)),
     :                     %VAL(CNF_PVAL(d_vptr)),status)

*       plot fit

          end if

*     fit a success or nag error

        else
          call par_wruser('No fit present',status)

*   fit present

        end if
        loop=par_quest('Another plot?',.true.)
      end do
      end
