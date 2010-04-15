      subroutine new_comb(left,right)
*+
* Name:
*    NEW_COMB

* Invocation:
*    CALL NEW_COMB(LEFT,RIGHT)

* Purpose:
*   Set up trams for COMB

* Description:
*   Set up trams for COMB

* Subroutines referenced:
*      COMB_SPACING       : Find positions of comb "teeth"
*      PLOT_SPECT         : Produce x-y plot, using histogram-type lines
*      GR_SOFT            : Open/select softcopy graphics device
*      GR_HARD            : Open/select hardcopy graphics device
*      GR_CLEAR           : Clear graphics screen/advance one frame
*      ZERO_REAL          : Zero real array
*      DSA_FREE_WORKSPACE : Free workspace
*      DSA_GET_WORK_ARRAY : Get work array
*      FIG_YTRACT         : Take cut through data parallel to 2nd axis
*      PAR_QNUM           : Obtain number from user
*      PAR_RDVAL          : Obtain numeric parameter from user
*      PAR_WRUSER         : Write character string to user
*
* Arguments:
*    LEFT(NYP) = REAL ARRAY (Returned)
*
*    RIGHT(NYP) = REAL ARRAY (Returned)
*
* History:
*   Workspace for COMB_SPACING now real, TNW 16/6/92
*
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
*
*  Import
*
      include 'arc_dims'
*-
      real left(nyp),right(nyp)
      integer status
*  Local
*
      integer NAVE

* averageing width in channels

      parameter (NAVE = 20)
      logical par_quest,qstat,par_qnum

      integer ptr1,slot,k
      real level
      character*29 chars
      integer halfnchan(2)

      status = SAI__OK
      call dsa_get_work_array(nyp,'float',ptr1,slot,status)
      if(status.ne.SAI__OK) return
*
*  Loop over crossections
*
      halfnchan(2)=(wavdim+NAVE)/2
      halfnchan(1)=(wavdim-NAVE)/2
      call fig_ytract(%VAL(CNF_PVAL(d_sptr)),wavdim,spdim1,halfnchan(1),
     :                halfnchan(2),%VAL(CNF_PVAL(d_vsptr)))
      if(batch) then
        call gr_hard(status)
      else
        call gr_soft(status)
      end if
*
*  Display cut
*
      call plot_spect(spdim1,%VAL(CNF_PVAL(d_xptr)),
     :                %VAL(CNF_PVAL(d_vsptr)),' ','X-sects',' ')
*
* Calculate how many tram lines are possible in the spectrum.
*
      call par_rdval('level',0.0,1.0,0.3,'Fraction of height of tooth'
     :      ,level)
   1  continue
      call zero_real(left,nyp)
      call zero_real(right,nyp)
      call comb_spacing(spdim1,%VAL(CNF_PVAL(d_xptr)),
     :                  %VAL(CNF_PVAL(d_vsptr)),level,left,
     :                  %VAL(CNF_PVAL(ptr1)),right,line_count,nyp)
      call par_wruser('Boundaries of windows : -',status)
      call par_wruser('  Left          Right',status)
      do k=1,line_count
        write(chars,'(2(2x,f12.5))')left(k),right(k)
        call par_wruser(chars,status)
      end do
      if(.not.par_quest('OK?',.true.)) then
        call pgpage
        call plot_spect(spdim1,%VAL(CNF_PVAL(d_xptr)),
     :        %VAL(CNF_PVAL(d_vsptr)),' ','X-sects',' ')
        qstat = par_qnum('Level?',0.0,1.0,0.3,.true.,' ',level)
        goto 1
      end if
      call dsa_free_workspace(slot,status)
      end
