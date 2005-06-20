      subroutine auto(status)
*+
* Name:
*    AUTO

* Invocation:
*    CALL AUTO(STATUS)
*
* Purpose:
*  Determine locations of comb spectra over frame.

* Description:
*   Subroutine to calculate automatically (except for initial setting of
*  windows) centres of data on continua lines and to display the results
*  on a graphics device.

* Arguments:
*    STATUS = INTEGER (Given and returned)
*        Error status
* History:
*  Altered TNW 11/11/88 to no longer erase graphics screen at end of
*  routine-unlikely to be run interactively anyway.
*  Now uses DSA_AXIS_RANGE TNW 25/1/91
*  Workspace changes, TNW 11/2/91
*
      implicit none
      include 'SAE_PAR'
      include 'CNF_PAR'          ! For CNF_PVAL function
      include 'arc_dims'
*- ---------------------------------------------------------------------
      integer status
*  Local
*
      integer nwindow
      integer xstart,xend
      integer nbls,ptr1,ptr2,slot,slot2
      real value,value1
      include 'PRM_PAR'
*
*
      call par_rdval('xblock',1.0,real(wavdim),20.0,'Channels',value)

      nwindow = nint(value)
      nbls = wavdim/nwindow

      call dsa_axis_range('data',1,' ',.false.,value,value1,xstart,xend,
     :                    status)

      call dsa_get_work_array(line_count,'float',ptr1,slot,status)
      call dsa_get_work_array(line_count,'float',ptr2,slot2,status)
      if(status.ne.SAI__OK) return

      call comb_window(%VAL(CNF_PVAL(d_xptr)),%VAL(CNF_PVAL(d_vsptr)),
     :                 nbls,nwindow,%VAL(CNF_PVAL(d_tlptr)),
     :                 %VAL(CNF_PVAL(d_trptr)),xstart,xend,
     :                 %VAL(CNF_PVAL(ptr1)),%VAL(CNF_PVAL(ptr2)))

      call dsa_free_workspace(slot2,status)
      call dsa_free_workspace(slot,status)

      end
