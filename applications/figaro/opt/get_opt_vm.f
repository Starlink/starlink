      subroutine get_opt_vm(bytes,ptr,status)
*+
* Name:
*    GET_OPT_VM

* Invocation:
*    CALL GET_OPT_VM(BYTES,PTR,STATUS)

* Purpose:
*  Get virtual memory for optimisation.

* Description:
*  Get virtual memory for common arrays in optimisation routines, and
*  extra as requested.
*
* Arguments:
*     BYTES = INTEGER (Given)
*        Bytes for pointer PTR
*     STATUS = INTEGER (Given and returned)
*        Error status, 0=ok
*     PTR = INTEGER (Returned)
*        Extra workspace (BYTES)
* Global variables:
*     MPTS = INTEGER (Given)
*        Number of elements in each array (include file opt_cmn)
*     DENSPTR = INTEGER (Returned)
*        Pointer to Y array (include file opt_cmn)
*     DATAPTR = INTEGER (Returned)
*         etc. (include file opt_cmn)
*     WEIGHTPTR = INTEGER (Returned)
*        (include file opt_cmn)
*     OPT_SLOT = INTEGER (Returned)
*        Slot used for memory (include file opt_cmn)

* Subroutines called:
*     GETWORK    : Get virtual memory
*     DSA_GET_LU : Get logical unit number

* Authors:
*   TNW: T.N.Wilkins Manchester until 1/89, then Cambridge

* History:
*   T.N.Wilkins 23/11/88
*   TNW 16/12/88 Changed to also call dsa_get_lu
*   TNW 9/8/89 Changed to also get memory into adaptr & adeptr
*    "  9/10/90 Allow extra workspace to be obtained at the same time.
*    "  21/10/91 Changes for scaling "in situ"
*    "  21/8/92 MPTS from opt_cmn used instead of passing m.
*   MJC 2011 June 30 Use DYN_INCAD for offset addressing.
*   MJC 2011 July 11 Replace GETVM call with DSA_GET_WORKSPACE as
*       the former uses the DYN_ELEMENT that causes DYN_INCAD to
*       create offsets not pointers.
*-
      implicit none
      include 'opt_cmn'
      include 'SAE_PAR'
      include 'PRM_PAR'

      integer status,bytes,ptr,total
      logical isnew

*  Get virtual memory.
      total = mpts*3*VAL__NBD+bytes
      call dsa_get_workspace(total,dataptr,opt_slot,status)

*  Indicate that we have VM.
      got_opt_vm = status.eq.SAI__OK

*  Divide the workspace into three double-precision arrays of M
*  elements, plus whatever else is asked for.
      call dyn_incad(dataptr,'double',mpts,densptr,isnew,status)
      call dyn_incad(densptr,'double',mpts,weightptr,isnew,status)
      if (bytes.gt.0)
     :  call dyn_incad(weightptr,'double',mpts,ptr,isnew,status)

*  Get the logical unit for .itt file.
      call dsa_get_lu(opt_lu,status)

      end
