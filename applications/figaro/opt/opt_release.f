      subroutine opt_release(status)
*+
* Name:
*    OPT_RELEASE

* Invocation:
*    CALL OPT_RELEASE(STATUS)

* Purpose:
*  To release workspace/logical unit used during optimisation.

* Description:
*  Flags which are set on obtaining vm are checked, and any VM obtained
*  is released. If an iteration file is open it is closed.

* Arguments:
*      STATUS = INTEGER (Given and returned)
*        Error status, 0=ok

* Authors:
*   TNW: T.N.Wilkins, Cambridge

* History:
*   TNW 19-MAR-1990, original version
*-
      implicit none
      integer status
      include 'opt_cmn'
      logical ifopen

*

      if(got_opt_vm) then
        call dsa_free_workspace(opt_slot,status)
        if(got_opt_vm2) then
          call dsa_free_workspace(opt_slot2,status)
        end if

*   This is to delete the iteration file if the fitting crashes, and
*   the iteration file is not wanted. DSA_FREE_LU will close the file
*   if it is still open, so we needn't bother about the case where you
*   do want to keep the file.

        inquire(unit=opt_lu,opened=ifopen)
        if(ifopen.and.(.not.keep_itt)) then
          close(opt_lu,status='delete')
        end if
        call dsa_free_lu(opt_lu,status)
      end if
      got_opt_vm = .false.
      end
