      subroutine getvm(nbytes,pointer,slot,status)
*+
* Name:
*    GETVM

* Invocation:
*    CALL GETVM(NBYTES,POINTER,SLOT,STATUS)

* Purpose:
*   To get virtual memory

* Description:
*   To get virtual memory, outputting an error message if this fails.

* Arguments:
*      NBYTES = INTEGER (Given)
*        Number of bytes required
*      POINTER = INTEGER (Returned)
*        Pointer to VM in array dynamic_mem
*      STATUS = LOGICAL (Returned)
*        0 if ok

* History:
*       T.N.Wilkins Manchester 5/8/88
*       Revised TNW 18/8/88 to return pointer to dynamic_mem array.
*       TNW 14/10/88 Changed to a subroutine, name changed from getvm1
*-
      implicit none
      integer nbytes,pointer,status
      integer dyn_element,slot
      call dsa_get_workspace(nbytes,pointer,slot,status)
      pointer = dyn_element(pointer)
      end
