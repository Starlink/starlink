      subroutine getwork(nelm,type,pointer,slot,status)
*+
* Name:
*    GETWORK

* Invocation:
*    CALL GETWORK(NELM,TYPE,POINTER,SLOT,STATUS)
*
* Description:
*   To get virtual memory, outputting an error message if this fails.
* Purpose:
*   To get virtual memory, outputting an error message if this fails.
* Arguments:
*    NELM = INTEGER (Given)
*        Number of elements required
*    TYPE = CHARACTER*(*) (Given)
*        Type of data, eg. 'float'
*    POINTER = INTEGER (Returned)
*        Pointer to VM in array dynamic_mem
*    STATUS = LOGICAL (Returned)
*        0 if ok
* History:
*       T.N.Wilkins Manchester 5/8/88
*       Revised TNW 18/8/88 to return pointer to dynamic_mem array.
*       TNW 14/10/88 Changed to a subroutine, name changed from getvm1
*       TNW 28/11/88 Version to call dsa_get_work_array.
*-
      implicit none
      integer nelm,pointer,status
      character*(*) type
      integer dyn_element,slot
      call dsa_get_work_array(nelm,type,pointer,slot,status)
      pointer = dyn_element(pointer)
      end
