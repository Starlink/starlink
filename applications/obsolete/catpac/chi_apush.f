*+  CHI_APUSH - Push integer item onto stack
      subroutine chi_apush (value, stack, pointer, stacksize,
     :                              errstatus, status)
*    Description :
*     Push an integer item onto the named stack.   The stack
*     pointer is first checked then incremented.
*    Parameters :
*     VALUE = INTEGER(INPUT)
*            Value to be put on the stack
*     STACK = INTEGER(*)(INPUT)
*            Name of the stack
*     POINTER = INTEGER(UPDATE)
*            Input value of stack pointer
*     STACKSIZE = INTEGER(INPUT)
*            Maximum permitted stack size
*     ERRSTATUS = INTEGER(INPUT)
*            Return status value on stack overflow
*     STATUS = INTEGER(UPDATE)
*            Status value
*    Type declarations :
      implicit none
*    Global constants :
      include 'sae_par'
*    Import
      integer pointer
      integer stacksize
      integer stack(*)
      integer value
      integer errstatus
*    Status
      integer status
*-
      if (status.eq.SAI__OK) then
         if (pointer .ge. stacksize) then
            status= errstatus
         else
            pointer= pointer+1
            stack(pointer)= value
         endif
      endif
*
      return
      end
