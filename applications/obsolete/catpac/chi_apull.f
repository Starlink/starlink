*+  CHI_APULL - Pull integer item from stack
      subroutine chi_apull (value, stack, pointer, errstatus, status)
*    Description :
*     Pull an integer item from the named stack.  The stack pointer
*     is first checked, but not subsequently decremented.
*    Parameters :
*     VALUE = INTEGER(OUTPUT)
*            Value to be pulled from the stack
*     STACK = INTEGER(*)(INPUT)
*            Name of the stack
*     POINTER = INTEGER(INPUT)
*            Value of stack pointer
*     ERRSTATUS = INTEGER(INPUT)
*            Return status value on stack underflow
*     STATUS = INTEGER(UPDATE)
*            Status value
*    Type declarations :
      implicit none
*    Global constants :
      include 'sae_par'
*    Import
      integer pointer
      integer stack(*)
      integer errstatus
*    Export
      integer value
*    Status
      integer status
*-
      if (status .eq. SAI__OK) then
         if (pointer.le.0) then
            status= errstatus
         else
            value= stack(pointer)
         endif
      endif
*
      return
      end
