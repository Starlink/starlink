
*+  CHI_APUSHC - Push character string to string stack
      subroutine chi_apushc (string, stack, pointer, stacksize,
     :                              errstatus, status)
*    Description :
*     Push a character string onto the named stack.   The stack
*     pointer is first checked then incremented.
*    Parameters :
*     STRING = CHAR(INPUT)
*            String to be put on the stack
*     STACK = CHAR*(STACKSIZE)(INPUT)
*            Name of the stack
*     POINTER = INTEGER(UPDATE)
*            Current top of stack
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
      character*(*) string
      character*(*) stack
      integer pointer
      integer stacksize
      integer errstatus
*    Status
      integer status
*    External references
      integer chr_len
*    Local variables :
      integer str_size          ! size of input string
      integer new_pointer          ! updated value of pointer
*-
      if (status.eq.SAI__OK) then
         str_size = chr_len(string)
         new_pointer= pointer + str_size + 1
         if (new_pointer .gt. stacksize) then
            status= errstatus
         else
            stack(pointer+1:)= string(:str_size)//'!'
            pointer= new_pointer
         endif
      endif
*
      return
      end
