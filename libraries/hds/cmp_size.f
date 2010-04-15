*+ CMP_SIZE - Component Size enquiry
      subroutine cmp_size(struct, comp, size, status)
*    Description :
*     A size enquiry is made for a structure component.
*    Invocation :
*     CALL CMP_SIZE(LOC, NAME; SIZE, STATUS)
*    Parameters :
*     LOC=CHARACTER*(CMP__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     SIZE=INTEGER
*           Variable to receive the total number values in the object.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Get object locator using DAT_FIND and then get size from
*     DAT_SIZE.
*    Authors :
*     Jack Giddings (UCL::JRG)
*    History :
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
*    Export :
      integer size			! Size of component
*    Status return :
      integer status			! Status Return
*    Local variables :
      character*(DAT__SZLOC) loc	! Component locator
*-

      if (status .eq. SAI__OK) then
         call dat_find(struct, comp, loc, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
         else
            call dat_size(loc, size, status)
            if (status .ne. SAI__OK) then
               call cmp_erdsn(struct, comp, status)
            endif
            call dat_annul(loc, status)
         endif
      endif

      end


