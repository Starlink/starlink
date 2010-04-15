*+ CMP_STRUC - Enquire if component is structure
      subroutine cmp_struc(struct, comp, struc, status)
*    Description :
*     An enquiry is made as to whether or not the structure component
*     is a structure object.
*    Invocation :
*     CALL CMP_STRUC(LOC, NAME; STRUC, STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with a structured
*           data object.
*     NAME=CHARACTER*(*)
*           Expression specifying the component name of a primitive
*           object contained in the structure.
*     STRUC=LOGICAL
*           Variable to receive whether the object is a structure.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Get object locator using DAT_FIND and
*     DAT_STRUC.
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
      logical struc			! Whether component is structure
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
            call dat_struc(loc, struc, status)
            if (status .ne. SAI__OK) then
               call cmp_erdsn(struct, comp, status)
            endif
            call dat_annul(loc, status)
         endif
      endif

      end


