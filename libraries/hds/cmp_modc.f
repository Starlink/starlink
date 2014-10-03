      subroutine cmp_modc(struct, comp, len, ndim, dims, status)
*+
*  Name:
*     CMP_MODC

*  Purpose:
*     Create new string object, or alter existing one.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_MODC(LOC, NAME, LEN, NDIM, DIMS, STATUS)

*  Description:
*     A structure component of specified string precision and dimensions
*     is procured.   If no such component exists, then one is created.
*     If an unsuitable object exists, it is altered or replaced.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the name of the component to be
*        created in the structure.
*     LEN=INTEGER
*        Expression specifying the number of characters per value.
*     NDIM=INTEGER
*        Expression specifying the number of object dimensions.
*     DIMS(NDIM)=INTEGER
*        Array containing the object dimensions.
*     STATUS=INTEGER
*        Variable holding the status value. If this variable is not
*        SAI__OK on input, the routine will return without action.
*        If the routine fails to complete, this variable will be
*        set to an appropriate error number.

*  Algorithm:
*     See if it exists.   If it does not exist, create it.
*     If it does exist, see that it matches the required
*     type and dimensions.
*     If it exists, but does not match, delete it and create
*     a new component.

*  Implementation Deficiencies:
*     It does not attempt to use DAT_ALTER if the mis-match is only
*     in the dimensions, rather than the dimensionality or type.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     11-MAR-1987: Remove calls to ERR_ANNUL
*        correct call dat_erase   (RAL::AJC)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_CONST'
      INCLUDE 'CMP_ERR'
      INCLUDE 'DAT_ERR'

*  Arguments Given:
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
      integer len			! String precision
      integer ndim			! Number of dimensions
      integer dims(*)			! Object dimensions
*    Status return :
      integer status			! Status Return

*  External References:
      logical chr_simlr			! Caseless string equality

*  Local Variables:
      character*(DAT__SZLOC) loc	! Component locator
      character*(DAT__SZTYP) atype	! Actual type
      integer alen			! Actual string precision
      integer nadim			! Actual number of dimensions
      integer adims(DAT__MXDIM)		! Actual dimensions
      integer i				! Loop index
      logical there                     ! If component there

*.


      if (status .ne. SAI__OK) return

      call dat_there(struct, comp, there, status)
      if (status .ne. SAI__OK) then
         call cmp_erdsn(struct, comp, status)
      elseif (.not.there) then
         call dat_newc(struct, comp, len, ndim, dims, status)
      else
         call dat_find(struct, comp, loc, status)
         call dat_type(loc, atype, status)
         call dat_shape(loc, DAT__MXDIM, adims, nadim, status)
         if (status .ne. SAI__OK) then
            call cmp_erdsn(struct, comp, status)
            call dat_annul(loc,status)
         else
            if (.not. chr_simlr(atype(1:5), '_CHAR')) then
               status = CMP__TYPIN
            elseif (nadim .ne. ndim) then
               status = CMP__DIMIN
            else
               call dat_len(loc, alen, status)
               call dat_annul(loc,status)
               if (status .eq. SAI__OK) then
                  if (alen .lt. len) then
                     status = CMP__DIMIN
                  else
                     do i = 1, ndim
                        if (adims(i) .ne. dims(i)) then
                           status = CMP__DIMIN
                        endif
                     enddo
                  endif
               endif
            endif
            if (status .ne. SAI__OK) then
               status = SAI__OK
               call dat_erase(struct, comp, status)
               if (status .ne. SAI__OK) then
                  call cmp_erdsn(struct, comp, status)
               else
                  call dat_newc(struct, comp, len, ndim, dims,
     :              status)
                  if (status .ne. SAI__OK) then
                     call cmp_erdsn(struct, comp, status)
                  endif
               endif
            endif
         endif
      endif

      end

