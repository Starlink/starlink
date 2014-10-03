      subroutine cmp_unmap(struct, comp, status)
*+
*  Name:
*     CMP_UNMAP

*  Purpose:
*     Unmap structure component.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL CMP_UNMAP(LOC, NAME, STATUS)

*  Description:
*     Unmap a structure component, previously mapped with CMP_MAPV
*     or CMP_MAPN.

*  Arguments:
*     LOC=CHARACTER*(DAT__SZLOC)
*        Variable containing a locator associated with a structured
*        data object.
*     NAME=CHARACTER*(*)
*        Expression specifying the component name of a primitive
*        object contained in the structure.
*     STATUS=INTEGER
*        Variable holding the status value.
*        The routine will attempt to execute regardless of the input
*        value of this variable.
*        If its input value is not SAI__OK then it is left unchanged
*        by this routine, even if it fails to complete.
*        If its input value is SAI__OK and this routine fails, then
*        the value is changed to an appropriate error number.

*  Algorithm:
*     Find index in Component Table.   Unmap the object.
*     Annul its Locator and table entry.

*  Authors:
*     Jack Giddings (UCL::JRG)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983:  Original.  (UCL::JRG)
*     15-APR-1987:  Improved prologue layout (RAL::AJC)
*     5-JUN-1991 (RFWS):
*        Fixed bug causing OK status to be returned if status was bad
*        on entry and the specified component was not mapped.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'CMP_CONST'
      INCLUDE 'CMP_ERR'

*  Arguments Given:
      character*(*) struct		! Structure Locator
      character*(*) comp		! Component Name
*    Status return :
      integer status			! Status Return

*  External References:
      logical chr_simlr			! Caseless string equality

*  Global Variables:
      INCLUDE 'CMP_CCT'

*  Local Variables:
      integer index			! Table index
      integer istat			! Local status

*.


      istat = status
      status = SAI__OK

*    Initialised ?
      if (Cmpslp) then
         call cmp_activ(status)
         if (status .ne. SAI__OK) then
            return
         endif
      endif

*    Find index in Component Table
      index = 1
      dowhile (index .le. Cmpcnt)
         if (.not. Cmpfre(index)) then
            if (struct .eq. Cmpstr(index)) then
               if (chr_simlr(comp, Cmpnam(index))) then
                  goto 1
               endif
            endif
         endif
         index = index + 1
      enddo
 1    continue

*    Check that it was found
      if (index .gt. Cmpcnt) then
         if (istat .ne. SAI__OK) then
            STATUS = ISTAT
            return
         else
            status = CMP__NOMAP
            call cmp_erdsn(struct, comp, status)
            return
         endif
      endif

*    Unmap object
      call dat_unmap(Cmploc(index), status)
      if (status .ne. SAI__OK) then
         call cmp_erdsn(struct, comp, status)
      endif

*    Annul locator
      call dat_annul(Cmploc(index), status)

*    Remove Table entry
      Cmpfre(index) = .true.

      if (istat .ne. SAI__OK) then
         status = istat
      endif

      end


