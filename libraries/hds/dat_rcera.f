*+ DAT_RCERA - recursive erase
      subroutine dat_rcera(loc, cname, status)
*    Description :
*     This routine is obsolete. Its function is now performed by
*     DAT_ERASE.
*     The routine recursively deletes the component of the data
*     structure pointed to by the input locator.
*    Invocation :
*     CALL DAT_RCERA(LOC, CNAME; STATUS)
*    Parameters :
*     LOC=CHARACTER*(DAT__SZLOC)
*           Variable containing a locator associated with an object.
*     CNAME=CHARACTER*(DAT__SZNAM)
*           Variable containing the name of the component to be deleted.
*     STATUS=INTEGER
*           Variable holding the status value. If this variable is not
*           SAI__OK on input, the routine will return without action.
*           If the routine fails to complete, this variable will be
*           set to an appropriate error number.
*    Method :
*     Call DAT_ERASE modified to do recursive erase.
*    Authors :
*     Sid Wright  (UCL::SLW)
*     Mike Lawden (RAL::MDL)
*     A. Chipperfield (RAL::AJC)
*    History :
*     29-Mar-1983:  Original (UCL::SLW)
*     27-Sep-1983:  Handle structure arrays (RAL::MDL)
*     16-Oct-1984:  Delete emptied structure (RAL::AJC)
*     23-Oct-1984:  Handle structure arrays (RAL::MDL)
*     09.04.1985:  Remove calls to ERR_   (REVAD::BDK)
*     25.02.1987:  Correct bug for 1 element structure arrays (RLVAD::AJC)
*     18.03.1987:  Straight through call to DAT_ERASE for HDS_C_3_4 (RLVAD::AJC)
*     15.04.1987:  Improved prologue layout (RAL::AJC)
*     03.06.1987:  Rename ELOC to LOC for consistency  (RAL::AJC)
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*    Import :
      character*(*) loc  		! Object Locator
      character*(*) cname		! component to be deleted
*    Export :
*    Status return :
      integer status			! Status Return
*    Local variables :
*-

      if (status .ne. SAI__OK) return

      call dat_erase(loc, cname, status)

      end
